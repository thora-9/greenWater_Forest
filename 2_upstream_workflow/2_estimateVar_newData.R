#########################################################
##Estimate indices for each basin (HydroATLAS/BASIN)
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Specify the continent
cur_continent = 'Australasia'
cur_continent_abr = 'AU'
cur_continent_abr_UP = 'au'
cur_run = 'upstream' #unconnected #upstream
##############################
#HydroATLAS/BASIN data 

#Load the raw data
data_in_atlas =
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

#Create a non-spatial DT copy to improve computation efficiency
data_in_atlas_DT = 
  data_in_atlas %>% 
  st_drop_geometry() %>%
  as.data.table()

#Remove the spatial copy (continent level file loaded below)
rm(data_in_atlas)


##############################
#Continent-level

path2WS = paste0(database, 'Watersheds/HydroBASIN/hybas_', cur_continent_abr, '_lev01-12_v1c/')

data_in = 
  st_read(paste0(path2WS, 'hybas_', cur_continent_abr, '_lev12_v1c.shp')) %>%
  st_make_valid()

#Create a non-spatial DT copy to improve computation efficiency
data_in_asDT = 
  data_in %>% 
  st_drop_geometry() %>%
  as.data.table()

#Remove variables improve computation efficiency
data_sub_DT = 
  data_in_asDT[, 1:4]

#Get just the HYBAS_ID to reduce memory/computation load and then get the centroid
data_sub = 
  data_in %>%
  dplyr::select(1:4) 

#To free memory
rm(data_in)

#Load the data to be estimated
##############################
in_dir = paste0(database, "LULC/ESA_CCI_LC_2025/1_processed/")
esa_tree_cover = fread(paste0(in_dir, "ESA_tree_cover_by_year_01.csv"))

# Identify columns by type
percent_cols <- grep("^percent", names(esa_tree_cover), value = TRUE)
count_cols    <- grep("^tree", names(esa_tree_cover), value = TRUE)

esa_tree_percent_rast = 
  terra::rast(cbind(esa_tree_cover[, .(lon, lat)], esa_tree_cover[, ..percent_cols]), 
              type = "xyz", crs = "epsg:4326")

esa_tree_count_rast = 
  terra::rast(cbind(esa_tree_cover[, .(lon, lat)], esa_tree_cover[, ..count_cols]), 
              type = "xyz", crs = "epsg:4326")


##############################
#Create a spatvector version of the vector HYBAS file  
data_sub_sv = 
  data_sub %>% 
  dplyr::select(HYBAS_ID) %>%
  as_spatvector()

#Variables summarized by mean
#Create a raster stack with all the indices
data_in_mean = 
  esa_tree_percent_rast

#Variables summarized by mode (categorical var)
#data_in_mode = 
#  c(fm_aggregated)

#Variables summarized by count
data_in_count = 
  esa_tree_count_rast

##############################
#Extract the values 
var_HYBAS_mean = 
  terra::extract(data_in_mean, data_sub_sv, fun = 'median', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

# var_HYBAS_mode = 
#   raster::extract(data_in_mode, data_sub_sv, fun = 'modal', na.rm=TRUE, bind = TRUE) %>%
#   as.data.table()

var_HYBAS_count = 
  terra::extract(data_in_count, data_sub_sv, fun = 'sum', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

var_HYBAS_all = 
  list(var_HYBAS_mean, 
    #var_HYBAS_mode, 
    var_HYBAS_count) %>%
  reduce(full_join, by='HYBAS_ID')

##########################################################################################
#Load the upstream-HYBAS/unconnected linkage
#This was calculated in step 1 of the workflow

if(cur_run == 'unconnected'){
  path2linkage = paste0(proj_dir, 'Unconnected/', cur_continent, '/')
  linkage_HYBAS = 
    readRDS(paste0(path2linkage, 'unconnected_codes_',cur_continent_abr, '.rds')) %>%
    tidytable::rename(chain = 2)
  
} else{
  path2linkage = paste0(proj_dir, 'Upstream/', cur_continent, '/')
  linkage_HYBAS = 
    readRDS(paste0(path2linkage, 'upstream_codes_',cur_continent_abr_UP, '_wdis.rds')) %>%
    tidytable::rename(chain = 2)
}

##############################
#Estimate the index for upstream regions

#Split the df by HYBAS_ID
upstream_HYBAS_list =
  split(data_sub_DT, by = 'HYBAS_ID') 

#Test - list number 27
dist_threshold_km = 100 #Use 99999 for essentially no dist threshold


#Function to get the centroid distances
summarize_upstream <- function(df) {
  
  #Subset the upstream linkage to the given watershed
  cur_upstream_HYBAS = 
    linkage_HYBAS[HYBAS_ID == df$HYBAS_ID]
  
  if(cur_run != 'unconnected') {
    cur_upstream_HYBAS = 
      cur_upstream_HYBAS %>%
      tidytable::filter(distance < dist_threshold_km)
  }
  
  #Summarise variables 
  # cur_var_upstream = 
  #   var_HYBAS_all[HYBAS_ID %in% cur_upstream_HYBAS$chain] %>%
  #   tidytable::summarise(across(FFI2000:Habitat_Extent, ~ mean(.x, na.rm = T)),
  #                        across(FM_class_mode, ~ modal(.x, na.rm = T)),
  #                        across(natural_only:total_cells, ~ sum(.x, na.rm = T))) %>%
  #   tidytable::mutate(ws_total = length(unique(cur_upstream_HYBAS$chain)))

  cur_var_upstream = 
    var_HYBAS_all[HYBAS_ID %in% cur_upstream_HYBAS$chain] %>%
    tidytable::summarise(across(all_of(percent_cols), ~ mean(.x, na.rm = T)),
                         across(all_of(count_cols), ~ sum(.x, na.rm = T))) %>%
    tidytable::mutate(ws_total = length(unique(cur_upstream_HYBAS$chain)))
  
  #Store the output for the current watershed
  out_df = 
    data.table(HYBAS_ID = df$HYBAS_ID) %>%
    cbind(cur_var_upstream)
  
  out_df
  
  # #Add it to the main output
  # out_df = rbind(out_df, out_temp)
  # 
  # getdist
  
}

#Run the process in parallel
start = Sys.time()
out_list <- pblapply(upstream_HYBAS_list, summarize_upstream)
#out_list <- parallel::mclapply(upstream_HYBAS_list, summarize_upstream, mc.cores = 4)
end = Sys.time()

end - start

#Merge the lists
out_list_merged = rbindlist(out_list)


#Save the summarized output
out_loc = paste0(path2linkage, '250807/')
if (!dir.exists(out_loc)) {
  dir.create(out_loc, recursive = TRUE)
}
out_name = 'upstream_VAR_ESA_LC_'

if(cur_run == 'unconnected'){
  saveRDS(out_list_merged, paste0(path2linkage, '240821/','unconnected_VAR_', cur_continent_abr_UP,'.rds'))
  
} else {
  if(dist_threshold_km == 99999){
    saveRDS(out_list_merged, paste0(out_loc, out_name, cur_continent_abr_UP, '_nodist.rds'))
  } else if (dist_threshold_km == 100){
    saveRDS(out_list_merged, paste0(out_loc, out_name, cur_continent_abr_UP, '_w100km.rds'))
  }
}

###################################################
# TEST Output
###################################################
out_loc = paste0(path2linkage, '250807/')
path2linkage = paste0(proj_dir, 'Upstream/', cur_continent, '/')
test = readRDS(paste0(out_loc, 'upstream_VAR_', cur_continent_abr_UP, '_nodist.rds'))
test2 = merge(data_sub, test, by = 'HYBAS_ID')

sf::st_write(test2, paste0(out_loc, "AU_HYBAS_Upstream.gpkg"), delete_layer = TRUE)

