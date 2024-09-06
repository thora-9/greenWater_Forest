#########################################################
##Estimate indices for each basin (HydroATLAS/BASIN)
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Specify the continent
cur_continent = 'Asia'
cur_continent_abr = 'AS'
cur_continent_abr_UP = 'as'
cur_folder_name = '240821'
cur_run = 'upstream' #'unconnected'

##############################
#Fix the fishnet

# #Run the fix geometry algorith
# data_fixed_geom <- qgis_run_algorithm(
#   "native:fixgeometries",
#   INPUT = fishnet)
# 
# fishnet = st_read(qgis_extract_output(data_fixed_geom, "OUTPUT"))
# 
# st_write(fishnet, paste0("/Users/tejasvi/Dropbox/Database/Fishnet_halfdegree/global_fishnet_fixed.gpkg"))

##############################
#HydroATLAS/BASIN data 

data_in_atlas =
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

data_in_atlas_DT = 
  data_in_atlas %>% 
  st_drop_geometry() %>%
  as.data.table()

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


##############################
#Find the centroid of each HYBAS polygon
data_sub_centroid = 
  data_sub %>% 
  st_make_valid() %>%
  st_centroid(data_sub)

#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "join")
qgis_show_help("native:joinattributesbylocation")

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = data_sub_centroid,
    JOIN = fishnet[,1:2],
    PREDICATE = 5 #Use st_within
  )

data_merged_fishnet = st_read(qgis_extract_output(qgis_join, "OUTPUT"))

##########################################
######Load here after changing distance threshold######
dist_threshold = 'w100km' #nodist #w100km
##########################################
##########################################
#Load the summarized output

if(cur_run == 'unconnected'){
  path2linkage = paste0(proj_dir, 'Unconnected/', cur_continent,'/', cur_folder_name, '/')
  summarized_HYBAS = 
    readRDS(paste0(path2linkage, 'unconnected_VAR_',cur_continent_abr_UP, '.rds')) %>%
    mutate(across(natural_only:agroforestry, ~ .x/total_cells))
  
} else{
  path2linkage = paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name, '/')
  summarized_HYBAS = 
    readRDS(paste0(path2linkage, 'upstream_VAR_',cur_continent_abr_UP, '_', dist_threshold, '.rds')) %>%
    mutate(across(natural_only:agroforestry, ~ .x/total_cells))
}

##############################
#Create a list of variables to summarize
var_mean = c("FFI2000", "FFI2020", "BHI_2010", "BHI_2020", "ForestAge_TC000", 
             "ForestAge_TC020", "Combined_SR_2022", "Amphibians_SR_2022", "Birds_SR_2022",
             "Mammals_SR_2022", "Reptiles_SR_2022", "BII", "Tree_Species", "Forests", 
             "Artificial", "Savanna", "Shrubland", "Grassland","Wetland_Inland", "Rocky.Areas", "Desert",
             "Habitat_Extent")
var_mode = c("FM_class_mode")
var_count = c("natural_only", "natural_managed", "natural_all", "planted", "agroforestry")

#Aggregation method 1 - Take the median/mean of all watersheds within grid cell

#Subset the variables, merge the fishnet id, summarize after grouping by fishnet id
data_in_summarized_v1 = 
  summarized_HYBAS %>%
  #dplyr::select(1, all_of(all_variables)) %>%
  mutate(across(FFI2000:ws_total, ~ ifelse(.x == -999, NA, .x))) %>%
  merge(data_merged_fishnet[, c("HYBAS_ID", "Id")], by = 'HYBAS_ID') %>%
  #merge(upstream_HYBAS, by = "HYBAS_ID", all.x = T) %>%
  arrange(desc(ws_total)) %>% 
  group_by(Id) %>%
  summarise(across(all_of(var_mean), ~ mean(.x, na.rm = TRUE)),
            across(all_of(var_mode), ~ modal(.x, na.rm = TRUE)),
            across(all_of(var_count), ~ mean(.x, na.rm = TRUE))) %>% 
  as.data.table()

#Aggregation method 2 - aggregate using the value of n watersheds with the greatest upstream extent

#Subset the variables, merge the fishnet id, summarize after grouping by fishnet id
data_in_summarized_v2 = 
  summarized_HYBAS %>%
  #dplyr::select(1, all_of(all_variables)) %>%
  mutate(across(FFI2000:ws_total, ~ ifelse(.x == -999, NA, .x))) %>%
  merge(data_merged_fishnet[, c("HYBAS_ID", "Id")], by = 'HYBAS_ID') %>%
  #merge(upstream_HYBAS, by = "HYBAS_ID", all.x = T) %>%
  arrange(desc(ws_total)) %>% 
  group_by(Id) %>%
  slice(1:5) %>%
  summarise(across(all_of(var_mean), ~ mean(.x, na.rm = TRUE)),
            across(all_of(var_mode), ~ modal(.x, na.rm = TRUE)),
            across(all_of(var_count), ~ mean(.x, na.rm = TRUE))) %>% 
  as.data.table()

#############################
#Merge the summarized dataset back to the fishnet to produce final output
out_fish_v1 = 
  fishnet %>%
  dplyr::select(Id, Lat, Lon) %>%
  merge(data_in_summarized_v1, by = 'Id', all.x = T) %>%
  filter(!is.na(BHI_2010))

out_fish_v2 = 
  fishnet %>%
  dplyr::select(Id, Lat, Lon) %>%
  merge(data_in_summarized_v2, by = 'Id', all.x = T) %>%
  filter(!is.na(BHI_2010))

#############################
#Tests: Create rasters and visualize output
out_fish_r = 
  fasterize(out_fish_v1, fishnet.r %>% raster, field = "Forests") %>%
  crop(data_sub) %>%
  plot()

out_fish_r2 =
  fasterize(out_fish_v2, fishnet.r %>% raster, field = "Forests") %>%
  crop(data_sub) %>%
  plot()

#############################
#Create the final DF for export
out_fish_df_v1 = 
  out_fish_v1 %>% 
  st_drop_geometry() %>%
  as.data.table()

out_fish_df_v2 = 
  out_fish_v2 %>% 
  st_drop_geometry() %>%
  as.data.table()


#############################
#Write Output

#Save the summarized output

#Save the summarized output
if(cur_run == 'unconnected'){
  path2output= paste0(proj_dir, 'Unconnected/', cur_continent, '/', cur_folder_name,'/')
  fwrite(out_fish_df_v1, paste0(path2output, 'unconnected_', cur_continent_abr_UP, '_allWS', 
                                 '.csv'))
  
  fwrite(out_fish_df_v2, paste0(path2output, 'unconnected_', cur_continent_abr_UP, '_top5up', 
                                 '.csv'))
  
} else {
  path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
  fwrite(out_fish_df_v1, paste0(path2output, 'upstream_', cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))
  
  fwrite(out_fish_df_v2, paste0(path2output, 'upstream_', cur_continent_abr_UP, '_top5up_', 
                                 dist_threshold, '.csv'))
}



#writeRaster(out_fish_r, paste0(proj_dir, 'Upstream/test.tif'), , overwrite=TRUE)

#writeRaster(out_fish_r2, paste0(proj_dir, 'Upstream/test3.tif'), overwrite=TRUE)
path2output= paste0(proj_dir, 'Unconnected/', cur_continent, '/', cur_folder_name,'/')
test = 
  fread(paste0(path2output, 'unconnected_', cur_continent_abr_UP, '_allWS','.csv'))

path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
dist_threshold = 'w100km' #nodist #w100km
test2 = 
  fread(paste0(path2output, 'upstream_', cur_continent_abr_UP, '_allWS_', 
                 dist_threshold, '.csv'))


test_all = 
  merge(test, test2, by = c('Lat', 'Lon'), all.x = T)

plot(test_all$ForestAge_TC000.x, test_all$ForestAge_TC000.y, 
     xlab = 'Forest Age - unconnected', ylab = 'Forest Age - upstream',
     main = 'Forest Age - unconnected vs upstream')

plot(test_all$FFI2000.x, test_all$FFI2000.y, 
     xlab = 'Forest Fragmentation - unconnected', ylab = 'Forest Fragmentation - upstream',
     main = 'Forest Fragmentation in 2000 - unconnected vs upstream')

test2 = rasterFromXYZ(test2[c("x", "y", "FFI2020_med")], crs=4326) 
writeRaster(test2, paste0(proj_dir, 'Upstream/test2.tif'))



#Load the upstream-HYBAS linkage
upstream_HYBAS =
  readRDS(paste0(proj_dir, 'Upstream/upstream_codes_SA.rds')) %>%
  group_by(HYBAS_ID) %>%
  summarise(count = n())
