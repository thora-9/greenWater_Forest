#########################################################
##Estimate indices for each basin (HydroATLAS/BASIN)
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Specify the continent
cur_continent = 'Australasia'
cur_continent_abr = 'AU'
cur_continent_abr_UP = 'au'
cur_run = 'unconnected' #unconnected #upstream

#Load the data to be estimated
##############################
#Forest Fragmentation Data 
data_in1 = 
  rast(paste0(database, "Forestry/forest_fragmentation_Ma_2023/FFI2000.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(fishnet.r, method = 'bilinear') 

data_in2 = 
  rast(paste0(database, "Forestry/forest_fragmentation_Ma_2023/FFI2020.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear') 

reference_raster = fasterize(fishnet, data_in1 %>% raster) %>% rast

##############################
#Forest Management Data 

fm_generated = 'Y'

fm_4x = rast(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"))
fm_aggregated = 
  terra::aggregate(fm_4x, cores = 16, fact = 32, fun = 'modal', na.rm = T)

#Make sure the NA's are coded correctly
fm_aggregated = 
  ifel(fm_aggregated == 128, NA, fm_aggregated) %>%
  terra::resample(data_in1, method = 'mode') 

names(fm_aggregated) = 'FM_class_mode'

if(fm_generated == 'N'){
  fm_8x = terra::aggregate(fm_4x, cores = 16, fact = 2, fun = 'modal', na.rm = T)
  
  #Approach 2: Get the percent of each class
  
  fm_8x = ifel(fm_8x == 128, NA, fm_8x)
  
  total_cells_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){length(x)}) 
  
  natural_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x == 11, na.rm = T)})
  
  natural_managed_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x == 20, na.rm = T)}) 
  
  natural_all_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x %in% c(11, 20), na.rm = T)}) 
  
  planted_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x %in% c(31, 32, 40), na.rm = T)}) 
  
  agroforestry_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x == 53, na.rm = T)}) 
  
  missing_32x = 
    terra::aggregate(fm_8x, cores = 16, fact = 4, fun=function(x){sum(x == 128, na.rm = T)}) 
  
  
  all_32x = 
    c(natural_32x, natural_managed_32x, natural_all_32x, planted_32x, agroforestry_32x, total_cells_32x)
  
  names(all_32x) = c("natural_only", "natural_managed", "natural_all", "planted", "agroforestry", "total_cells")
  
  #Write this conditioned output to save time
  #saveRDS(all_32x, paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_per_class_32x.tif"))
} else if (fm_generated == 'Y'){
  all_32x = 
    readRDS(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_per_class_32x.tif"))
  
  all_32x = 
    all_32x %>%
    terra::aggregate(fact = 16, fun = 'sum') %>%
    terra::project(data_in1, method = 'near') 
}


##############################
#Biodiversity Habitat Index
path2data = paste0(database, "Biodiversity/Habitat/bio_habitat_index_Harwood_2022/data/BHI_v2_Proportion_of_Species/")

data_BHI1 = 
  rast(paste0(path2data, "BILBI_P_BHIv2_Species_2010.tif")) %>%
  terra::resample(data_in1, method = 'bilinear')

data_BHI2 = 
  rast(paste0(path2data, "BILBI_P_BHIv2_Species_2020.tif")) %>%
  terra::resample(data_in1, method = 'bilinear')

data_BHI = c(data_BHI1, data_BHI2)

names(data_BHI) = c("BHI_2010", "BHI_2020")

##############################
#Forest Age
path2data = paste0(database, "Forestry/forest_age_Besnard_2021/")

data_forestAge = 
  rast(paste0(path2data, "202437221216222_BGIForestAgeMPIBGC1.0.0.nc")) %>%
  .[[c(1,3)]] %>% #Select the 20% threshold and no threshold case
  terra::resample(data_in1, method = 'bilinear')

##############################
#IUCN Species Richness
path2data = paste0(database, "Biodiversity/Richness/IUCN_Species Richness_2022_2/")

data_richness1 = 
  rast(paste0(path2data, "Combined_SR_2022/Combined_SR_2022.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear')

data_richness2 = 
  rast(paste0(path2data, "Amphibians_SR_2022/Amphibians_SR_2022.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear')

data_richness3 = 
  rast(paste0(path2data, "Birds_SR_2022/Birds_SR_2022.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear')

data_richness4 = 
  rast(paste0(path2data, "Mammals_SR_2022/Mammals_SR_2022.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear')

data_richness5 = 
  rast(paste0(path2data, "Reptiles_SR_2022/Reptiles_SR_2022.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear')

data_richness = c(data_richness1, data_richness2, data_richness3, data_richness4, data_richness5)

##############################
#Biodiversity Intactness Index
path2data = paste0(database, "Biodiversity/Intactness/BII_Newbold_2016/")

data_BII = 
  rast(paste0(path2data, "lbii.asc")) %>%
  terra::resample(data_in1, method = 'bilinear')

names(data_BII) = 'BII'

##############################
#Tree Species Richness
path2data = paste0(database, "Biodiversity/Richness/Tree_Species_Richness_Liang_2022/")

data_TSR = 
  rast(paste0(path2data, "S_mean_raster.tif")) %>%
  terra::resample(data_in1, method = 'bilinear')

names(data_TSR) = 'Tree_Species'

##############################
#Habitat Extent
path2data = paste0(database, "LULC/Habitat_Jung_2020/")

#Habitat layers 
layer_list = 
  list.files(paste0(database, "LULC/Habitat_Jung_2020/"), ".tif$",
             full.names = T)

habitat_layers = 
  rast(layer_list) 

habitat_extent = sum(habitat_layers[[c(1,3:8)]], na.rm = T)

data_habitat = 
  c(habitat_layers, habitat_extent) %>%
  terra::resample(data_in1, method = 'bilinear')

data_habitat = data_habitat/1000

names(data_habitat) = c("Forests","Artificial",
                                  "Savanna", "Shrubland",
                                  "Grassland","Wetland_Inland",
                                  "Rocky Areas", "Desert", "Habitat_Extent")

data_habitat = 
  ifel(is.na(data_habitat), 0, data_habitat) %>%
  mask(reference_raster)

##########################################################################################
#Create a dataframe with all the variables

#Create a raster stack with all the indices
data_all = 
  c(data_in1, data_in2, data_BHI, data_forestAge, data_richness, 
    data_BII, data_TSR, data_habitat, fm_aggregated, all_32x) 

data_all_df = 
  as.data.frame(data_all, xy = T) %>%
  as.data.table() %>%
  merge(fishnet %>% st_drop_geometry() %>% as.data.frame(),
        by.x = c('x', 'y'), by.y = c('Lon', 'Lat')) %>%
  mutate(across(natural_only:total_cells, ~ .x/total_cells))


##########################################################################################
#Load the upstreamness index file

cur_continent_abr = 'AS'

path2linkage = paste0(proj_dir, 'Upstreamness/', '/')
upstreamness = 
  fread(paste0(path2linkage, 'upstreamness_cell_',cur_continent_abr, '.csv')) 

##############################
#Estimate the index for upstream regions

#Split the df by HYBAS_ID
upstream_list =
  as.list(unique(upstreamness$OBJECTID)) 


#Function to get the centroid distances
summarize_upstream <- function(obj) {
  
  cur_neighbours = 
    upstreamness %>%
    filter(OBJECTID == obj) %>%
    arrange(neighbour_ID) %>%
    mutate(upstreamness_across = ifelse(is.na(upstreamness_across), 0, upstreamness_across))
  
  cur_values = 
    data_all_df %>%
    filter(OBJECTID %in% cur_neighbours$neighbour_ID) %>%
    arrange(OBJECTID) 
  
  weights = 
    cur_neighbours$upstreamness_across 
  
  
  #Summarise variables 
  cur_var_upstream = 
    cur_values %>%
    tidytable::summarise(across(FFI2000:Habitat_Extent, ~ weighted.mean(.x, weights, na.rm = T)),
                         across(natural_only:total_cells, ~ weighted.mean(.x, weights, na.rm = T))) %>%
    as.data.table()
    
  
  #Store the output for the current watershed
  out_df = 
    cur_neighbours %>%
    dplyr::select(1:3) %>%
    distinct() %>%
    cbind(cur_var_upstream)
  
  out_df
  
  # #Add it to the main output
  # out_df = rbind(out_df, out_temp)
  # 
  # getdist
  
}

#Run the process in parallel
start = Sys.time()
out_list <- pblapply(upstream_list, summarize_upstream)
#out_list <- parallel::mclapply(upstream_HYBAS_list[1:10000], summarize_upstream, mc.cores = 4)
end = Sys.time()

end - start

#Merge the lists
out_list_merged = rbindlist(out_list)


#Save the summarized output
##########################################################################################
path2output = paste0(proj_dir, 'Upstreamness/var_summary', '/')
fwrite(out_list_merged, paste0(path2output, 'upstreamness_var_summary_', cur_continent_abr, '.csv'))



test = fread(paste0(proj_dir, 
                    'Upstream/', 'Asia', '/240821/upstream_as_top5up_w100km.csv'))


df_all = 
  test %>%
  merge(out_list_merged, by.x = 'Id', by.y = 'OBJECTID', all.x = T)


# 
# 
test = readRDS(paste0(path2linkage, '240821/','unconnected_VAR_', cur_continent_abr_UP,'.rds'))
test2 = readRDS(paste0('/Users/tejasvi/Dropbox/WB/livable_planet/greenwater_forests_biodiv/Upstream/South America/',
                       '240821/','upstream_VAR_sa_w100km','.rds'))

