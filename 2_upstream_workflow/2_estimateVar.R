#########################################################
##Estimate indices for each basin (HydroATLAS/BASIN)
#########################################################

library(tidync)
library(data.table)
library(tidyverse)
library(lubridate)
library(zyp)
library(sf)
library(raster)
library(terra)
library(RColorBrewer)
library(rasterVis)
library(xts)
library(fasterize)
library(stringdist)
library(fuzzyjoin)
library(stars)
library(tidyterra)
library(modelr)
library(haven)
library(pbapply)


#paths
machine = 'local'
if(machine == 'gcp'){
  proj_dir = "/home/thora/Projects/greenWater_Forest/"
  database = "/home/thora/Database/"
} else{
  proj_dir = "~/Dropbox/WB/greenWater_Forest/"
  database = "~/Dropbox/Database/"
}


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}


#Specify the continent
cur_continent = 'Asia'
cur_continent_abr = 'AS'
cur_continent_abr_UP = 'as'

##############################
####Load World Regions
wb_regions = 
  st_read(paste0(database, "Admin/WB_Regions/WB_countries_Admin0_10m.shp")) %>%
  dplyr::select(WB_NAME, ISO_A2, ISO_A3, ISO_N3, TYPE, REGION_WB) %>%
  filter(TYPE != 'Dependency') %>%
  st_make_valid()

#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

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

#Load the upstream-HYBAS linkage
#This was calculated in step 1 of the workflow
path2upstream = paste0(proj_dir, 'Upstream/', cur_continent, '/')
upstream_HYBAS = readRDS(paste0(path2upstream, 'upstream_codes_',cur_continent_abr_UP, '_wdis.rds'))

#Load the data to be estimated
##############################
#Forest Fragmentation Data 
data_in1 = 
  rast(paste0(database, "Forestry/forest_fragmentation_Ma_2023/FFI2000.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') #Use bilinear as the variable is continuous


data_in2 = 
  rast(paste0(database, "Forestry/forest_fragmentation_Ma_2023/FFI2020.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(data_in1, method = 'bilinear') 

##############################
#Forest Management Data 

fm_generated = 'Y'

fm_4x = rast(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"))
fm_aggregated = 
  terra::aggregate(fm_4x, cores = 16, fact = 4, fun = 'modal', na.rm = T)

#Make sure the NA's are coded correctly
fm_aggregated = ifel(fm_aggregated == 128, NA, fm_aggregated)

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
  all_32x = readRDS(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_per_class_32x.tif"))
  
}

##############################
#Biodiversity Habitat Index
path2data = paste0(database, "Biodiversity/Habitat/bio_habitat_index_Harwood_2022/data/BHI_v2_Proportion_of_Species/")

data_BHI1 = 
  rast(paste0(path2data, "BILBI_P_BHIv2_Species_2010.tif")) %>%
  terra::resample(data_in1, method = 'med')

data_BHI2 = 
  rast(paste0(path2data, "BILBI_P_BHIv2_Species_2020.tif")) %>%
  terra::resample(data_in1, method = 'med')

data_BHI = c(data_BHI1, data_BHI2)

names(data_BHI) = c("BHI_2010", "BHI_2020")

##############################
#Forest Age
path2data = paste0(database, "Forestry/forest_age_Besnard_2021/")

data_forestAge = 
  rast(paste0(path2data, "202437221216222_BGIForestAgeMPIBGC1.0.0.nc")) %>%
  .[[c(1,3)]] %>% #Select the 20% threshold and no threshold case
  terra::resample(data_in1, method = 'med')

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
  terra::resample(data_in1, method = 'med')

names(data_BII) = 'BII'

##############################
#Create a spatvector version of the vector HYBAS file  
data_sub_sv = 
  data_sub %>% 
  dplyr::select(HYBAS_ID) %>%
  as_spatvector()

#Variables summarized by mean
#Create a raster stack with all the indices
data_in_mean = 
  c(data_in1, data_in2, data_BHI, data_forestAge, data_richness, data_BII) 

#Variables summarized by mode (categorical var)
data_in_mode = 
  c(fm_aggregated)

#Variables summarized by count
data_in_count = 
  c(all_32x)

##############################
#Extract the values 
var_HYBAS_mean = 
  terra::extract(data_in_mean, data_sub_sv, fun = 'mean', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

var_HYBAS_mode = 
  raster::extract(data_in_mode, data_sub_sv, fun = 'modal', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

var_HYBAS_count = 
  terra::extract(data_in_count, data_sub_sv, fun = 'sum', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

var_HYBAS_all = 
  list(var_HYBAS_mean, var_HYBAS_mode, var_HYBAS_count) %>%
  reduce(full_join, by='HYBAS_ID')


##############################
#Estimate the index for upstream regions

#Split the df by HYBAS_ID
upstream_HYBAS_list =
  split(data_sub_DT, by = 'HYBAS_ID') 

#Test - list number 27
dist_threshold_km = 200 #Use 99999 for essentially no dist threshold


#Function to get the centroid distances
summarize_upstream <- function(df) {
  
  #Subset the upstream linkage to the given watershed
  cur_upstream_HYBAS = 
    upstream_HYBAS[HYBAS_ID == df$HYBAS_ID] %>%
    dplyr::filter(distance < dist_threshold_km)
  
  #Summarise variables 
  cur_var_upstream = 
    var_HYBAS_all[HYBAS_ID %in% cur_upstream_HYBAS$upstream_chain] %>%
    summarise(across(FFI2000:BII, ~ mean(.x, na.rm = T)),
              across(FM_class_mode, ~ modal(.x, na.rm = T)),
              across(natural_only:total_cells, ~ sum(.x, na.rm = T))) %>%
    mutate(upstream_total = length(unique(cur_upstream_HYBAS$upstream_chain)))
  
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
#out_list <- parallel::mclapply(upstream_HYBAS_list[1:10000], summarize_upstream, mc.cores = 4)
end = Sys.time()

end - start

#Merge the lists
out_list_merged = rbindlist(out_list)


#Save the summarized output

if(dist_threshold_km == 99999){
  saveRDS(out_list_merged, paste0(path2upstream, 'upstream_VAR_', cur_continent_abr_UP, '_nodist.rds'))
} else if (dist_threshold_km == 200){
  saveRDS(out_list_merged, paste0(path2upstream, 'upstream_VAR_', cur_continent_abr_UP, '_w200km.rds'))
}




test = readRDS(paste0(proj_dir, 'Upstream/upstream_VAR_set2_SA.rds'))
test2 = readRDS(paste0(proj_dir, 'Upstream/upstream_VAR_SA_w200km.rds'))
