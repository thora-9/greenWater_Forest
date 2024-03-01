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

data_in_atlas =
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

data_in_atlas_DT = 
  data_in_atlas %>% 
  st_drop_geometry() %>%
  as.data.table()

rm(data_in_atlas)

data_in1 = 
  st_read(paste0(database, "Watersheds/HydroBASIN/hybas_sa_lev01-12_v1c/hybas_sa_lev12_v1c.shp"))

data_in_asDT = 
  data_in1 %>% 
  st_drop_geometry() %>%
  as.data.table()


data_sub_DT = 
  data_in_asDT[, 1:4]

#Get just the HYBAS_ID to reduce memory/computation load and then get the centroid
data_sub = 
  data_in1 %>%
  dplyr::select(1:4) 


#To free memory
rm(data_in1)

#Load the upstream-HYBAS linkage
upstream_HYBAS = readRDS(paste0(proj_dir, 'Upstream/upstream_codes_SA.rds'))

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
#Create a raster stack with all the indices
data_in = 
  c(data_in1, data_in2) 

#Create a spatvector version of the vector HYBAS file  
data_sub_sv = data_sub %>% as_spatvector()

##############################
var_per_HYBAS = 
  terra::extract(data_in, data_sub_sv, fun = 'mean', na.rm=TRUE, bind = TRUE) %>%
  as.data.table()

##############################
#Estimate the index for upstream regions
i = 23152
out_df = NULL

for(i in 1:nrow(data_sub_DT)){
  
  #Track progress
  print(i)
  #Get the main level 1 watershed ID/row
  cur_ws_primary = data_sub_DT[i, ]
  cur_ws_ID_primary = cur_ws_primary$HYBAS_ID
  
  #Subset the upstream linkage to the given watershed
  cur_upstream_HYBAS = upstream_HYBAS[HYBAS_ID == cur_ws_ID_primary]
  
  #Summarise variables 
  cur_var_upstream = 
    var_per_HYBAS[HYBAS_ID %in% cur_upstream_HYBAS$upstream_chain] %>%
    summarise(across(FFI2000:FFI2020, ~ mean(.x, na.rm = T))) 
 
  #Store the output for the current watershed
  out_temp = 
    data.table(HYBAS_ID = cur_ws_ID_primary) %>%
    cbind(cur_var_upstream)
  
  #Add it to the main output
  out_df = rbind(out_df, out_temp)
  
}

#Save the summarized output
#saveRDS(out_df, paste0(proj_dir, 'Upstream/upstream_VAR_1_SA.rds'))
