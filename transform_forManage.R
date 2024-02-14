##############################
##External Data Transformation
##############################

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

#paths
machine = 'gcp'
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
# 
# 
# #Load the fishnet
# fishnet.r = 
#   fishnet %>%
#   st_drop_geometry() %>%
#   as.data.table()
# 
# fishnet.r = 
#   rasterFromXYZ(fishnet.r[,.(Lon, Lat)])
# 
# crs(fishnet.r) = crs(wb_regions)
# values(fishnet.r) = 1
# 
# fishnet.r2 = fasterize(fishnet, fishnet.r) %>% rast
# 
# writeRaster(fishnet.r2, paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'), overwrite = TRUE)


#Forest Management Data 
FM_source = 
  rast(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3-2_with-colorbar.tif"))

terra::terraOptions(progress = 1, todisk = TRUE)

#Create an aggregated layer using modal (basic implementation)
fm_4x = terra::aggregate(FM_source, cores = 16, fact = 4, fun = 'modal', na.rm = T)
#writeRaster(fm_05deg2, paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"), overwrite = TRUE)

#Load the saved 4x version
fm_4x = rast(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"))

fm_256x = terra::aggregate(fm_4x, cores = 16, fact = 64, fun = 'modal')

fm_512x = terra::aggregate(fm_256x, cores = 16, fact = 2, fun = 'modal')

#Create a fake flag for NA values


fm_05deg = 
  terra::resample(fm_512x, fishnet.r, method = 'mode') %>%
  mask(fishnet.r)


test = terra::aggregate(fishnet.r, cores = 16, fact = 8, fun = 'modal')
test2 = terra::aggregate(fishnet.r, cores = 16, fact = 8, fun = 'modal', na.rm = T)
