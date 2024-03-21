##############################
##SoilGrids
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
library(haven)
library(gdalUtilities)


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
#Download Soil Grids
#Using these instructions - 
#https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/webdav_from_R.md
##############################


library(terra)
library(gdalUtilities)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"



