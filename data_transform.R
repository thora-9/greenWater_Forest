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

library(usethis) 
#usethis::edit_r_environ()

#paths
proj_dir = "~/Dropbox/WB/greenWater_Forest/"
database = "~/Dropbox/Database/"

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

#Load the watersheds

ws12_asia = 
  st_read(paste0(database, "Watersheds/hybas_as_lev01-12_v1c/hybas_as_lev12_v1c.shp")) 

ws12_africa = 
  st_read(paste0(database, "Watersheds/hybas_af_lev01-12_v1c/hybas_af_lev12_v1c.shp")) 

#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp'))

#For figures
fishnet.sub = 
  fishnet %>% filter(Id == 41861)

ws12_sub = 
  ws12_asia %>% st_crop(fishnet.sub)


#Load the fishnet
fishnet.r = 
  fishnet %>%
  st_drop_geometry() %>%
  as.data.table()

fishnet.r = 
  rasterFromXYZ(fishnet.r[,.(Lon, Lat)])

crs(fishnet.r) = crs(wb_regions)

fishnet.r = mask(fishnet.r, wb_regions) %>% rast

#Forest Management Data 
FM_source = 
  rast(paste0(proj_dir, "Forestry/forest_managemen_lesiv_2021/FML_v3-2_with-colorbar.tif"))

#Create an aggregated layer using modal (basic implementation)
fm_05deg = terra::resample(FM_source, fishnet.r, method = 'mode')

fm_05deg = terra::aggregate(FM_source, fact = 500, fun = 'modal')

# #Create a forest mask layer for calculating percentages later
# forest_mask = 
#   ifel(is.na(FM_source), NA, 1)



