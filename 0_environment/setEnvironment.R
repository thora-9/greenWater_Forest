#####################################
##Set Environment
#####################################

library(tidytable)
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
library(parallel)
library(qgisprocess)

#project code
reportCode = "livable_planet"
projCode = "greenwater_forests_biodiv"

#paths
if(machine == 'WB_VDM'){
  proj_dir = paste0("C:\\Users\\wb583485\\Documents\\WB\\", reportCode, "\\",
                    projCode, "\\")
  database = "C:\\Users\\wb583485\\Documents\\Database\\"
} else{
  proj_dir = paste0("~/Dropbox/WB/", reportCode, "/", 
                    projCode, "/")
  database = "~/Dropbox/Database/"
}


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}


##############################
#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet_fixed.gpkg'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

##############################
#Load World Regions

wb_regions = 
  st_read(paste0(database, "Admin/WB_Regions/WB_countries_Admin0_10m_QGIS_fixed.gpkg")) %>%
  filter(TYPE != 'Dependency') %>%
  dplyr::select(WB_NAME, ISO_A2, ISO_A3, ISO_N3, TYPE, REGION_WB) %>%
  mutate(rownumber = row_number()) %>% #Need to create custom ID; otherwise some duplicates due to random colonial territories
  mutate(ISO_N3 = as.numeric(ISO_N3))

wb_regions_non_spatial = 
  wb_regions %>% st_drop_geometry() %>% as.data.table()

#Use the ISO_N3 to create a raster
wb_regions_raster = 
  fasterize(wb_regions, fishnet.r %>% raster, "rownumber") %>% rast
