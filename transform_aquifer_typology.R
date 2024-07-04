############################################################
##Aquifer Typology
############################################################

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


source('myfunctions.R')

#paths
machine = 'local'
if(machine == 'gcp'){
  proj_dir = "/home/thora/Projects/greenWater_Forest/"
  database = "/home/thora/Database/"
} else{
  proj_dir = "~/Dropbox/WB/irrigation_dev/"
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

#DEM (reference data)
dem = 
  terra::rast(paste0(proj_dir, 'DEM/hyd_sa_dem_15s/hyd_sa_dem_15s.tif')) %>%
  crop(wb_regions) %>%
  terra::aggregate(fact = 8, fun = 'mean')

#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

#Reference Raster
fishnet.r.03125 = 
  disagg(fishnet.r, fact = 16)


##############################
#CI data
path2data = paste0(database, "Groundwater/aquifer_typology_worldbank_2023/")


data_lookup = 
  data.frame(aqtyp = c('Complex', "Karstic", "Local/Shallow", "Major Alluvial"),
             aqtyp_code = c(1, 2, 3, 4))

data_all = 
  st_read(paste0(path2data, 'aqtyp_dissolved.gpkg')) %>%
  merge(data_lookup, by = 'aqtyp') %>%
  fasterize(fishnet.r.03125 %>% raster, field = 'aqtyp_code')


out.df = 
  as.data.frame(data_all, xy = T) %>%
  as.data.table() %>%
  dplyr::rename(aqtyp_code = layer) %>%
  merge(data_lookup, by = 'aqtyp_code') 
  

terra::writeRaster(data_all,
                   paste0(path2data, "processed/aquifer_typology_03125.tif"),
                   overwrite = T)

#st_write(data_in_merged, paste0(proj_dir, "Outcomes_Variables/relative_wealth_index/RWI_03125.gpkg"))

fwrite(out.df, paste0(path2data, "processed/aquifer_typology_03125.csv"))

