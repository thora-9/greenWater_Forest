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
library(magrittr)
library(hablar)

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

#Good place for metadata - https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean 
#And - https://www.isric.org/explore/wosis/accessing-wosis-derived-datasets 
#Variables/Units - 
#1) bdod - Bulk density of the fine earth fraction - cg/cm³
#2) cec - Cation Exchange Capacity of the soil - mmol(c)/kg
#3) cfvo - Volumetric fraction of coarse fragments (> 2 mm) - cm3/dm3 (vol‰)
#4) clay - Proportion of clay particles (< 0.002 mm) in the fine earth fraction - g/kg
#5) nitrogen - Total nitrogen (N) - cg/kg
#6) phh2o - Soil pH - pHx10
#7) sand - Proportion of sand particles (> 0.05 mm) in the fine earth fraction - g/kg
#8) silt - Proportion of silt particles (≥ 0.002 mm and ≤ 0.05 mm) in the fine earth fraction - g/kg
#9) soc - Soil organic carbon content in the fine earth fraction - dg/kg
#10) ocd - Organic carbon density - hg/m³
#11) ocs - Organic carbon stocks - t/ha
#12 wv0010/wv0033/wv1500 - Water retention volumetric at 10, 33 and 1500 kPa - cm³/100cm³

getFiles = list.files(paste0(database, 'Soils/SoilGrids/5000m'), pattern = '.tif', full.names = T,  recursive = T)
getNames = list.files(paste0(database, 'Soils/SoilGrids/5000m'), pattern = '.tif', recursive = T, include.dirs = F)

#Load the rasters
soilGrids_5000m = 
  rast(getFiles) %>%
  terra::project(crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(fishnet.r, method = 'bilinear')

#Convert to dataframe
out.df = 
  as.data.frame(soilGrids_5000m, xy = T) %>%
  dplyr::relocate(`ocs_0-30cm_mean_5000`, .after = last_col()) %>%
  #gather(variable, value, `bdod_0-5cm_mean_5000`:`wv1500_60-100cm_mean_5000`) %>%
  as.data.table()

l <- map(seq(3, ncol(out.df)-1, 6), ~seq(.x, .x + 5))

test = 
  map_dfc(l, ~out.df %>% transmute(mean = rowMeans(.x))) %>% 
  set_rownames(rownames(out.df))


##############################    
#Check with existing dataframe to see if cells exist/coverage
#X coordinates
sum(out.df$x %in% fishnet$Lon) == nrow(out.df)

#Y coordinates
sum(out.df$y %in% fishnet$Lat) == nrow(out.df)

test2 = merge(out.df, fishnet[, 1:4], by.x = c('x', 'y'), by.y = c('Lon', 'Lat'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(out.df)

##############################
#Write Output
##############################
fwrite(out.df, paste0(path2data, "processed/forest_age_05deg.csv"))

