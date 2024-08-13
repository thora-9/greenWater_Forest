####################################
##Forest Landscape Integrity Index
####################################

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
#Forest Fragmentation Data
path2data = paste0(database, "Forestry/forest_landscapeIntegrity_Grantham_2020/")

data_in1 = 
  rast(paste0(path2data, "flii_earth.tif"))

data_in_4x = terra::aggregate(data_in1, cores = 16, fact = 4, fun = 'median', na.rm=TRUE)


#Align and resample the data
data_fishnet1 = 
  terra::resample(data_in1, fishnet.r, method = 'bilinear')

data_fishnet2 = 
  terra::resample(data_in1, fishnet.r, method = 'med')

data_fishnet = c(data_fishnet1, data_fishnet2)

#Convert to dataframe
out.df = 
  as.data.frame(data_fishnet, xy = T) %>%
  dplyr::rename(FLII_bil = 3, FLII_med = 4) %>%
  mutate(FLII_bil = round(FLII_bil, 2),
         FLII_med = round(FLII_med, 2)) %>%
  as.data.table()

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

path2out = paste0(path2data ,"processed/")

if (dir.exists(path2out) == F) {
  dir.create(path2out)
}

fwrite(out.df, paste0(path2out ,"FLII_05deg.csv"))
