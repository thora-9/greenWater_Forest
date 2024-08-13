##############################
##IUCN Species Richness
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
path2data = paste0(database, "Biodiversity/Richness/IUCN_Species Richness_2022_2/")

data_in1 = 
  rast(paste0(path2data, "Combined_SR_2022/Combined_SR_2022.tif"))

data_in2 = 
  rast(paste0(path2data, "Amphibians_SR_2022/Amphibians_SR_2022.tif")) %>%
  terra::resample(data_in1, method = 'bilinear')

data_in3 = 
  rast(paste0(path2data, "Birds_SR_2022/Birds_SR_2022.tif"))

data_in4 = 
  rast(paste0(path2data, "Mammals_SR_2022/Mammals_SR_2022.tif"))

data_in5 = 
  rast(paste0(path2data, "Reptiles_SR_2022/Reptiles_SR_2022.tif"))

data_all = c(data_in1, data_in2, data_in3, data_in4, data_in5)

#Align and resample the data
data_fishnet1 = 
  terra::project(data_all, crs(fishnet.r), method = 'bilinear') %>% #Use bilinear as the variable is continuous
  terra::resample(fishnet.r, method = 'bilinear')

#Convert to dataframe
out.df = 
  as.data.frame(data_fishnet1, xy = T) %>%
  rename(Combined_species_richness_2022 = Combined_SR_2022) %>%
  mutate(Combined_species_richness_2022 = round(Combined_species_richness_2022, 2)) %>%
  as.data.table()


##############################    
#Check with existing dataframe to see if cells exist/coverage
test = read_dta(paste0(proj_dir, "esha-data/econ_greenwater_paper data/fishnet_upstream_share_forest_esa_1992.dta"))

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
fwrite(out.df, paste0(database, "Biodiversity/Richness/IUCN_Species Richness_2022_2/processed/IUCN_species_richness_05deg.csv"))

