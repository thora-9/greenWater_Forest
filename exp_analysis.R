##############################
##Exploratory analysis
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

#HydroATLAS
hydroATLAS = 
  fread(paste0(database, "Watersheds/HydroATLAS/Processed/hydroATLAS_05deg.csv")) %>%
  drop_na()

#BII
BII = fread(paste0(database, "Biodiversity/Intactness/BII_Newbold_2016/processed/BII_05deg.csv"))

#BHI
BHI = fread(paste0(database, "Biodiversity/Habitat/bio_habitat_index_Harwood_2022/processed/BHI_prop_species_05deg.csv"))



#Combined
allData = 
  merge(hydroATLAS, BII, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(BHI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  mutate(for_pc_sse_interval = cut(for_pc_sse, n = 4, breaks = c(-Inf, 25, 50, 75, 100)),
         other_pc_sse_est = 100-(for_pc_sse+urb_pc_sse+crp_pc_sse)) %>%
  rowwise() %>%
  mutate(maxLC = which.max(c(for_pc_sse, urb_pc_sse, crp_pc_sse, other_pc_sse_est))) %>%
  #filter(for_pc_sse > 50 | urb_pc_sse > 50 | crp_pc_sse > 50) %>%
  group_by(maxLC) %>%
  summarise(BII_med = median(BII_med, na.rm = T),
            BHI_med = median(BHI_2020_med, na.rm = T)) %>%
  mutate(LC_est = c('Forest', 'Urban', 'Crop', 'Other'))



cor.test(allData$for_pc_sse, allData$BII_med)
