##############################
##CRU - Precipitation
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
#CRU Precip data
cru_precip =
  tidync(paste0(database,'Climate/Precipitation/CRU/cru_ts4.07.1901.2022.pre.dat.nc')) %>%
  activate("D0,D1,D2") %>%
  hyper_tibble() %>% as.data.table() %>%
  .[order(lat, lon)] %>% #ensures the order is the same
  group_by(lat,lon) %>%
  mutate(cell_id = cur_group_id()) %>% 
  as.data.table()

#Dates 
dates1 = 
  cru_precip[,.(time)] %>% distinct() %>%
  mutate(dates = as.Date(time, origin = '1900-01-01')) %>%
  mutate(ym = substr(ymd(dates), 1, 7),
         year = year(dates),
         month = month(dates)) 

dates1 = dates1[!duplicated(dates1$ym)]

#Add the dates to the precip data
cru_precip = 
  cru_precip %>% 
  merge(dates1, by = 'time')

scale_vec = function(x) {c(scale(x))} #Need this to be a vectorized version - https://stackoverflow.com/questions/54695830/data-table-create-multiple-columns-with-lapply-and-sd

#1) Subset to time period after 1970
#2) Calculate the climatology
#3) Remove the climatology
#4) Estimate the z-score of the deficit 
#5) Bin the monthly precip based on the z-score into dry, wet and normal
#6) Get the median monthly z-score to get the yearly precip z_score
#7) Bin the yearly z-score into dry, wet and normal
cru_precip_70_22 = 
  cru_precip[year >= 1970] %>%
  .[, climatology := mean(pre, na.rm = T), .(cell_id, month)] %>%
  .[order(cell_id, year, month)] %>%
  .[, deficit := pre - climatology] %>%
  .[, z_score_pre := scale_vec(deficit), cell_id] %>%
  .[, z_score_bin := ifelse(z_score_pre <= -1, 'Dry', 
                            ifelse(z_score_pre >= 1, 'Wet', 'Normal'))] 

scale_vec = function(x) {c(scale(x))} #Need this to be a vectorized version - https://stackoverflow.com/questions/54695830/data-table-create-multiple-columns-with-lapply-and-sd

cru_precip_70_22_yearly = 
  cru_precip_70_22 %>%
  group_by(lon, lat, cell_id, year) %>%
  mutate(pre_yearly = sum(pre)) %>%
  dplyr::select(lon, lat, cell_id, year, pre_yearly) %>%
  distinct() %>%
  as.data.table() %>%
  .[, z_score_pre_yearly := scale_vec(pre_yearly), .(cell_id)] %>%
  .[, z_score_bin_yearly := ifelse(z_score_pre_yearly <= -1, 'Dry', 
                            ifelse(z_score_pre_yearly >= 1, 'Wet', 'Normal'))] 

#fwrite(cru_precip_70_22_yearly, paste0(database,'Climate/Precipitation/CRU/cru_precip_70_22_yearly.csv'))

