##############################
##Forest Management
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
machine = 'gcp'
if(machine == 'gcp'){
  proj_dir = "/home/tejasvi/Projects/greenWater_Forest/"
  database = "/home/tejasvi/Database/"
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
#fm_4x = terra::aggregate(FM_source, cores = 16, fact = 4, fun = 'modal')
#writeRaster(fm_4x, paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"), overwrite = TRUE)

#Load the saved 4x version
fm_4x = rast(paste0(database, "Forestry/forest_managemen_lesiv_2021/FML_v3_2_4x.tif"))
fm_8x = terra::aggregate(fm_4x, cores = 16, fact = 2, fun = 'modal', na.rm = T)

NAflag(fm_8x)<-128


##############################
#Approach-1: Mode
fm_256x = terra::aggregate(fm_4x, cores = 8, fact = 64, fun = 'modal', na.rm = T)

##Get the aggregation based on mode
data_fishnet = 
  terra::resample(fm_256x, fishnet.r, method = 'near') %>%
  mask(fishnet.r)

data_fishnet = ifel(data_fishnet==128, NA, data_fishnet)

#Create a lookup table linking codes to classes
fm_classes = data.frame(FM_class_code = c(11, 20, 31, 32, 40, 53),
                        FM_class = c("Naturally regenerating forest; no management",
                                     "Naturally regenerating forest; management",
                                     "Planted forests",
                                     "Plantation forests (rotation time up to 15 years)",
                                     "Oil palm plantations",
                                     "Agroforestry"))

#Convert to dataframe
out.df1 = 
  as.data.frame(data_fishnet, xy = T) %>%
  dplyr::rename(FM_class_code = `FML_v3-2_with-colorbar`) %>%
  merge(fm_classes, by = "FM_class_code", all.x = T) %>%
  dplyr::select(2,3,1,4) %>%
  as.data.table()

##############################
#Approach 2: Get the percent of each class

total_cells_05deg = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){length(x)}) %>%
  terra::resample(fishnet.r, method = 'near') 

# total_cells_05deg = 
#   ifel(fm_4x==128, NA, fm_4x) %>%
#   terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(!is.na(x))}) %>%
#   terra::resample(fishnet.r, method = 'near') 

natural_05deg = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(x == 11, na.rm = T)}) %>%
  terra::resample(fishnet.r, method = 'near') 
  
natural_managed_05deg = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(x == 20, na.rm = T)}) %>%
  terra::resample(fishnet.r, method = 'near')

natural_all_05deg = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(x %in% c(11, 20), na.rm = T)}) %>%
  terra::resample(fishnet.r, method = 'near') 

planted_512x = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(x %in% c(31, 32, 40), na.rm = T)}) %>%
  terra::resample(fishnet.r, method = 'near') 

agroforestry_512x = 
  terra::aggregate(fm_4x, cores = 8, fact = 128, fun=function(x){sum(x == 53, na.rm = T)}) %>%
  terra::resample(fishnet.r, method = 'near') 

all_512 = 
  c(natural_05deg/total_cells_05deg, natural_managed_05deg/total_cells_05deg, natural_all_05deg/total_cells_05deg,
    planted_512x/total_cells_05deg, agroforestry_512x/total_cells_05deg)

names(all_512) = c("natural_only", "natural_managed", "natural_all", "planted", "agroforestry")

#Convert to dataframe
out.df = 
  as.data.frame(all_512, xy = T) %>%
  as.data.table() %>%
  mutate(across(natural_only:agroforestry, ~ round(.x, 2))) %>%
  mutate(score = rowSums(across(natural_only:agroforestry))) %>%
  filter(score != 0) %>%
  dplyr::select(-score)

##############################    
#Check with existing dataframe to see if cells exist/coverage
test = read_dta(paste0(proj_dir, "esha-data/econ_greenwater_paper data/fishnet_upstream_share_forest_esa_1992.dta"))

#X coordinates
sum(out.df$x %in% test$longitude) == nrow(out.df)

#Y coordinates
sum(out.df$y %in% test$latitude) == nrow(out.df)

test2 = merge(out.df, test[, 1:4], by.x = c('x', 'y'), by.y = c('longitude', 'latitude'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(out.df)

##############################
#Write Output
##############################
fwrite(out.df, paste0(database, "Forestry/forest_managemen_lesiv_2021/forest_management_05_proportions.csv"))

writeRaster(all_512, paste0(database, "Forestry/forest_managemen_lesiv_2021/forest_management_05_prop.tif"), overwrite = TRUE)
