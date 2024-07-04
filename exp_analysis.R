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
library(binsreg)


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
#Soil Moisture data
sm_data = read_dta(paste0(proj_dir, "esha-data/econ_greenwater_paper data/sm_shockstouse_19702016.dta"))

# scale_vec = function(x) {c(scale(x))} #Need this to be a vectorized version - https://stackoverflow.com/questions/54695830/data-table-create-multiple-columns-with-lapply-and-sd
# 
# sm_data_sub = 
#   sm_data %>%
#   dplyr::select(1:6) %>%
#   as.data.table() %>%
#   .[,`:=`(z_score_mean = scale_vec(mean_ors_0100), 
#           z_score_olc = scale_vec(olc_ors_0100)), OBJECTID] %>%
#   .[,`:=`(z_mean_3yrAve = frollmean(z_score_mean, n = 3, fill = NA),
#           z_olc_3yrAve = frollmean(z_score_olc, n = 3, fill = NA)), OBJECTID]

sm_data_sub = fread(paste0(proj_dir, "esha-data/econ_greenwater_paper data/sm_shockstouse_19702016_processed.csv"))

#fwrite(sm_data_sub, paste0(proj_dir, "esha-data/econ_greenwater_paper data/sm_shockstouse_19702016_processed.csv"))
  
##############################
#Precipitation
precip = fread(paste0(database,'Climate/Precipitation/CRU/cru_precip_70_22_yearly.csv'))

#SM-Precip (Dry spells)
sm_precip = 
  sm_data_sub %>%
  merge(precip, by.x = c('longitude', 'latitude', 'year'), by.y = c('lon', 'lat', 'year'), all.x = T) %>%
  .[!is.na(cell_id)] %>%
  .[z_score_bin_yearly == 'Dry'] %>%
  dplyr::select(Lon = longitude, Lat = latitude, OBJECTID, pre_yearly, z_score_mean:z_olc_3yrAve) %>%
  group_by(Lon, Lat, OBJECTID) %>%
  summarise(across(z_score_mean:z_olc_3yrAve, ~ sum(.x, na.rm = T)))

##############################
#Grid-level
#HydroATLAS
hydroATLAS = 
  fread(paste0(database, "Watersheds/HydroATLAS/Processed/hydroATLAS_05deg.csv")) %>%
  drop_na()

#BII
BII = fread(paste0(database, "Biodiversity/Intactness/BII_Newbold_2016/processed/BII_05deg.csv"))

#BHI
BHI = fread(paste0(database, "Biodiversity/Habitat/bio_habitat_index_Harwood_2022/processed/BHI_prop_species_05deg.csv"))

#FFI
FFI = fread(paste0(database, "Forestry/forest_fragmentation_Ma_2023/forest_frag_05_v2.csv"))

#Forest Mangement
FM = fread(paste0(database, "Forestry/forest_managemen_lesiv_2021/forest_management_05_proportions.csv"))


#Get the files
path2data = paste0(proj_dir, "Upstream/")
pattern = "top5up_w200km_240407"
files = list.files(path2data, pattern, recursive = T, full.names = T)
file_names = list.files(path2data, pattern, recursive = T)

upstream_data = 
  lapply(files, fread) %>%
  rbindlist()

#Combined
# allData = 
#   merge(hydroATLAS, BII, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
#   merge(BHI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
#   mutate(for_pc_sse_interval = cut(for_pc_sse, n = 4, breaks = c(-Inf, 25, 50, 75, 100)),
#          other_pc_sse_est = 100-(for_pc_sse+urb_pc_sse+crp_pc_sse)) %>%
#   rowwise() %>%
#   mutate(maxLC = which.max(c(for_pc_sse, urb_pc_sse, crp_pc_sse, other_pc_sse_est))) %>%
#   #filter(for_pc_sse > 50 | urb_pc_sse > 50 | crp_pc_sse > 50) %>%
#   group_by(maxLC) %>%
#   summarise(BII_med = median(BII_med, na.rm = T),
#             BHI_med = median(BHI_2020_med, na.rm = T)) %>%
#   mutate(LC_est = c('Forest', 'Urban', 'Crop', 'Other'))

###################################
allData = 
  SA_200km %>%
  rbind(AF_200km) %>% 
  rbind(AS_200km, fill=TRUE) %>% 
  merge(sm_precip, by = c("Lon", "Lat"), all.x = T) %>%
  merge(hydroATLAS, by = c("Lon", "Lat"), all.x = T) %>%
  merge(BII, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(BHI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(FFI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(FM, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T)

test = allData %>%filter(Lon == -16.75)
  
parameter <- par(mfrow=c(2,2)) #set up the plotting space

plot(allData$BHI_2010, allData$BHI_2010_bil, xlab = 'Upstream: BHI', ylab = 'Grid-level: BHI')
abline(coef = c(0,1), col = 'green')
plot(allData$natural_all.x, allData$natural_all.y, xlab = 'Upstream: Natural Forests', ylab = 'Grid-level: Natural Forests')
abline(coef = c(0,1), col = 'green')
plot(allData$planted.x, allData$planted.y, xlab = 'Upstream: Planeted Forests', ylab = 'Grid-level: Planeted Forests')
abline(coef = c(0,1), col = 'green')
plot(allData$FFI2020, allData$FFI2020_med, xlab = 'Upstream: FFI', ylab = 'Grid-level: FFI')
abline(coef = c(0,1), col = 'green')


cor.test(allData$BHI_2010, allData$BHI_2010_bil)
cor.test(allData$natural_all.x, allData$natural_all.y)
cor.test(allData$planted.x, allData$planted.y)
cor.test(allData$FFI2020, allData$FFI2020_med)

binsreg(allData$z_score_mean, allData$for_pc_use, randcut=1)


parameter <- par(mfrow=c(1,1)) #set up the plotting space

#Tests: Create rasters and visualize output
out_fish_r = 
  rasterFromXYZ(allData[, c("Lon", "Lat","for_pc_sse")]) 

out_fish_r2 = 
  rasterFromXYZ(allData[, c("Lon", "Lat","natural_all.y")]) 

testRaster = 
  rasterFromXYZ(BII[, c("x", "y","BII_med")]) 

###################################
allData = 
  sm_precip %>%
  merge(hydroATLAS, by = c("Lon", "Lat"), all.x = T) %>%
  merge(BII, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(BHI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(FFI, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T) %>%
  merge(FM, by.x = c("Lon", "Lat"), by.y = c("x", "y"), all.x = T)


binsreg(allData$z_score_mean, allData$for_pc_use, randcut=1)


parameter <- par(mfrow=c(2,2)) #set up the plotting space

plot(allData$BHI_2010, allData$BHI_2010_bil, xlab = 'Upstream: BHI', ylab = 'Grid-level: BHI')
abline(coef = c(0,1), col = 'green')
plot(allData$natural_all.x, allData$natural_all.y, xlab = 'Upstream: Natural Forests', ylab = 'Grid-level: Natural Forests')
abline(coef = c(0,1), col = 'green')
plot(allData$planted.x, allData$planted.y, xlab = 'Upstream: Planeted Forests', ylab = 'Grid-level: Planeted Forests')
abline(coef = c(0,1), col = 'green')
plot(allData$FFI2020, allData$FFI2020_med, xlab = 'Upstream: FFI', ylab = 'Grid-level: FFI')
abline(coef = c(0,1), col = 'green')


(ggplot(allData, aes(x=natural_all.x,y=natural_all.y)) +
    stat_summary_bin(fun='mean', bins=40,
                     color='orange', size=2, geom='point'))


cor.test(allData$BHI_2010, allData$BHI_2010_bil)
cor.test(allData$natural_all.x, allData$natural_all.y)
cor.test(allData$planted.x, allData$planted.y)
cor.test(allData$FFI2020, allData$FFI2020_med)


