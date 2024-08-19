############################################################
##Process ISIMIP Data
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


source('0_environment/myfunctions.R')
source('4_scarcity_metrics/workflow_functions.R')

#paths
proj_dir = "~/Dropbox/WB/livable_planet/"
database = "~/Dropbox/Database/"
path2data = paste0(database, "ISIMIP/ISIMIP2b/Global_Hydrology/PCR_GLOBWB/GFDL_ESM2M/monthly/subset/")
path2data_alt = paste0(database, "Agriculture/Crop Water Requirements/Potential_ET/Liu_ETc_2022/")

##############################
#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

####Load World Regions
wb_regions = 
  st_read(paste0(database, "Admin/WB_Regions/WB_countries_Admin0_10m_QGIS_fixed.gpkg")) %>%
  #filter(TYPE != 'Dependency') %>%
  dplyr::select(WB_NAME, WB_A3, ISO_A3, ISO_N3, TYPE, REGION_WB) %>%
  mutate(rownumber = row_number()) %>% #Need to create custom ID; otherwise some duplicates due to random colonial territories
  mutate(ISO_N3 = as.numeric(ISO_N3))

wb_regions_non_spatial = 
  wb_regions %>% st_drop_geometry() %>% as.data.table()

#Use the ISO_N3 to create a raster
wb_regions_raster = 
  fasterize(wb_regions, fishnet.r %>% raster, "rownumber") %>% rast

##########################################################################################
#ISIMIP data

month_day = data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

ds1 = 
  tidync(paste0(path2data, 'merged_all.nc4')) %>%
  hyper_tibble() %>% as.data.table() 

#Create a date sequence
date.seq = 
  seq(as.Date("1981/1/1"), as.Date("2005/12/30"), "month") %>%
  as.data.table() %>%
  rownames_to_column() %>%
  .[, rowname := as.integer(rowname)]
colnames(date.seq) = c('rowname', 'date')

#Get the times from the netCDF
netcdf_time = sort(unique(ds1$time))

#Not ideal - but cbind the netcdf_time to actual datetime
date.seq = 
  date.seq %>%
  mutate(yearmon = substr(date, 1, 7),
         year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6, 7))) %>%
  merge(month_day, by = 'month') %>%
  arrange(rowname) %>% 
  mutate(netcdf_time = netcdf_time)

#Merge the dates using the rowname column
ds1 = 
  ds1 %>%
  merge(date.seq, by.x = 'time', by.y = 'netcdf_time', all.x = T) %>%
  .[, cell_id := paste0(lat, lon)]

##########################################################################################
#Convert to spatial object for:
#(a) linking country codes; (b) getting cell areas to calculate volumes

ref_raster = 
  rasterFromXYZ(ds1_transform[,.(lon, lat, 1)]) %>% rast

crs(ref_raster) = crs(fishnet.r)

ref_raster = terra::resample(ref_raster, fishnet.r)

ref_raster_df = 
  c(ref_raster, cellSize(ref_raster, unit = 'm'), wb_regions_raster) %>% #Combine rasters into one (area + area code)
  terra::as.data.frame(xy=T, cells=FALSE) %>%
  as.data.table() %>%
  dplyr::rename(lon = x, lat = y, rownumber = layer) %>%
  merge(wb_regions %>% st_drop_geometry() %>% dplyr::select(WB_NAME, ISO_A3, WB_A3, rownumber, REGION_WB),
        by = 'rownumber', all.x = T) %>%
  drop_na()

##########################################################################################
#Potential Crop ET data
#Source: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021EF002567

ds2 = 
  tidync(paste0(path2data_alt, 'Allcrops_GSWP3_hist_ETc_volume_monthly_1981_2005.nc4')) %>%
  hyper_tibble() %>% as.data.table() 

#Create a date sequence
date.seq = 
  seq(as.Date("1981/1/1"), as.Date("2005/12/30"), "month") %>%
  as.data.table() %>%
  rownames_to_column() %>%
  .[, rowname := as.integer(rowname)]
colnames(date.seq) = c('rowname', 'date')

#Get the times from the netCDF
netcdf_time = sort(unique(ds2$time))

#Not ideal - but cbind the netcdf_time to actual datetime
date.seq = 
  date.seq %>%
  mutate(yearmon = substr(date, 1, 7),
         year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6, 7))) %>%
  merge(month_day, by = 'month') %>%
  arrange(rowname) %>%
  mutate(netcdf_time = netcdf_time)


#Merge the dates using the rowname column
ds2 = 
  ds2 %>%
  merge(date.seq, by.x = 'time', by.y = 'netcdf_time', all.x = T) %>%
  .[, cell_id := paste0(lat, lon)]

#Reference for unit conversion: https://www.researchgate.net/post/How-to-convert-cmip5-monthly-precipitation-kg-m2-s1-into-mm-month
ds2_transform = 
  ds2 %>%
  as.data.table() %>%
  filter(ETc != 0)#filter(ETc_m3_year != 0)

#Checks
#check_raster = rasterFromXYZ(ds2_transform[,.(lon, lat, ETc_m3_year/10^9)])
#plot(check_raster)

##########################################################################################
#Water Footprint Data (Hoekstra)
####
path2WF = paste0(database, 'WaterManagement/Water_Footprint_2011_Hoekstra',
                 '/processed/National_WF_1996_2005.csv')
WF_data = 
  fread(path2WF) %>%
  dplyr::select(WB_NAME, WF_Green_Total:WF_Blue_Total) %>%
  mutate(across(WF_Green_Total:WF_Blue_Total, ~ .x*(10^6), .names = "{.col}_m3_year")) %>%
  dplyr::select(-WF_Green_Total, -WF_Blue_Total)

# matches = WF_data %>% filter(Country_WB %in% wb_regions$WB_NAME)
# non_matches = WF_data %>% filter(!Country_WB %in% wb_regions$WB_NAME)

##########################################################################################
#Cropland Data
path2data = paste0(database, 'Agriculture/Cropped Area/Potapov_cropland_extent_2022/')

#Load the irrigated area map by Meier et al. (2018)
rast_crop = 
  rast(paste0(path2data, 'Global_cropland_3km_2019.tif')) %>%
  terra::project(fishnet.r, 'average')

#Create layers for 3 thresholds -> >0, >1%, >10% 
crop_thresholds = 
  c(ifel(rast_crop > 5, 1, 0), 
    ifel(rast_crop >= 10, 1, 0), 
    ifel(rast_crop >= 20, 1, 0))

#In percentages
names(crop_thresholds) = c('cropland_5', 'cropland_10', 'cropland_20')

#Convert to dataframe
df_crop =
  as.data.frame(crop_thresholds, xy = T) %>%
  dplyr::rename(lon = x, lat = y) %>%
  as.data.table()

##########################################################################################
#Estimate Scarcity

withdrawal_based = T
#Transform PCR-GLOBWB data
ds1_transform = pre_process_PCR_GLOBWB(ds1, withdrawal_based)

# #Checks
# check_raster = rasterFromXYZ(ds1_transform[,.(lon, lat, gwa_m_month)])
# plot(check_raster, zlim = c(0,1))

ds_pixel_year = pixel_level_scarcity(ds1_transform, withdrawal_based = withdrawal_based)

ds_country_year = country_level_scarcity(ds_pixel_year, withdrawal_based = withdrawal_based,
                                         cropland_threshold = F)

###############
#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_level_metrics_1981_2005.csv'))
#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_scarcity_metrics_CONSUMPTION_1981_2005.csv'))
#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_scarcity_metrics_WITHDRAWAL_1981_2005.csv'))

#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_scarcity_metrics_CONSUMPTION_allPixels_1981_2005.csv'))
#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_scarcity_metrics_WITHDRAWAL_allPixels_1981_2005.csv'))


#test = fread(paste0(proj_dir, 'scarcity_outputs/country_level_metrics_1981_2005.csv'))
##############

#Checks
ds_for_raster = 
  ds_pixel_year %>%
  filter(year == 2005) #%>% filter(cropland_5 == 1)

ds_for_country =
  ds_country_year %>%
  filter(year == 2005) %>%
  filter(WB_NAME == 'India') 
  # mutate(ratio_BW_WF = actual_water_consumed_m3_year/WF_Blue_Total_m3_year,
  #        ratio_GW_WF = gwa_m3_year/WF_Green_Total_m3_year) %>%
  #dplyr::select(WB_NAME, ratio_GW_WF)

ds_all_sub =
  ds_country_year %>%
  filter(year > 2000) %>%
  #filter(WB_NAME == 'India') 
  group_by(WB_NAME, ISO_A3) %>%
  summarise(across(gws_monthly_aggregate:bws_area_scarce, ~median(.x, na.rm=T))) %>%
  as.data.table()
# mutate(ratio_BW_WF = actual_water_consumed_m3_year/WF_Blue_Total_m3_year,
#        ratio_GW_WF = gwa_m3_year/WF_Green_Total_m3_year) %>%
#dplyr::select(WB_NAME, ratio_GW_WF)

check_raster = rasterFromXYZ(ds_for_raster[,.(lon, lat, gws_pixel_year)])
plot(check_raster, col = map.pal("viridis"), main = 'Green Water Scarcity',
     zlim = c(0.1,1))  

#Hoekstra vs PCR-GLOBWB
plot(log(ds_all_sub$WF_Blue_Total_m3_year), log(ds_all_sub$actual_water_consumed_m3_year),
     xlab = 'Blue Water Footprint (m3/yr; Hoekstra)', xlim = c(13,30),
     ylab = 'Actual Water Consumed (m3/yr); PCR-GLOBWB)', ylim = c(13,30),
     main = 'Blue Water Consumption comparison (log-scale)')
abline(coef = c(0,1))


plot(log(ds_all_sub$WF_Green_Total_m3_year), log(ds_all_sub$gwa_m3_year),
     xlab = 'Green Water Footprint (m3/yr; Hoekstra)', xlim = c(13,30),
     ylab = 'Actual Green ET (m3/yr; PCR-GLOBWB)', ylim = c(13,30),
     main = 'Green Water Consumption comparison (log-scale)')
abline(coef = c(0,1))

plot(log(ds_all_sub$gws_year_country), log(ds_all_sub$bws_year_country),
     xlab = 'Log - Green Water Stress (option 4)',
     ylab = 'Log - Blue Water Stress (option 4)',
     main = 'BWS vs GWS (log-scale)')
abline(coef = c(0,1))

plot((ds_all_sub$gws_pixel_year), (ds_all_sub$bws_pixel_year),
     xlab = 'Green Water Scarcity (option 3)',
     ylab = 'Blue Water Scarcity (option 3)', ylim = c(0,0.3),
     main = 'BWS vs GWS')
#abline(coef = c(0,1))

hist(ds_for_country$gws_area_scare, 
     main = 'Proportion of Greenwater Scarce Area',  ylim = c(0,40),
     ylab = 'Number of Countries', xlab = 'Proportion of greenwater scarce area')

hist(ds_for_country$gws_year_country, 
     main = 'Greenwater Scarcity (option 4)', 
     ylab = 'Number of Countries', xlab = 'Greenwater Scarcity')

##########################################################################################
#Estimate Rosa/Liu metrics
#first by month; then average over entire time-period
ds_all = 
  ds1_transform %>%
  filter(year>2000) %>% #Subset to the recent 5 years
  merge(ref_raster_df, by = c('lon', 'lat'), all.x=T) %>%
  merge(ds2_transform, by = c('lon', 'lat', "year", "month"), all.x=T) %>%
  #drop_na %>%
  dplyr::select(lon, lat, area:REGION_WB, airruse, adomuse, ainduse, aliveuse, pirruse, bwa_m_month, gwa_m_month, ETc) %>%
  mutate(across(airruse:pirruse, ~ .x*area, .names = "{.col}_m3_month"),
         bwa_m3_month = bwa_m_month*area,
         gwa_m3_month = gwa_m_month*area) %>%
  mutate(gws_rosa = pirruse_m3_month/ETc, #Potential Irrigation Water Consumption to Potential Water Demands
         bws_rosa = (airruse_m3_month+adomuse_m3_month+ainduse_m3_month+aliveuse_m3_month)/bwa_m3_month, #Irrigation Water Consumption to Blue Water Availability
         bws_liu = ETc/bwa_m3_month,
         aws_liu = ETc/(bwa_m3_month+gwa_m3_month)) %>%
  group_by(lon, lat, WB_NAME) %>%
  summarise(across(gws_rosa:bws_liu, ~ mean(.x, na.rm = T))) %>%
  mutate(gws_rosa_bin = ifelse(gws_rosa > 0.1, 1, 0),
         bws_rosa_trunc = ifelse(bws_rosa > 5, 5, bws_rosa),
         bws_liu_trunc = ifelse(bws_liu > 2, 2, bws_liu)) %>%
  as.data.table()
  
  

#Checks
check_raster = rasterFromXYZ(ds_all[,.(lon, lat, gws_rosa_bin)])
plot(check_raster, col = map.pal("viridis"), main = 'Green Water Scarce Regions - Rosa')

check_raster = rasterFromXYZ(ds_all[,.(lon, lat, bws_rosa_trunc)])
plot(check_raster, col = map.pal("viridis"), main = 'Blue Water Scarce Regions - Rosa')

check_raster = rasterFromXYZ(ds_all[,.(lon, lat, bws_liu_trunc)])
plot(check_raster, col = map.pal("viridis"), main = 'Blue Water Scarce Regions - Liu', zlim = c(0, 2.1))
