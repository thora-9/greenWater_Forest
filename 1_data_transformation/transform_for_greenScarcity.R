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


source('myfunctions.R')

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
  filter(TYPE != 'Dependency') %>%
  dplyr::select(WB_NAME, ISO_A2, ISO_A3, ISO_N3, TYPE, REGION_WB) %>%
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

#Reference for unit conversion: https://www.researchgate.net/post/How-to-convert-cmip5-monthly-precipitation-kg-m2-s1-into-mm-month
env_factor = 0.4 #proportion of runoff left for environmental demands

#Yearly
# ds1_transform = 
#   ds1 %>%
#   dplyr::select(lon:cell_id, qs, qsb, airruse, airrusegreen, arainfusegreen, pirrww, pirruse, airrww) %>%
#   filter(year>2000) %>% #Subset to the recent 5 years
#   mutate(across(qs:airrww, ~ .x*86400)) %>% #(kg m-2 s-1) to (kg m-2 day-1)
#   mutate(across(qs:airrww, ~ .x*(days/1000))) %>% #(kg m-2 day-1) to (kg m-2 month-1) # kg m-2 == (1/1000) m 
#   group_by(cell_id, lon, lat, year) %>% #Convert from monthly to yearly by taking the sum
#   summarise(across(qs:airrww, ~ sum(.x, na.rm = T))) %>%
#   group_by(cell_id, lon, lat) %>% #Average across yearly
#   summarise(across(qs:airrww, ~ mean(.x, na.rm = T))) %>%
#   mutate(bwa_m_year = (1-env_factor)*(qs + qsb),
#          gwa_m_year = airrusegreen + arainfusegreen) %>%
#   as.data.table()

#Monthly
ds1_transform = 
  ds1 %>%
  dplyr::select(lon:cell_id, qs, qsb, adomuse:pirrww) %>%
  #filter(year>2000) %>% #Subset to the recent 5 years
  mutate(across(qs:pirrww, ~ .x*86400)) %>% #(kg m-2 s-1) to (kg m-2 day-1)
  mutate(across(qs:pirrww, ~ .x*(days/1000))) %>% #(kg m-2 day-1) to (kg m-2 month-1) # kg m-2 == (1/1000) m 
  # group_by(cell_id, lon, lat, year) %>% #Convert from monthly to yearly by taking the sum
  # summarise(across(qs:airrww, ~ sum(.x, na.rm = T))) %>%
  # group_by(cell_id, lon, lat) %>% #Average across yearly
  # summarise(across(qs:airrww, ~ mean(.x, na.rm = T))) %>%
  mutate(bwa_m_month = (1-env_factor)*(qs + qsb),
         gwa_m_month = airrusegreen + arainfusegreen) %>%
  as.data.table()

#Checks
check_raster = rasterFromXYZ(ds1_transform[,.(lon, lat, gwa_m_month)])
plot(check_raster, zlim = c(0,1))

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
  merge(wb_regions %>% st_drop_geometry() %>% dplyr::select(WB_NAME, ISO_A3, rownumber, REGION_WB),
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
  #filter(year>2000) %>% #Subset to the recent 5 years
  # group_by(cell_id, lon, lat, year) %>% #Convert from monthly to yearly by taking the sum
  # summarise(ETc_m3_year = sum(ETc, na.rm = T)) %>%
  # group_by(cell_id, lon, lat) %>% #Average across yearly
  # summarise(ETc_m3_year = mean(ETc_m3_year, na.rm = T)) %>%
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
#Merge all

# ds_all = 
#   ds1_transform %>%
#   merge(ref_raster_df, by = c('lon', 'lat'), all.x=T) %>%
#   merge(ds2_transform, by = c('lon', 'lat'), all.x=T) %>%
#   drop_na %>%
#   dplyr::select(lon, lat, area:REGION_WB, airruse, pirruse, bwa_m_year, gwa_m_year, ETc_m3_year) %>%
#   mutate(airruse_m3_year = airruse*area,
#          pirruse_m3_year = pirruse*area,
#          bwa_m3_year = bwa_m_year*area,
#          gwa_m3_year = gwa_m_year*area) %>%
#   mutate(gws_rosa = pirruse_m3_year/ETc_m3_year, #Potential Irrigation Water Consumption to Potential Water Demands
#          bws_rosa = airruse_m3_year/bwa_m3_year, #Irrigation Water Consumption to Blue Water Availability
#          gws_liu = ETc_m3_year/gwa_m3_year,
#          bws_liu = ETc_m3_year/(bwa_m3_year+gwa_m3_year)) %>%
#   mutate(gws_rosa_bin = ifelse(gws_rosa > 0.1, 1, 0),
#          bws_rosa_bin = ifelse(bws_rosa > 1, 1, 0),
#          bws_liu_trunc = ifelse(bws_liu > 2, 2, bws_liu))

#Get yearly water metrics by pixel by year
ds_pixel_year = 
  ds1_transform %>%
  merge(ref_raster_df, by = c('lon', 'lat'), all.x=T) %>%
  merge(ds2_transform, by = c('lon', 'lat', "year", "month"), all.x=T) %>%
  dplyr::select(lon, lat, area:REGION_WB, year, month, gwa_m_month, bwa_m_month, airrww, aindww, aliveww, adomww,
                airruse, ainduse, aliveuse, adomuse, pirruse, ETc) %>%
  mutate(across(airrww:pirruse, ~ .x*area, .names = "{.col}_m3"),
         ETc_m3 = ETc,
         gwa_m3 = gwa_m_month*area,
         bwa_m3 = bwa_m_month*area) %>%
  mutate(actual_water_withdrawal_m3 = airrww_m3+aindww_m3+aliveww_m3+adomww_m3,
         actual_water_consumed_m3 = airruse_m3+ainduse_m3+aliveuse_m3+adomuse_m3) %>%
  #Calculate scarcity at the monthly-scale
  mutate(gws_pixel = pirruse_m3/ETc_m3, #Potential Irrigation Water Consumption to Potential Water Demands
         bws_pixel = actual_water_consumed_m3/bwa_m3 #Irrigation Water Consumption to Blue Water Availability
  ) %>%
  mutate(gws_pixel_bin = ifelse(gws_pixel > 0.1, 1, 0), #Estimate the monthly stressed regions
         bws_pixel_bin = ifelse(bws_pixel > 1, 1, 0)) %>%
  group_by(lon, lat, WB_NAME, ISO_A3, year) %>% #Convert from monthly to yearly by taking the sum
  summarise(across(airrww_m3:actual_water_consumed_m3, ~ sum(.x, na.rm = T), .names = "{.col}_year"),
            across(gws_pixel:bws_pixel, ~median(.x, na.rm = T), .names = "{.col}_mean"),
            across(gws_pixel_bin:bws_pixel_bin, ~ sum(.x, na.rm = T), .names = "{.col}_year")) %>%
  #Calculate scarcity at the yearlu-scale
  mutate(gws_pixel_year = pirruse_m3_year/ETc_m3_year, 
         bws_pixel_year = actual_water_consumed_m3_year/bwa_m3_year) %>%
  as.data.table() %>%
  filter(!is.na(WB_NAME))

#Get yearly water metrics by country by year
ds_country_year = 
  ds_pixel_year %>%
  group_by(year, WB_NAME, ISO_A3) %>%
  summarise(across(airrww_m3_year:actual_water_consumed_m3_year, ~ sum(.x, na.rm = T)),
            across(gws_pixel_mean:bws_pixel_year, ~median(.x, na.rm = T), .names = "{.col}_median")) %>%
  mutate(gws_year_country = pirruse_m3_year/ETc_m3_year, #Potential Irrigation Water Consumption to Potential Water Demands
         bws_year_country = actual_water_consumed_m3_year/bwa_m3_year #Irrigation Water Consumption to Blue Water Availability
  ) %>%
  merge(WF_data, by = 'WB_NAME', all.x = T) %>%
  as.data.table() %>%
  dplyr::select(year, WB_NAME, ISO_A3, gws_pixel_mean_median:bws_year_country) %>%
  dplyr::rename(gws_monthly_aggregate = gws_pixel_mean_median, bws_monthly_aggregate = bws_pixel_mean_median,
                gws_monthly_binary = gws_pixel_bin_year_median, gws_monthly_binary = gws_pixel_bin_year_median,
                gws_pixel_year = gws_pixel_year_median, bws_pixel_year = bws_pixel_year_median) %>%
  arrange(WB_NAME, year)

###############
#fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_level_metrics_1981_2005.csv'))
fwrite(ds_country_year, paste0(proj_dir, 'scarcity_outputs/country_scarcity_metrics_1981_2005.csv'))
##############

#Checks
ds_all_sub = 
  ds_pixel_year %>%
  filter(year == 2005) 

ds_all_sub =
  ds_country_year %>%
  #filter(year == 2005) %>%
  filter(WB_NAME == 'India') 
  # mutate(ratio_BW_WF = actual_water_consumed_m3_year/WF_Blue_Total_m3_year,
  #        ratio_GW_WF = gwa_m3_year/WF_Green_Total_m3_year) %>%
  #dplyr::select(WB_NAME, ratio_GW_WF)

check_raster = rasterFromXYZ(ds_all_sub[,.(lon, lat, actual_water_withdrawal_m3_year)])
plot(check_raster, main = 'Green Water Scarce Regions')  

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
