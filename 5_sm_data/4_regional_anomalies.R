############################################################
##Continental Drying
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)
library(ggnewscale)
library(spatialEco)
library(trend)

##############################
#Load the fishnet
fishnet_wb = 
  readRDS(paste0(database,'Fishnet_halfdegree/global_fishnet_fixed_wb_regions.rds'))

fishnet_wb_df = 
  fishnet_wb %>%
  st_drop_geometry() %>%
  as.data.table()

##############################
#Load World Regions

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

# #Get the centroids
# data_centroid = qgis_run_algorithm(
#   "native:centroids",
#   INPUT = fishnet)
# 
# fishnet_centroids = 
#   st_read(qgis_extract_output(data_centroid, "OUTPUT"))
# 
# #Do a spatial join on the centroids to link fishnet cell id
# qgis_search_algorithms(algorithm = "join")
# qgis_show_help("native:joinattributesbylocation")
# 
# qgis_join =
#   qgis_run_algorithm(
#     "native:joinattributesbylocation",
#     INPUT = fishnet_centroids,
#     JOIN = wb_regions,
#     PREDICATE = 5 #Use st_within
#   )
# 
# fishnet_centroid_wb_region =
#   st_read(qgis_extract_output(qgis_join, "OUTPUT"))
# 
# fishnet_wb_region = 
#   fishnet %>%
#   merge(fishnet_centroid_wb_region %>% st_drop_geometry() %>% select(OBJECTID, WB_NAME:REGION_WB),
#         by = 'OBJECTID', all.x = T)
# 
# saveRDS(fishnet_wb_region,
#          paste0(database,'Fishnet_halfdegree/global_fishnet_fixed_wb_regions.rds'))

##########################################################################################
#SM data

path2era = "/Users/tejasvi/Dropbox/Database/Hydrology/era5_land_soil_moisture/processed/"
path2Wang = "/Users/tejasvi/Dropbox/Database/Hydrology/Wang_2021_Soil_Moisture/processed/"

era5_sm = 
  fread(paste0(path2era, "ERA5_land_sm_data_05deg_1979_2024.csv")) %>%
  mutate(swvl_0_100_cm = swvl_0_100 * 100,
         swvl_0_289_cm = swvl_0_289 * 289) %>%
  merge(fishnet_wb_df[,c("Lon", "Lat", "WB_NAME", "REGION_WB")], 
        by.x = c("lon", "lat"), by.y = c("Lon", "Lat"),
        all.x = T) %>%
  drop_na

wang_sm = 
  fread(paste0(path2Wang, "sm_data_yearly_1970_2016.csv")) %>%
  mutate(sm_0_100_cm = sm_0_100 * 100) %>%
  merge(fishnet_wb_df[,c("Lon", "Lat", "WB_NAME", "REGION_WB")], 
        by.x = c("lon", "lat"), by.y = c("Lon", "Lat"),
        all.x = T) %>%
  drop_na 


regions = c("Global", unique(era5_sm$REGION_WB))


get_anomalies_era = function(df, region, start, end,
                             base_yr1, base_yr2){
  if(region != 'Global'){
    df = 
      df %>%
      filter(REGION_WB == region)
  }
  
  region_mean =
    df %>%
    #filter(year > 1978 & year < 2000) %>%
    filter(year > base_yr1 & year < base_yr2) %>%
    group_by(lat, lon) %>%
    summarise(global_mean_0_100_cm = mean(swvl_0_100_cm, na.rm = T),
              global_mean_0_289_cm = mean(swvl_0_289_cm, na.rm = T))
  
  df_sm_anomalies = 
    df %>%
    filter(year > start) %>%
    merge(region_mean, by = c("lat", "lon"), all.x = T) %>%
    mutate(swvl_0_100_cm_anomaly = swvl_0_100_cm - global_mean_0_100_cm,
           swvl_0_289_cm_anomaly = swvl_0_289_cm - global_mean_0_289_cm) %>%
    group_by(year) %>%
    summarise(swvl_0_100_cm_anomaly_mean = mean(swvl_0_100_cm_anomaly, na.rm = T),
              swvl_0_289_cm_anomaly_mean = mean(swvl_0_289_cm_anomaly, na.rm = T))
  
  
  # Plot
  ggplot(df_sm_anomalies, aes(x=year, y=swvl_0_289_cm_anomaly_mean)) +
    geom_area(fill = "navyblue", alpha = 0.6) +
    labs(
      title = paste0("ERA5 Land - ", region, " (", start, "-2024)"),
      x = "Year",
      y = "SM anomaly (cm)"
    ) + ylim(-5, 1) +
    theme_bw()
  
  print(zyp.yuepilon(df_sm_anomalies$swvl_0_289_cm_anomaly_mean))
} 

get_anomalies_era(era5_sm, regions[5], 
                  start = 1979, base_yr1 = 1979, base_yr2 = 2000)

get_anomalies_era(era5_sm, regions[5], 
                  start = 2002, base_yr1 = 2004, base_yr2 = 2009)


get_anomalies_wang = function(df, region){
  if(region != 'Global'){
    df = 
      df %>%
      filter(REGION_WB == region)
  }
  
  region_mean =
    df %>%
    filter(year > 1978 & year < 2000) %>%
    #filter(year > 2002) %>%
    group_by(lat, lon) %>%
    summarise(global_mean_0_100_cm = mean(sm_0_100_cm, na.rm = T))
  
  df_sm_anomalies = 
    df %>%
    merge(region_mean, by = c("lat", "lon"), all.x = T) %>%
    mutate(sm_0_100_cm_anomaly = sm_0_100_cm - global_mean_0_100_cm) %>%
    group_by(year) %>%
    summarise(sm_0_100_cm_anomaly_mean = mean(sm_0_100_cm_anomaly, na.rm = T))
  
  
  # Plot
  ggplot(df_sm_anomalies, aes(x=year, y=sm_0_100_cm_anomaly_mean)) +
    geom_area(fill = "brown4", alpha = 0.6) +
    labs(
      title = paste0("Wang et al - ", region, " (1970-2016)"),
      x = "Year",
      y = "SM anomaly (cm)"
    ) + ylim(-1, 0.5) +
    theme_bw()
} 

get_anomalies_era(era5_sm, regions[1])
get_anomalies_wang(wang_sm, regions[1])

get_anomalies_wang(wang_sm, regions[5]) 

era_global = 
  era5_sm %>%
  merge(region_mean, by = c("lat", "lon"), all.x = T) %>%
  mutate(swvl_0_100_cm_anomaly = swvl_0_100_cm - 0,
         swvl_0_289_cm_anomaly = swvl_0_289_cm - 0) %>%
  group_by(year) %>%
  summarise(swvl_0_100_cm_anomaly_mean = mean(swvl_0_100_cm_anomaly, na.rm = T),
            swvl_0_289_cm_anomaly_mean = mean(swvl_0_289_cm_anomaly, na.rm = T))
  
wang_global = 
  wang_sm %>%
  merge(region_mean, by = c("lat", "lon"), all.x = T) %>%
  mutate(sm_0_100_cm_anomaly = sm_0_100_cm - 0) %>%
  group_by(year) %>%
  summarise(sm_0_100_cm_anomaly_mean = mean(sm_0_100_cm_anomaly, na.rm = T))

# Plot
ggplot() +
  geom_line(data = wang_global, aes(x = year, y = sm_0_100_cm_anomaly_mean, color = "Wang et al.")) +
  geom_line(data = era_global, aes(x = year, y = swvl_0_100_cm_anomaly_mean, color = "ERA5-Land")) +
  scale_color_manual(
    name = "Data Source",
    values = c("Wang et al." = "brown4", "ERA5-Land" = "navyblue")
  ) +
  labs(
    title = "Global Mean SM",
    x = "Year",
    y = "SM Estimate (cm)"
  ) + ylim(20, 30) +
  theme_bw() 

###TWSA trends
path2data = paste0(database, 'Hydrology/grace_mascons/')
tws = 
  tws_mascon %>% #From twsa_trends_mascons.R 
  merge(fishnet_wb_df[,c("Lon", "Lat", "WB_NAME", "REGION_WB")], 
        by.x = c("lon", "lat"), by.y = c("Lon", "Lat"),
        all.x = T) %>%
  drop_na %>%
  filter(REGION_WB == regions[5]) %>%
  group_by(year, lat, lon) %>%
  summarise(twsa = mean(lwe_thickness_scaled, na.rm = T)) %>%
  group_by(year) %>%
  summarise(twsa = mean(twsa, na.rm = T))
  
# Plot
ggplot(tws, aes(x=as.numeric(year), y=twsa)) +
  geom_area(fill = "indianred", alpha = 0.6) +
  labs(
    title = paste0("GRACE TWSA - ", region),
    x = "Year",
    y = "TWSA anomaly (cm)"
  ) + 
  theme_bw()

print(zyp.yuepilon(df_sm_anomalies$swvl_0_289_cm_anomaly_mean))

