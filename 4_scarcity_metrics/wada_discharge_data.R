############################################################
##Processing Wada Discharge data
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)
##########################################################################################
#Input data

path2data = paste0(database, 'WaterManagement/Water_Scarcity_Wada_WB_2024/discharge_data/')

years = data.frame(Year = 1980:2022,
                   Time = 1:43)
ds1 = 
  tidync(paste0(path2data, 'discharge_lpjml_1980_2022.nc')) %>%
  hyper_tibble() %>% as.data.table() %>%
  merge(years, by = 'Time', all.x = T) %>%
  dplyr::select(-Time) %>%
  dplyr::relocate(data, .after = Year) %>%
  dplyr::rename(Discharge_lpjml_km3yr = data)

ds2 = 
  tidync(paste0(path2data, 'discharge_pcrglobe_1980_2022.nc')) %>%
  hyper_tibble() %>% as.data.table() %>%
  merge(years, by = 'Time', all.x = T) %>%
  dplyr::select(-Time) %>%
  dplyr::relocate(data, .after = Year) %>%
  dplyr::rename(Discharge_pcrglobe_km3yr = data)

##########################################################################################
#Convert to spatial object for:
#(a) linking country codes; (b) getting cell areas to calculate volumes

ref_raster_df = 
  wb_regions_raster %>%
  terra::as.data.frame(xy=T, cells=FALSE) %>%
  as.data.table() %>%
  dplyr::rename(Lon = x, Lat = y, rownumber = layer) %>%
  merge(wb_regions %>% st_drop_geometry() %>% dplyr::select(WB_NAME, ISO_A3, rownumber, REGION_WB),
        by = 'rownumber', all.x = T) %>%
  drop_na()

##########################################################################################
ds_all = 
  ds1 %>%
  merge(ds2, by = c('Lat', 'Lon', 'Year')) %>%
  merge(ref_raster_df, by = c('Lon', 'Lat'), all.x = T) %>%
  drop_na()

ds_country_level = 
  ds_all %>%
  group_by(WB_NAME, ISO_A3, Year) %>%
  summarise(Discharge_lpjml_km3yr = sum(Discharge_lpjml_km3yr, na.rm = T))

##########################################################################################
#Tests

ds_test = ds1 %>% filter(Year == 1981)

ds_global_level = 
  ds_all %>%
  group_by(Year) %>%
  summarise(Discharge_lpjml_km3yr = sum(Discharge_lpjml_km3yr, na.rm = T))

##########################################################################################
fwrite(ds_all, paste0(path2data, "processed/discharge_1980_2022_05deg.csv"))
fwrite(ds_country_level, paste0(path2data, "processed/discharge_1980_2022_country.csv"))


