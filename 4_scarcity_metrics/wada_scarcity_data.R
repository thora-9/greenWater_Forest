############################################################
##Processing Wada Scarcity data
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)
##########################################################################################
#Input data

path2data = paste0(database, 'WaterManagement/Water_Scarcity_Wada_WB_2024/scarcity_data/')

years = data.frame(Year = 1980:2022,
                   Time = 1:43)
ds1 = 
  tidync(paste0(path2data, 'waterscarcity_EFRQ90_Q50_lpjml_1980_2022.nc')) %>%
  hyper_tibble() %>% as.data.table() %>%
  merge(years, by = 'Time', all.x = T) %>%
  dplyr::select(-Time) %>%
  dplyr::relocate(data, .after = Year) %>%
  dplyr::rename(Water_Scarcity_lpjml = data)

ds2 = 
  tidync(paste0(path2data, 'waterscarcity_EFRQ90_Q50_pcrglb_1980_2022.nc')) %>%
  hyper_tibble() %>% as.data.table() %>%
  merge(years, by = 'Time', all.x = T) %>%
  dplyr::select(-Time) %>%
  dplyr::relocate(data, .after = Year) %>%
  dplyr::rename(Water_Scarcity_pcrglobe = data)


##########################################################################################
#Convert to spatial object for:
#(a) linking country codes; (b) getting cell areas to calculate volumes

ref_raster_df = 
  c(wb_regions_raster, cellSize(wb_regions_raster, unit = 'm')) %>% #Combine rasters into one (area + area code)
  terra::as.data.frame(xy=T, cells=FALSE) %>%
  as.data.table() %>%
  drop_na() %>%
  dplyr::rename(Lon = x, Lat = y, rownumber = layer) %>%
  merge(wb_regions %>% st_drop_geometry() %>% dplyr::select(WB_NAME, ISO_A3, rownumber, REGION_WB),
        by = 'rownumber', all.x = T) %>%
  drop_na()

##########################################################################################
path2pop = paste0(database, "SocioEconomic/Population/ghs_pop/")
population = 
  fread(paste0(path2pop, "processed/population_1980_2020_05.csv"))

# source('4_scarcity_metrics/workflow_functions.R')
# ds_isimip = readRDS(paste0(database, "ISIMIP/ISIMIP2b/Global_Hydrology/PCR_GLOBWB/GFDL_ESM2M/processed/all_vars_1981_2005_monthly.rds"))
# 
# withdrawal_based = F
# #Transform PCR-GLOBWB data
# ds_isimip_transform = pre_process_PCR_GLOBWB(ds_isimip, withdrawal_based)
# 
# ds_pixel_year = blue_water_scarcity(ds_isimip_transform, withdrawal_based)

##########################################################################################
ds_all = 
  ds1 %>%
  merge(ds2, by = c('Lat', 'Lon', 'Year')) %>%
  merge(ref_raster_df, by = c('Lon', 'Lat'), all.x = T) %>%
  merge(population, by.x = c('Lon', 'Lat'), by.y = c('x', 'y'), all.x = T) %>%
  drop_na()

ds_country_level = 
  ds_all %>%
  group_by(WB_NAME, ISO_A3, Year) %>%
  summarise(Water_Scarcity_lpjml = mean(Water_Scarcity_lpjml, na.rm = T))

##########################################################################################
#Tests

#TH isimip data map
ds_for_raster = 
  ds_pixel_year %>%
  filter(year == 2005) %>%
  mutate(bws_pixel = ifelse(bws_pixel>1, 1, bws_pixel),
         bws_pixel_year = ifelse(bws_pixel_year>1, 1, bws_pixel_year))

check_raster = rasterFromXYZ(ds_for_raster[,.(lon, lat, bws_pixel_year)])
plot(check_raster, col = map.pal("plasma"), main = 'Blue Water Scarcity - 2005',
     zlim = c(0,1))  

#Get population in stressed regions
pop_stressed = 
  ds_all %>%
  filter(Year == 2022) %>%
  mutate(stress_cat = cut(Water_Scarcity_pcrglobe, 
                          breaks = c(-1, 0.2, 0.4, 0.8, 2),
                          labels = c('< 0.2', '0.2 - 0.4', '0.4 - 0.8', '> 0.8'))) %>%
  group_by(stress_cat) %>%
  summarise(Pop_2020 = sum(Pop_2020)) %>%
  mutate(Pop_2020 = prop.table(Pop_2020))

plot_rast_year = function(year){
  cur_rast = 
    ds2 %>% 
    filter(Year == year) %>%
    dplyr::select(1, 2, 4) %>%
    rasterFromXYZ()
  
  rc = cut(cur_rast, c(-0.1,0.1,0.2,0.4,0.8,1.1))
  plot(rc, col = rev(brewer.pal(5, 'RdYlBu')), main = paste0('Water Scarcity - ', year))
  #plot(cur_rast, main = paste0('Water Scarcity - ', year))
  
}

plot_rast_year(1981)
plot_rast_year(2022)



##########################################################################################
#fwrite(ds_all, paste0(path2data, "processed/water_scarcity_EFR_1980_2022_05deg.csv"))

terra::writeRaster(cur_rast,
                   paste0(path2data, "processed/scarcity_2022_lpj2.tif"),
                   overwrite = T)

##########################################################################################
path2data = paste0(database, 'WaterManagement/Water_Scarcity_Wada_WB_2024/scarcity_data/')

ds_LPJml =
  fread(paste0(path2data, "processed/water_scarcity_EFR_1980_2022_05deg.csv")) %>%
  filter(Year == 2022) %>%
  dplyr::select(1, 2, Water_Scarcity_lpjml) %>%
  rasterFromXYZ() %>% rast()

ds_pcrglobwb =
  fread(paste0(path2data, "processed/water_scarcity_EFR_1980_2022_05deg.csv")) %>%
  filter(Year == 2022) %>%
  dplyr::select(1, 2, Water_Scarcity_pcrglobe) %>%
  rasterFromXYZ() %>% rast()


########################################################################################################################
#Aggregate to admin-2
path2admin = paste0(database, 'Admin/GAUL/')

admin2 = 
  st_read(paste0(path2admin, 'g2015_2014_2.shp'), crs = 4326) %>%
  terra::extract(ds_LPJml, ., fun=mean, bind = T, na.rm = T) %>%
  terra::extract(ds_pcrglobwb, ., fun=mean, bind = T, na.rm = T) %>%
  as_sf()

admin2_df = 
  admin2%>% 
  st_drop_geometry() %>% as.data.table() 

fwrite(admin2_df, paste0(path2data, "processed/water_scarcity_admin2_2022.csv"))

plot1 = 
  admin2 %>%
  filter(!Water_Scarcity_lpjml %in% c(NaN, NA)) %>%
  mutate(ws_cat = cut(Water_Scarcity_lpjml, breaks = c(0,0.1,0.2,0.4,0.8,1))) %>%
  ggplot() + 
  geom_sf(aes(fill = ws_cat), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=rev(c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049')), name = "Water Stress - LPJmL") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2data, 'processed/water_stress_lpj', '.png'), plot=plot1,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')


plot2 = 
  admin2 %>%
  filter(!Water_Scarcity_pcrglobe %in% c(NaN, NA)) %>%
  mutate(ws_cat = cut(Water_Scarcity_pcrglobe, breaks = c(0,0.1,0.2,0.4,0.8,1))) %>%
  ggplot() + 
  geom_sf(aes(fill = ws_cat), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=rev(c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049')), name = "Water Stress - PCR-GLOBWB") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2data, 'processed/water_stress_pcr', '.png'), plot=plot2,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')

########################################################################################################################
#Aggregate to admin-0
path2admin = paste0(database, 'Admin/GAUL/')

admin0 = 
  wb_regions %>%
  terra::extract(ds_LPJml, ., fun=mean, bind = T, na.rm = T) %>%
  terra::extract(ds_pcrglobwb, ., fun=mean, bind = T, na.rm = T) %>%
  as_sf()

admin0_df = 
  admin0 %>% 
  st_drop_geometry() %>% as.data.table() 

fwrite(admin0_df, paste0(path2data, "processed/water_scarcity_admin0_2022.csv"))

plot1 = 
  admin0 %>%
  filter(!Water_Scarcity_lpjml %in% c(NaN, NA)) %>%
  mutate(ws_cat = cut(Water_Scarcity_lpjml, breaks = c(0,0.1,0.2,0.4,0.8,1))) %>%
  ggplot() + 
  geom_sf(aes(fill = ws_cat), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=rev(c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049')), name = "Water Stress - LPJmL") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2data, 'processed/water_stress_lpj_admin0', '.png'), plot=plot1,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')


plot2 = 
  admin0 %>%
  filter(!Water_Scarcity_pcrglobe %in% c(NaN, NA)) %>%
  mutate(ws_cat = cut(Water_Scarcity_pcrglobe, breaks = c(0,0.1,0.2,0.4,0.8,1))) %>%
  ggplot() + 
  geom_sf(aes(fill = ws_cat), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=rev(c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049')), name = "Water Stress - PCR-GLOBWB") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2data, 'processed/water_stress_pcr_admin0', '.png'), plot=plot2,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')

