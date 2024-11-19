############################################################
##Processing Aquaduct 4.0 data
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)

##########################################################################################
#Input data

path2data = paste0(database, 'WaterManagement/Aqueduct_v4_2023/Aqueduct40_waterrisk_download_Y2023M07D05/')

layers <- st_layers(dsn = paste0(path2data, 'GDB/Aq40_Y2023D07M05.gdb'))

aquaduct_bs_annual = 
  st_read(paste0(path2data, 'GDB/Aq40_Y2023D07M05.gdb'), layer = 'baseline_annual')

aquaduct_bs_annual_stripped = 
  aquaduct_bs_annual %>%
  dplyr::select(1:12)

aquaduct_bs_annual_df = 
  aquaduct_bs_annual %>%
  st_drop_geometry() %>%
  as.data.table()

aquaduct_future_annual = 
  st_read(paste0(path2data, 'GDB/Aq40_Y2023D07M05.gdb'), layer = 'future_annual') %>%
  dplyr::select(pfaf_id, contains("ws_x"))

aquaduct_future_annual_df = 
  aquaduct_future_annual %>%
  st_drop_geometry() %>%
  as.data.table()
  
########################################################################################################################
#Aggregate to admin-2

path2admin = paste0(database, 'Admin/GAUL/')

admin2 = 
  st_read(paste0(path2admin, 'g2015_2014_2.shp'), crs = 4326)

admin2_df = 
  admin2 %>% st_drop_geometry() %>% as.data.table()

#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "centroid")
qgis_show_help("native:centroids")

qgis_centroid = 
  qgis_run_algorithm(
    "native:centroids",
    INPUT = admin2,
  )

admin2_centroid = 
  st_read(qgis_extract_output(qgis_centroid, "OUTPUT"))

#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "join")
qgis_show_help("native:joinattributesbylocation")

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = admin2_centroid,
    JOIN = aquaduct_bs_annual_stripped,
    PREDICATE = 5 #Use st_within
  )

join_output_df = 
  st_read(qgis_extract_output(qgis_join, "OUTPUT")) %>%
  dplyr::select(ADM2_CODE, string_id) %>% 
  st_drop_geometry() %>% as.data.table() 


admin2_linked_aquaduct = 
  admin2 %>%
  merge(join_output_df, by = 'ADM2_CODE', all.x = T) %>%
  merge(aquaduct_bs_annual_df, by = 'string_id', all.x = T) %>%
  merge(aquaduct_future_annual_df, by = 'pfaf_id', all.x = T) %>%
  dplyr::select(string_id:bws_label, bau30_ws_x_r:pes80_ws_x_l) %>%
  dplyr::select(1:3, 6:7, 10:11, 17, 22:24, 26:29, bau30_ws_x_r:pes80_ws_x_l)

admin2_linked_aquaduct_df = 
  admin2_linked_aquaduct %>% 
  st_drop_geometry() %>% as.data.table() 


##########################################################################################
path2out = paste0(database, 'WaterManagement/Aqueduct_v4_2023/')

fwrite(admin2_linked_aquaduct_df, 
       paste0(path2out, "processed/water_stress_admin2.csv"))

st_write(admin2_linked_aquaduct, 
         paste0(path2out, "processed/water_stress_admin2.gpkg"), delete_layer = T)

##########################################################################################
path2out = paste0(database, 'WaterManagement/Aqueduct_v4_2023/')

admin2_linked_aquaduct =
  st_read(paste0(path2out, "processed/water_stress_admin2.gpkg")) 


plot1 = 
  admin2_linked_aquaduct %>%
  filter(!bws_label %in% c("No Data", NA)) %>%
  mutate(bws_label=factor(bws_label, levels=c("Extremely High (>80%)","High (40-80%)",
                                      "Medium - High (20-40%)", "Low - Medium (10-20%)", "Low (<10%)", 
                                      "Arid and Low Water Use"))) %>%
  ggplot() + 
  geom_sf(aes(fill = bws_label), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049', 'grey'), name = "Water Stress - present") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2out, 'water_stress_2020', '.png'), plot=plot1,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')


plot2 = 
  admin2_linked_aquaduct %>%
  filter(!bau50_ws_x_l %in% c("No Data", NA)) %>%
  mutate(bau50_ws_x_l=factor(bau50_ws_x_l, levels=c("Extremely high (>80%)","High (40-80%)",
                                              "Medium-high (20-40%)", "Low-medium (10-20%)", "Low (<10%)", 
                                              "Arid and low water use"))) %>%
  ggplot() + 
  geom_sf(aes(fill = bau50_ws_x_l), lwd = 0, alpha = 1) + 
  scale_fill_manual(values=c("#780000", "#C1121F", "#FDF0D5", '#669BBC', '#003049', 'grey'), name = "Water Stress - 2050 (BAU)") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.2, .2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title = element_text(size=22),
        legend.key.size = unit(0.8, "cm")) 

ggsave(paste0(path2out, 'water_stress_2050', '.png'), plot=plot2,
       scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')
