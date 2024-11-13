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
