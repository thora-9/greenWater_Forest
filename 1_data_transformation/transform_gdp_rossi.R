####################################
##GDP (Rossi-Hansberg et al., 2025)
####################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Reference Raster
fishnet.r.03125 = 
  disagg(fishnet.r, fact = 16)

##############################
#GDP Data
path2data = paste0(database, "SocioEconomic/GDP/Local_GDP_Rossi_Hansberg_2025/")

data_in1 = 
  fread(paste0(path2data, 'final_GDP_0_5deg_postadjust_pop_dens_0_adjust.csv'))

data_vec =
  st_read(paste0(path2data, 'geom_0_5deg.shp'))

##############################
#Run the fix geometry algorith
data_fixed_geom <- qgis_run_algorithm(
  "native:fixgeometries",
  INPUT = data_vec)

qgis_search_algorithms(algorithm = "centroid")

data_vec = 
  st_read(qgis_extract_output(data_fixed_geom, "OUTPUT"))

#Get the centroids
data_centroid = qgis_run_algorithm(
  "native:centroids",
  INPUT = data_vec)

data_vec = 
  st_read(qgis_extract_output(data_centroid, "OUTPUT"))


#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "join")
qgis_show_help("native:joinattributesbylocation")

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = data_vec,
    JOIN = fishnet[,2:4],
    PREDICATE = 5 #Use st_within
  )

data_vec_fishnet = st_read(qgis_extract_output(qgis_join, "OUTPUT"))

##############################
#Merge cells that span borders by summing values

data_in_conditioned = 
  data_in1 %>%
  group_by(cell_id, subcell_id, year) %>%
  summarise(across(predicted_GCP_const_2017_USD:pop_cell, ~sum(.x)))


#Add the Fishnet IDs
out.df = 
  data_in_conditioned %>%
  merge(data_vec_fishnet %>% st_drop_geometry() %>% dplyr::select(cell_id, subcell_id, Id, Lon, Lat), 
        by = c('cell_id', 'subcell_id')) %>%
  as.data.table() %>%
  drop_na()


##############################    
#Check with existing dataframe to see if cells exist/coverage
#X coordinates
sum(out.df$Lon %in% fishnet$Lon) == nrow(out.df)

#Y coordinates
sum(out.df$Lat %in% fishnet$Lat) == nrow(out.df)

test2 = merge(out.df, fishnet[, 1:4], by.x = c('x', 'y'), by.y = c('Lon', 'Lat'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(out.df)

##############################    
#Test Plot
plot_data = 
  out.df %>%
  filter(year == 2021) %>%
  mutate(gdp_per_capita = 10^9*predicted_GCP_const_2017_PPP/pop_cell)

plot_raster = 
  rasterFromXYZ(plot_data[,.(Lon, Lat, gdp_per_capita)]) %>%
  rast()

plot(plot_raster)

terra::writeRaster(plot_raster,
                   paste0(path2data, "processed/gdp_per_capita_2021.tif"),
                   overwrite = T)

plot_raster2 = 
  rasterFromXYZ(plot_data[,.(Lon, Lat, predicted_GCP_const_2017_PPP)]) %>%
  rast()

terra::writeRaster(plot_raster2,
                   paste0(path2data, "processed/gdp_2021.tif"),
                   overwrite = T)

##############################
#Write Output
##############################

path2out = paste0(path2data ,"processed/")

if (dir.exists(path2out) == F) {
  dir.create(path2out)
}


fwrite(out.df, paste0(path2out ,"final_GDP_0_5deg_2012_2021.csv"))
