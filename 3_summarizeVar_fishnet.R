#########################################################
##Estimate indices for each basin (HydroATLAS/BASIN)
#########################################################

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
library(qgisprocess)


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
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp')) %>%
  st_make_valid()

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

##############################
#HydroATLAS/BASIN data 

data_in_atlas =
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

data_in_atlas_DT = 
  data_in_atlas %>% 
  st_drop_geometry() %>%
  as.data.table()

rm(data_in_atlas)

data_in1 = 
  st_read(paste0(database, "Watersheds/HydroBASIN/hybas_af_lev01-12_v1c/hybas_af_lev12_v1c.shp"))

data_in_asDT = 
  data_in1 %>% 
  st_drop_geometry() %>%
  as.data.table()


data_sub_DT = 
  data_in_asDT[, 1:4]

#Get just the HYBAS_ID to reduce memory/computation load and then get the centroid
data_sub = 
  data_in1 %>%
  dplyr::select(1:4) 

#To free memory
rm(data_in1)

##############################
#Load the upstream-HYBAS summarized linkage

region = 'AF'
dist_threshold = 'nodist'


upstream_summarized_HYBAS = 
  readRDS(paste0(proj_dir, 'Upstream/upstream_VAR_', region, '_', dist_threshold, '.rds')) %>%
  mutate(across(natural_only:total_cells, ~ .x/total_cells))

#Load the upstream-HYBAS linkage
#This was calculated in step 1 of the workflow
upstream_HYBAS = readRDS(paste0(proj_dir, 'Upstream/upstream_codes_', region, '.rds'))

#Find the centroid of each HYBAS polygon
data_sub_centroid = 
  data_sub %>% 
  st_make_valid() %>%
  st_centroid(data_sub)

#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "join")
qgis_show_help("native:joinattributesbylocation")

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = data_sub_centroid,
    JOIN = fishnet[,1:2],
    PREDICATE = 5 #Use st_within
  )

data_merged_fishnet = st_read(qgis_extract_output(qgis_join, "OUTPUT"))


#Create a list of variables to summarize
var_mean = c("FFI2000", "FFI2020", "BHI_2010", "BHI_2020")
var_mode = c("FM_class_mode")
var_count = c("natural_only", "natural_managed", "natural_all", "planted", "agroforestry")

#Aggregation method 1 - Take the median/mean of all watersheds within grid cell

#Subset the variables, merge the fishnet id, summarize after grouping by fishnet id
data_in_summarized_v1 = 
  upstream_summarized_HYBAS %>%
  #dplyr::select(1, all_of(all_variables)) %>%
  mutate(across(FFI2000:upstream_total, ~ ifelse(.x == -999, NA, .x))) %>%
  merge(data_merged_fishnet[, c("HYBAS_ID", "Id")], by = 'HYBAS_ID') %>%
  #merge(upstream_HYBAS, by = "HYBAS_ID", all.x = T) %>%
  arrange(desc(upstream_total)) %>% 
  group_by(Id) %>%
  summarise(across(all_of(var_mean), ~ mean(.x, na.rm = TRUE)),
            across(all_of(var_mode), ~ modal(.x, na.rm = TRUE)),
            across(all_of(var_count), ~ mean(.x, na.rm = TRUE))) %>% 
  as.data.table()

#Aggregation method 2 - aggregate using the value of n watersheds with the greatest upstream extent

#Subset the variables, merge the fishnet id, summarize after grouping by fishnet id
data_in_summarized_v2 = 
  upstream_summarized_HYBAS %>%
  #dplyr::select(1, all_of(all_variables)) %>%
  mutate(across(FFI2000:upstream_total, ~ ifelse(.x == -999, NA, .x))) %>%
  merge(data_merged_fishnet[, c("HYBAS_ID", "Id")], by = 'HYBAS_ID') %>%
  #merge(upstream_HYBAS, by = "HYBAS_ID", all.x = T) %>%
  arrange(desc(upstream_total)) %>% 
  group_by(Id) %>%
  slice(1:5) %>%
  summarise(across(all_of(var_mean), ~ mean(.x, na.rm = TRUE)),
            across(all_of(var_mode), ~ modal(.x, na.rm = TRUE)),
            across(all_of(var_count), ~ mean(.x, na.rm = TRUE))) %>% 
  as.data.table()

#############################
#Merge the summarized dataset back to the fishnet to produce final output
out_fish_v1 = 
  fishnet %>%
  dplyr::select(Id, Lat, Lon) %>%
  merge(data_in_summarized_v1, by = 'Id', all.x = T) %>%
  filter(!is.na(BHI_2010))

out_fish_v2 = 
  fishnet %>%
  dplyr::select(Id, Lat, Lon) %>%
  merge(data_in_summarized_v2, by = 'Id', all.x = T) %>%
  filter(!is.na(BHI_2010))

#############################
#Tests: Create rasters and visualize output
out_fish_r = 
  fasterize(out_fish_v1, fishnet.r %>% raster, field = "natural_all") %>%
  crop(data_sub)

out_fish_r2 =
  fasterize(out_fish_v2, fishnet.r %>% raster, field = "natural_all") %>%
  crop(data_sub)

#############################
#Create the final DF for export
out_fish_df_v1 = 
  out_fish_v1 %>% 
  st_drop_geometry() %>%
  as.data.table()

out_fish_df_v2 = 
  out_fish_v2 %>% 
  st_drop_geometry() %>%
  as.data.table()


#############################
#Write Output


fwrite(out_fish_df_v1, paste0(proj_dir, 'Upstream/upstream_', region, '_allWS_', dist_threshold, '_240304.csv'))
fwrite(out_fish_df_v2, paste0(proj_dir, 'Upstream/upstream_', region, '_top5up_', dist_threshold, '_240304.csv'))


#writeRaster(out_fish_r, paste0(proj_dir, 'Upstream/test.tif'), , overwrite=TRUE)

#writeRaster(out_fish_r2, paste0(proj_dir, 'Upstream/test3.tif'), overwrite=TRUE)

test = 
  rast(paste0(database, "Forestry/forest_fragmentation_Ma_2023/FFI2000.tif")) %>%
  terra::project(crs(fishnet.r), method = 'bilinear')  %>% #Use bilinear as the variable is continuous
  crop(out_fish)

test2 = 
  fread(paste0(database, "Forestry/forest_fragmentation_Ma_2023/forest_frag_05_v2.csv")) %>%
  as.data.frame() 

test2 = rasterFromXYZ(test2[c("x", "y", "FFI2020_med")], crs=4326) 
writeRaster(test2, paste0(proj_dir, 'Upstream/test2.tif'))



#Load the upstream-HYBAS linkage
upstream_HYBAS =
  readRDS(paste0(proj_dir, 'Upstream/upstream_codes_SA.rds')) %>%
  group_by(HYBAS_ID) %>%
  summarise(count = n())
