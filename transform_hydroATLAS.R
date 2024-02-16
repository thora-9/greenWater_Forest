##############################
##HydroATLAS
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
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

##############################
#HydroATLAS data 

data_in1 = 
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

data_in_asDT = 
  data_in1 %>% 
  st_drop_geometry() %>%
  as.data.table()
  

#Using QGIS to speed up operations
#Get just the HYBAS_ID to reduce memory/computation load and then get the centroid
data_sub = 
  data_in1 %>%
  dplyr::select(1:4) 

#Run the fix geometry algorith
data_fixed_geom <- qgis_run_algorithm(
  "native:fixgeometries",
  INPUT = data_sub)

data_sub_fixed = st_read(qgis_extract_output(data_fixed_geom, "OUTPUT"))

#Now run the QGIS algorithm to get centroid
#1) Find the algorithm
qgis_search_algorithms(algorithm = "centroid")

#2) Run the algorithm -> store the results

#Get the input format
qgis_show_help("native:centroids")

centroids = 
  qgis_run_algorithm(
    "native:centroids",
    INPUT = data_sub_fixed)

data_centroid = st_read(qgis_extract_output(centroids, "OUTPUT"))


#Do a spatial join on the centroids to link fishnet cell id
qgis_search_algorithms(algorithm = "join")
qgis_show_help("native:joinattributesbylocation")

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = data_centroid,
    JOIN = fishnet[,1:2],
    PREDICATE = 5 #Use st_within
  )

data_merged_fishnet = st_read(qgis_extract_output(qgis_join, "OUTPUT"))

#Create a list of variables to summarize
hydrology = c('dis_m3_pyr', 'run_mm_syr', 'gwt_cm_sav')
physio = c('ele_mt_sav', 'ele_mt_uav', "slp_dg_sav", "slp_dg_uav")
climate = c("clz_cl_smj", "cls_cl_smj", "tmp_dc_syr", "tmp_dc_uyr",
            "pre_mm_syr", "pre_mm_uyr", "pet_mm_syr", "pet_mm_uyr",
            "aet_mm_syr", "aet_mm_uyr", "ari_ix_sav", "ari_ix_uav", "cmi_ix_syr", "cmi_ix_uyr")
landcover = c("for_pc_sse", "for_pc_use", "crp_pc_sse", "crp_pc_use","pst_pc_sse",
              "pst_pc_use", "ire_pc_sse", "ire_pc_use")
soils = c("cly_pc_sav", "cly_pc_uav", "slt_pc_sav", "slt_pc_uav", "snd_pc_sav",
          "snd_pc_uav", "soc_th_sav", "soc_th_uav", "swc_pc_syr", "swc_pc_uyr")
anthropogenic = c("pop_ct_ssu", "pop_ct_usu", "ppd_pk_sav", "ppd_pk_uav",
                  "urb_pc_sse", "urb_pc_use", "nli_ix_sav", "nli_ix_uav",
                  "rdd_mk_sav", "rdd_mk_uav", "hft_ix_s93","hft_ix_u93", 
                  "hft_ix_s09", "hft_ix_u09", "gad_id_smj", "gdp_ud_sav", 
                  "gdp_ud_ssu","gdp_ud_usu", "hdi_ix_sav")

all_variables = c(hydrology, physio, climate, landcover, soils, anthropogenic)

#Subset the variables, merge the fishnet id, summarize after grouping by fishnet id
data_in_summarized = 
  data_in_asDT %>%
  dplyr::select(1:4, all_of(all_variables)) %>%
  mutate(across(all_variables, ~ ifelse(.x == -999, NA, .x))) %>%
  merge(data_merged_fishnet[, c("HYBAS_ID", "Id")], by = 'HYBAS_ID') %>%
  group_by(Id) %>%
  summarise(across(all_variables, ~ median(.x, na.rm = TRUE))) %>% 
  as.data.table()

#Merge the summarized dataset back to the fishnet to produce final output
out_fish = 
  fishnet %>%
  dplyr::select(Id, Lat, Lon) %>%
  merge(data_in_summarized, by = 'Id', all.x = T)

out_fish_df = 
  out_fish %>% 
  st_drop_geometry() %>%
  as.data.table()

##############################
#Write Output
##############################

fwrite(all_variables %>% as.data.frame(), paste0(database, "Watersheds/HydroATLAS/Processed/variables.csv"))
st_write(out_fish, paste0(database, "Watersheds/HydroATLAS/Processed/hydroATLAS_05deg.gpkg"))
fwrite(out_fish_df, paste0(database, "Watersheds/HydroATLAS/Processed/hydroATLAS_05deg.csv"))

