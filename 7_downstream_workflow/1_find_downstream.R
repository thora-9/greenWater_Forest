#####################################
##Get Downstream (HydroATLAS/BASIN)
#####################################

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

#paths
proj_dir = "~/Dropbox/WB/livable_planet/greenwater_forests_biodiv/"
database = "~/Dropbox/Database/"

#Specify the continent
cur_continent = 'South America'
cur_continent_abr = 'SA'
cur_continent_abr_UP = 'sa'
cur_folder_name = '250901'

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
#HydroATLAS/BASIN data 
##############################
#Continent-level

path2WS = paste0(database, 'Watersheds/HydroBASIN/hybas_', cur_continent_abr, '_lev01-12_v1c/')

data_in = 
  st_read(paste0(path2WS, 'hybas_', cur_continent_abr, '_lev12_v1c.shp')) %>%
  st_make_valid()

#Create a non-spatial DT copy to improve computation efficiency
data_in_asDT = 
  data_in %>% 
  st_drop_geometry() %>%
  as.data.table()

#Remove variables improve computation efficiency
data_sub_DT = 
  data_in_asDT[, 1:4]

#Get just the HYBAS_ID to reduce memory/computation load and then get the centroid
data_sub = 
  data_in %>%
  dplyr::select(1:4) 

#To free memory
rm(data_in)

########################################################
#Run the algorithm to get the upstream
i = 2
downstream_out = NULL

out_df = data.frame(HYBAS_ID = NULL, downstream_chain = NULL)

#------------------------------------------------------------
# Example input
# data_sub_DT : data.table of HydroBASINS with at least HYBAS_ID and NEXT_DOWN
#------------------------------------------------------------

# Build a lookup table: HYBAS_ID → NEXT_DOWN
nextdown_lookup <- setNames(data_sub_DT$NEXT_DOWN, data_sub_DT$HYBAS_ID)

# Split dataset by HYBAS_ID
HYBAS_list <- split(data_sub_DT, by = "HYBAS_ID")

#------------------------------------------------------------
# Function to get downstream chain (long format)
#------------------------------------------------------------
getDownstream = function(df) {
  
  # starting watershed ID
  cur_ws_ID_primary <- df$HYBAS_ID
  
  # initialize chain
  downstream_out <- cur_ws_ID_primary
  
  # follow NEXT_DOWN using the lookup vector
  cur_next <- nextdown_lookup[as.character(cur_ws_ID_primary)]
  
  while(!is.na(cur_next) && cur_next != 0){
    downstream_out <- c(downstream_out, cur_next)
    cur_next <- nextdown_lookup[as.character(cur_next)]
  }
  
  # return in long format
  out_df <- data.table(HYBAS_ID = cur_ws_ID_primary,
                       downstream_member = downstream_out,
                       order = seq_along(downstream_out))
  
  out_df
}

#------------------------------------------------------------
# Apply to all HYBAS_IDs
#------------------------------------------------------------
downstream_results <- pblapply(HYBAS_list, getDownstream)

# bind into one big table
downstream_DT <- rbindlist(downstream_results)


pathOut = paste0(proj_dir, 'Downstream/', cur_continent, '/', 'downstream_codes_' , cur_continent_abr, '.rds')
#saveRDS(downstream_DT, pathOut)

########################################################
#Tests

# #Get the size of upstream watersheds for each watershed
# out_df_mod =
#   downstream_DT %>%
#   group_by(HYBAS_ID) %>%
#   summarise(group_size = n()) %>%
#   arrange(-group_size) %>%
#   as.data.table()

# #Test to see how well the upstream estimated variables between hydroATLAS and 
# #my estimates matches

# #Get all the upstream watersheds for the given watershed
# test_upstream = 
#   data_in_atlas_DT %>%
#   filter(HYBAS_ID %in% out_list_merged[HYBAS_ID == 2120350690]$upstream_chain) %>%
#   filter(HYBAS_ID != 2120350690)

# test_upstream_spatial= 
#   data_sub %>%
#   filter(HYBAS_ID %in% downstream_DT[HYBAS_ID == 6121164390]$downstream_member) 

# test_upstream_spatial= 
#   data_sub %>%
#   filter(HYBAS_ID %in% downstream_DT[HYBAS_ID %in% out_df_mod$HYBAS_ID[1:2000]]$downstream_member) 

# st_write(test_upstream_spatial, paste0(proj_dir, 'Downstream/', cur_continent, '/', 'test3.gpkg'), append=FALSE)


# test2

# #Get the hydroATLAS calculated value
# test_current = 
#   data_in_atlas_DT %>%
#   filter(HYBAS_ID == 2120350690)

# #Assess the ratio
# #GDP
# sum(test_upstream$gdp_ud_ssu)/test_current$gdp_ud_usu

# #
# sum(test_upstream$pop_ct_ssu)/test_current$pop_ct_usu

########################################################
########################################################

#------------------------------------------------------------
# Load downstream chains (already computed separately)
downstream_HYBAS <- readRDS(
  file.path(proj_dir, "Downstream", cur_continent,
            paste0("downstream_codes_", cur_continent_abr, ".rds"))
)

#------------------------------------------------------------
# Precompute centroid of each HYBAS polygon (once only)
data_sub_centroid <- data_sub |>
  st_make_valid() |>
  st_centroid()

# Make a lookup table: HYBAS_ID -> centroid geometry
centroid_lookup <- setNames(st_geometry(data_sub_centroid),
                            data_sub_centroid$HYBAS_ID)

#------------------------------------------------------------
# Split downstream chains by HYBAS_ID
downstream_HYBAS_list <- split(downstream_HYBAS, by = "HYBAS_ID")

# Sort each list element (optional, for reproducibility)
downstream_HYBAS_list <- pblapply(
  downstream_HYBAS_list,
  function(df) df[order(df$downstream_member)]
)

# Extract the CRS once
crs_val <- st_crs(data_sub_centroid)

#------------------------------------------------------------
# Function to compute centroid-to-centroid distances
getdist <- function(df) {
  ws_1_geom <- centroid_lookup[[as.character(df$HYBAS_ID[1])]]
  ws_2_geom <- centroid_lookup[as.character(df$downstream_member)]
  
  if (length(ws_2_geom) == 0) return(NULL)
  
  # Wrap ws_1_geom as sfc (single geometry)
  ws_1_geom <- st_sfc(ws_1_geom, crs = crs_val)
  
  # Wrap ws_2_geom as sfc with same CRS
  ws_2_geom <- st_sfc(ws_2_geom, crs = crs_val)
  
  # Compute distances in km
  dist_km <- as.numeric(st_distance(ws_1_geom, ws_2_geom)) / 1000
  
  data.table(
    HYBAS_ID = df$HYBAS_ID[1],
    downstream_member = df$downstream_member,
    distance_km = round(dist_km, 2)
  )
}

#------------------------------------------------------------
# Run in parallel
start <- Sys.time()
out_list <- pblapply(downstream_HYBAS_list, getdist)
end <- Sys.time()

print(end - start)

# Merge results into one long table
out_list_merged <- rbindlist(out_list)

pathOut = paste0(proj_dir, 'Downstream/', cur_continent, '/', 'downstream_codes_' , cur_continent_abr, '_wdist.rds')
saveRDS(out_list_merged, pathOut)

# fwrite(out_list_merged, paste0(proj_dir, 'Upstream/upstream_codes_AU_wdist.csv'))
# saveRDS(out_list_merged, paste0(proj_dir, 'Upstream/upstream_codes_AU_wdis.rds'))
# 