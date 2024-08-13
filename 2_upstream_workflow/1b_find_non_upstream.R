#####################################
##Get Non-Upstream (HydroATLAS/BASIN)
#####################################

#Additional packages to run this script
#library(future)
#library(future.apply)

source("0_setEnvironment.R")

#Specify the continent
cur_continent = 'Africa'
cur_continent_abr = 'AF'
cur_continent_abr_UP = 'af'

##############################
#HydroATLAS/BASIN data 

#Load level 12 HYBAS data
data_in1 = 
  st_read(paste0(database, "Watersheds/HydroBASIN/hybas_", cur_continent_abr_UP, 
                 "_lev01-12_v1c/hybas_", cur_continent_abr_UP, "_lev12_v1c.shp"))

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

#Find the centroid of each HYBAS polygon
data_sub_centroid = 
  data_sub %>% 
  st_make_valid() %>%
  st_centroid()

#Load the upstream-HYBAS linkage
#This was calculated in step 1 of the workflow
path2upstream = paste0(proj_dir, 'Upstream/', cur_continent, '/')
upstream_HYBAS = readRDS(paste0(path2upstream, 'upstream_codes_',cur_continent_abr_UP, '_wdis.rds'))


########################################################
#Run the algorithm to get the upstream
i = 2
out_df = data.frame(HYBAS_ID = NULL, non_upstream = NULL)


#Split the df by HYBAS_ID
HYBAS_list =
  split(data_sub_DT, by = 'HYBAS_ID') 

#Test - list number 27
dist_threshold_km = 200 #Use 99999 for essentially no dist threshold

#Algorithm Description: For a given distance threshold, the algorithm aims to find all watersheds that are considered 'not upstream or downstream' 
#of a given watershed. This involves finding all sub-watersheds within a certain distance, and then excluding watersheds that were previously 
#estimated to be upstream in scrip 1.

get_unConnected = function(df) {
  
  library(data.table)
  library(tidyverse)
  library(sf)
  library(raster)
  library(terra)
  library(fasterize)
  library(stringdist)
  
  #Get the main level 1 watershed ID/row
  #cur_ws_primary = data_sub_DT[i, ]
  cur_ws_ID_primary = df$HYBAS_ID
  cur_st = data_sub_centroid %>% filter(HYBAS_ID == cur_ws_ID_primary)
  
  # If HYBAS_ID is equal to current WS -> upstream
  # If current WS is part of upstream chain of different watershed -> downstream
  connected_WS = 
    upstream_HYBAS[upstream_chain == cur_ws_ID_primary | HYBAS_ID == cur_ws_ID_primary]
  
  #Find the watersheds that are within distance threshold
  within_dist = 
    as.numeric(round(sf::st_distance(data_sub_centroid, cur_st)/1000, 2)) <= dist_threshold_km
  
  gc()
  
  out_df = 
    data_sub_DT %>%
    mutate(in_surround = within_dist) %>%
    filter(in_surround == T & 
             !HYBAS_ID %in% c(connected_WS$upstream_chain, connected_WS$HYBAS_ID)) %>% #Keep all within distance threshold but not upstream or downstream
    dplyr::select(unConnected = HYBAS_ID) %>%
    dplyr::mutate(HYBAS_ID = cur_ws_ID_primary) %>%
    dplyr::select(2,1)
  
  out_df

}


#Run the process on a single thread
# system.time({
#   out_list <- pblapply(HYBAS_list[1:100], get_unConnected)
# })
#out_list <- parallel::mclapply(upstream_HYBAS_list[1:10000], summarize_upstream, mc.cores = 4)

#Set up environment for parallel processing (Windows)
n.cores <- detectCores()
cl <- makeCluster (n.cores)
#Export environment
clusterExport(cl=cl, c('data_sub_centroid', 'upstream_HYBAS', 'data_sub_DT', 'dist_threshold_km'))

system.time({
out_list <- parLapply(cl, HYBAS_list, fun = get_unConnected)
#Merge the lists
out_list_merged = rbindlist(out_list)

saveRDS(out_list_merged, paste0(proj_dir, 'Unconnected/', 'unconnected_codes_', 
                                cur_continent_abr, '.rds'))
stopCluster (cl)

})


# #Trying the futureverse
# plan(multisession)
# 
# system.time({
# res2 = future_lapply(HYBAS_list[1:100], get_unConnected)
# })


########################################################
#Checks


test = readRDS(paste0(proj_dir, 'Unconnected/', 'unconnected_codes_', 
                      'AS', '.rds'))

#Get the size of upstream watersheds for each watershed
out_df_mod =
  upstream_HYBAS %>%
  group_by(HYBAS_ID) %>%
  summarise(group_size = n()) %>%
  as.data.table()


test = data_sub_centroid %>% filter(HYBAS_ID %in% out_list_merged[HYBAS_ID == 6120222160,]$unConnected)
test2 = data_sub_centroid %>% filter(HYBAS_ID %in% upstream_HYBAS[HYBAS_ID == 6120222160,]$upstream_chain)

st_write(test, paste0(proj_dir, 'Unconnected/', 'test.gpkg'), append=FALSE)
st_write(test2, paste0(proj_dir, 'Unconnected/', 'test2.gpkg'), append=FALSE)


test3 = data_sub_DT %>% filter(HYBAS_ID == 6120222160)

########################################################