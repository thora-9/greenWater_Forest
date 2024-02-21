#####################################
##Get Upstream (HydroATLAS/BASIN)
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
#HydroATLAS/BASIN data 

data_in_atlas =
  st_read(paste0(database, "Watersheds/HydroATLAS/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.gpkg"))

data_in_atlas_DT = 
  data_in_atlas %>% 
  st_drop_geometry() %>%
  as.data.table()

rm(data_in_atlas)

data_in1 = 
  st_read(paste0(database, "Watersheds/HydroBASIN/hybas_sa_lev01-12_v1c/hybas_sa_lev12_v1c.shp"))

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


#Run the algorithm to get the upstream
i = 2
upstream_out = NULL

out_df = data.frame(HYBAS_ID = NULL, upstream_chain = NULL)

#Algorithm Description: The algorithm essentially uses the fields NEXT_DOWN in the hydrosheds database.
#to backpropogate and find watersheds that are flowing into a given watershed.

for(i in 1:nrow(data_sub_DT)){
  
  #Track progress
  print(i)
  #Get the main level 1 watershed ID/row
  cur_ws_primary = data_sub_DT[i, ]
  cur_ws_ID_primary = cur_ws_primary$HYBAS_ID
  
  #Find the watersheds that have the level 1 watershed list as NEXT_DOWN
  cur_list = data_sub_DT[NEXT_DOWN %in% cur_ws_ID_primary]
  #Create a chain with list of watershed IDs that are upstream
  upstream_out = c(cur_ws_ID_primary, cur_list$HYBAS_ID)
  
  #Do a backpropogation to find the watersheds that upstream of the upstream watersheds
  #and so on
  while(nrow(cur_list) > 0){
    #print(nrow(cur_list))
    cur_ws = cur_list[1, ]
    cur_ws_ID = cur_ws$HYBAS_ID
    
    cur_list_temp = data_sub_DT[NEXT_DOWN %in% cur_ws_ID]
    upstream_out = c(upstream_out, cur_list_temp$HYBAS_ID)
    
    cur_list = rbind(cur_list, cur_list_temp)
    cur_list = cur_list[-1, ]
  }
  
  #Store the output for the current watershed
  out_temp = data.table(HYBAS_ID = cur_ws_ID_primary, 
                        upstream_chain = upstream_out)
  #Add it to the main output
  out_df = rbind(out_df, out_temp)
  
  #Reset the upstream chain vector
  upstream_out = NULL
  
}


fwrite(out_df, paste0(proj_dir, 'Upstream/upstream_codes_SA.csv'))
saveRDS(out_df, paste0(proj_dir, 'Upstream/upstream_codes_SA.rds'))

#Tests

#Get the size of upstream watersheds for each watershed
out_df_mod =
  out_df %>%
  group_by(HYBAS_ID) %>%
  summarise(group_size = n()) %>%
  as.data.table()

#Test to see how well the upstream estimated variables between hydroATLAS and 
#my estimates matches

#Get all the upstream watersheds for the given watershed
test_upstream = 
  data_in_atlas_DT %>%
  filter(HYBAS_ID %in% out_df[HYBAS_ID == 6120007000]$upstream_chain) %>%
  filter(HYBAS_ID != 6120007000)

test_upstream_spatial= 
  data_sub %>%
  filter(HYBAS_ID %in% out_df[HYBAS_ID == 1120020040]$upstream_chain) %>%
  filter(HYBAS_ID != 1120020040)

#Get the hydroATLAS calculated value
test_current = 
  data_in_atlas_DT %>%
  filter(HYBAS_ID == 6120007000)

#Assess the ratio
#GDP
sum(test_upstream$gdp_ud_ssu)/test_current$gdp_ud_usu

#
sum(test_upstream$pop_ct_ssu)/test_current$pop_ct_usu

