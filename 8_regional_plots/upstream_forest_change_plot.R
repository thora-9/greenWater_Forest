##########################################################################
##Upstream Forest Cover Plots
##########################################################################

library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(terra)
library(ggrepel)
library(ggpubr)
library(fasterize)
library(qgisprocess)
library(RColorBrewer)

#paths
proj_dir = "~/Dropbox/WB/livable_planet/greenwater_forests_biodiv/"
out_dir = paste0(proj_dir, 'Regional_Plots/')
database = "~/Dropbox/Database/"


############################################################
#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet_fixed.gpkg'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster_30percent.tif'))

############################################################
#Load World Regions

wb_regions = 
  st_read(paste0(database, "Admin/WB_Regions/WB_countries_Admin0_10m_QGIS_fixed.gpkg")) %>%
  filter(TYPE != 'Dependency') %>%
  dplyr::select(WB_NAME, ISO_A2, ISO_A3, ISO_N3, TYPE, REGION_WB, SUBREGION) %>%
  mutate(rownumber = row_number()) %>% #Need to create custom ID; otherwise some duplicates due to random colonial territories
  mutate(ISO_N3 = as.numeric(ISO_N3))

south_africa = 
  wb_regions %>%
  filter(WB_NAME == 'South Africa') %>%
  st_make_valid()

eap_wb = 
  wb_regions %>%
  st_make_valid() %>%
  mutate(REGION_WB = as.character(REGION_WB)) %>%
  filter(REGION_WB == 'East Asia & Pacific')

country_list = c('Angola', 'Botswana', 'Burundi', 'Comoros',
                 'Congo, Democratic Republic of', 'Eritrea', 'eSwatini',
                 'Ethiopia', 'Kenya', 'Lesotho', 'Madagascar',
                 'Malawi', 'Mauritius', 'Mozambique', 'Namibia',
                 'Rwanda', 'Seychelles', 'São Tomé and Príncipe', 'Somalia',
                 'South Africa', 'South Sudan', 'Sudan', 'Tanzania',
                 'Uganda', 'Zambia', 'Zimbabwe')

east_south_africa = 
  wb_regions %>%
  st_make_valid() %>%
  mutate(REGION_WB = as.character(REGION_WB)) %>%
  filter(WB_NAME %in% country_list)

country_list = c('Benin', 'Burkina Faso', 'Cabo Verde', 'Chad',
                 'Cameroon', 'Central African Republic', "Côte d'Ivoire", 'Equatorial Guinea',
                 'Gabon', 'Gambia, The', 'Ghana', 'Guinea',
                 'Guinea-Bissau', 'Liberia', 'Mali', 'Mauritania',
                 'Niger', 'Nigeria', 'Congo, Rep. of', 'Senegal',
                 'Sierra Leone', 'Togo')

west_africa = 
  wb_regions %>%
  st_make_valid() %>%
  mutate(REGION_WB = as.character(REGION_WB)) %>%
  filter(WB_NAME %in% country_list)

wb_regions_non_spatial = 
  wb_regions %>% st_drop_geometry() %>% as.data.table()

#Use the ISO_N3 to create a raster
wb_regions_raster = 
  fasterize(wb_regions, fishnet.r %>% raster, "rownumber") %>% rast

south_africa_admin2 = 
  st_read(paste0(database, 'Admin/South_Africa/MDB_District_Municipal_Boundary_2018.shp'))

############################################################
#Map 2  - Economic Costs of Droughts

cur_region = 'West Africa' #'South Africa' #or EAP

if (cur_region == 'South Africa') {region_bound = south_africa}
if (cur_region == 'EAP') {region_bound = eap_wb}
if (cur_region == 'East/South Africa') {region_bound = east_south_africa}
if (cur_region == 'West Africa') {region_bound = west_africa}

source("8_regional_plots/make_forest_cover_plots.R")

plots <- make_forest_cover_plots(
  cur_data   = "upstream_100", #'upstream_all' #'upstream_100' #'non_upstream'
  proj_dir   = proj_dir,
  region_bound = region_bound,
  fishnet.r  = fishnet.r,
  wb_regions = wb_regions,
  out_dir    = out_dir
)


source("8_regional_plots/make_abs_forest_cover_plots.R")

plots <- make_abs_forest_cover_plots(
  cur_data   = "non_upstream", #'upstream_all' #'upstream_100' #'non_upstream'
  proj_dir   = proj_dir,
  region_bound = region_bound,
  fishnet.r  = fishnet.r,
  wb_regions = wb_regions,
  out_dir    = out_dir
)
