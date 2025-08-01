############################################################
##Processing Wada Discharge data
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)

##########################################################################################
#Input data

upstream = 
  readRDS(paste0(proj_dir, 'Upstream/South America/upstream_codes_SA_wdis.rds'))

upstream_counts = 
  upstream %>%
  group_by(HYBAS_ID) %>%
  summarise(total_rows = n())

upstream_sub = 
  upstream %>%
  filter(HYBAS_ID == '6121180370')

upstream_sub_100 = 
  upstream %>%
  filter(HYBAS_ID == '6121180370' & distance < 100)

watersheds = 
  st_read(paste0(database, "Watersheds/HydroBASIN/hybas_sa_lev01-12_v1c/hybas_sa_lev12_v1c.shp"))

ws_upstream = 
  watersheds %>%
  filter(HYBAS_ID %in% upstream_sub$upstream_chain)

ws_upstream_100 = 
  watersheds %>%
  filter(HYBAS_ID %in% upstream_sub_100$upstream_chain)

##########################################################################################

#Load the upstreamness index file

#(GET THIS FROM RUNNING THE 4.Upstreamness.R file)
#Link cell numbers to values and HYBAS ID
df_sub = 
  data_merged_fishnet %>%
  filter(HYBAS_ID == '6121180370')

cur_continent_abr = 'SA'

path2linkage = paste0(proj_dir, 'Upstreamness/', '/')
upstreamness = 
  fread(paste0(path2linkage, 'upstreamness_cell_',cur_continent_abr, '.csv')) %>%
  filter(OBJECTID == 3532)

all_ws_within = 
  data_merged_fishnet %>%
  filter(OBJECTID == 3532)

upstream_sub_all = 
  upstream %>%
  filter(HYBAS_ID %in% all_ws_within$HYBAS_ID)

all_upstream_within = 
  watersheds %>%
  filter(HYBAS_ID %in% upstream_sub_all$upstream_chain)


##########################################################################################
st_write(ws_upstream, 
         paste0(proj_dir, 'Upstream/South America/upstream_sub_figure.gpkg'),
         append=FALSE)

st_write(ws_upstream_100, 
         paste0(proj_dir, 'Upstream/South America/upstream_sub_figure_100.gpkg'),
         append=FALSE)

st_write(all_upstream_within, 
         paste0(proj_dir, 'Upstream/South America/upstream_sub_all_figure.gpkg'),
         append=FALSE)



