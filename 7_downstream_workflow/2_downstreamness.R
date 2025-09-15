#########################################################
## Get downstreamness of neighbouring cells
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Specify the continent
cur_continent = 'South America'
cur_continent_abr = 'SA'
cur_continent_abr_UP = 'sa'

##############################
# Continent-level input
path2WS = paste0(database, 'Watersheds/HydroBASIN/hybas_', cur_continent_abr, '_lev01-12_v1c/')

data_in = 
  st_read(paste0(path2WS, 'hybas_', cur_continent_abr, '_lev12_v1c.shp')) %>%
  st_make_valid()

# Non-spatial copy for efficiency
data_in_asDT = 
  data_in %>% 
  st_drop_geometry() %>%
  as.data.table()

data_sub_DT = data_in_asDT[, 1:4]

# Keep HYBAS_ID + area
data_sub = 
  data_in %>%
  dplyr::select(1:4) %>%
  dplyr::mutate(area = as.numeric(st_area(.)/10000)) # m² → ha

data_sub_r = fasterize(data_sub, fishnet.r %>% raster)

rm(data_in) # free memory

##############################
# Centroids + fishnet linkage
data_sub_centroid = 
  data_sub %>% 
  st_make_valid() %>%
  st_centroid()

qgis_join = 
  qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT = data_sub_centroid,
    JOIN = fishnet[,1:2],
    PREDICATE = 5 # within
  )

data_merged_fishnet = st_read(qgis_extract_output(qgis_join, "OUTPUT"))

##########################################################################################
# Load downstream HYBAS linkage (instead of upstream)
path2linkage = paste0(proj_dir, 'Downstream/', cur_continent, '/')
linkage_HYBAS = 
  readRDS(paste0(path2linkage, 'downstream_codes_', cur_continent_abr, '_wdist.rds')) %>%
  tidytable::rename(chain = 2)

##########################################################################################
# Pre-processing
fishnet_rID = 
  fasterize(fishnet, fishnet.r %>% raster, field = 'Id') %>%
  mask(data_sub_r) %>% rast

unique_values = unique(values(fishnet_rID)) %>% .[!is.na(.)]

cell_numbers = 
  terra::cells(fishnet_rID, unique_values, pairs = T) %>% as.data.table() %>%
  dplyr::rename(cellnumber = 1, Id = 2)

data_merged_fishnet = 
  merge(data_merged_fishnet, cell_numbers, by = 'Id', all.x = T)

focal_neighbours = 
  adjacent(fishnet_rID, cell_numbers$cellnumber, directions = 'queen', include = T) %>%
  as.data.table()
colnames(focal_neighbours) <- c("Center","NW","N","NE","W","E","SW","S","SE")

focal_neighbours_long = 
  focal_neighbours %>%
  pivot_longer(cols = NW:SE, names_to = 'Direction', values_to = 'cellnumber')

##########################################################################################
# Estimate downstreamness index
downstreamness_cell_list = split(focal_neighbours, by = 'Center') 

get_downstreamness <- function(df) {
  
  cur_cell = df$Center
  cur_neighbours = df[1, 2:9] %>% as.numeric()
  
  # Watersheds in current cell
  ws_within_cur_cell = data_merged_fishnet %>% filter(cellnumber == cur_cell)
  
  # Their downstream chains
  downstream_cur_cell = linkage_HYBAS %>% filter(HYBAS_ID %in% ws_within_cur_cell$HYBAS_ID)
  
  downstream_cur_cell_vec = 
    data_merged_fishnet %>% 
    filter(HYBAS_ID %in% downstream_cur_cell$chain) %>%
    filter(cellnumber %in% cur_neighbours)
  
  downstream_cur_cell_area = 
    downstream_cur_cell_vec %>%
    st_drop_geometry() %>%
    as.data.table() %>%
    dplyr::group_by(cellnumber) %>%
    dplyr::summarise(down_area = sum(area, na.rm = T)) %>%
    dplyr::mutate(downstreamness_across = round(prop.table(down_area), 2)) 
  
  neighbour_cur_cell_area = 
    data_merged_fishnet %>% 
    filter(cellnumber %in% cur_neighbours) %>%
    st_drop_geometry() %>%
    as.data.table() %>%
    dplyr::group_by(cellnumber) %>%
    dplyr::summarise(total_area = sum(area, na.rm = T))
  
  merged_output = 
    neighbour_cur_cell_area %>%
    mutate(Center = cur_cell) %>%
    merge(downstream_cur_cell_area, by = 'cellnumber', all.x = T) %>%
    relocate(Center)
  
  merged_output
}

# Parallel execution
start = Sys.time()
out_list <- pblapply(downstreamness_cell_list, get_downstreamness)
end = Sys.time()
print(end - start)

out_list_merged = rbindlist(out_list)

##########################################################################################
# Condition output
downstreamness_out = 
  focal_neighbours_long %>%
  merge(out_list_merged, by = c('Center','cellnumber'), all.x = T) %>%
  merge(cell_numbers, by.x = 'Center', by.y = 'cellnumber', all.x = T) %>%
  merge(fishnet[,c('Id','Lon','Lat')] %>% st_drop_geometry(),
        by = 'Id', all.x = T) %>% 
  dplyr::rename(OBJECTID = Id) %>% 
  merge(cell_numbers, by = 'cellnumber', all.x = T) %>% 
  dplyr::rename(neighbour_ID = Id) %>%
  mutate(downstreamness_within = round(down_area/total_area, 2)) %>%
  mutate(downstreamness_within = ifelse(is.na(downstreamness_within), 0, downstreamness_within)) %>%
  dplyr::select(-cellnumber, -Center, -down_area, -total_area) %>%
  dplyr::relocate(neighbour_ID) %>%
  dplyr::relocate(OBJECTID) %>%
  filter(!is.na(neighbour_ID)) %>%
  dplyr::relocate(Lon:Lat, .before = neighbour_ID) %>%
  dplyr::arrange(OBJECTID)

##########################################################################################
# Save output
path2output = paste0(proj_dir, 'Downstream/', cur_continent, '/')
fwrite(downstreamness_out, paste0(path2output, 'downstreamness_cell_', cur_continent_abr, '.csv'))
