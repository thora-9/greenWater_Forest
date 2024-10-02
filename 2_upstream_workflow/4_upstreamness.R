#########################################################
##Get upstreamness of neighbouring cells
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Specify the continent
cur_continent = 'Australasia'
cur_continent_abr = 'AU'
cur_continent_abr_UP = 'au'

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
  dplyr::select(1:4) %>%
  mutate(area = as.numeric(st_area(.)/10000)) #Convert m2 to hectare - by dividing by 10000

data_sub_r = fasterize(data_sub, fishnet.r %>% raster)

#To free memory
rm(data_in)

##############################
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

##########################################################################################
#Load the upstream-HYBAS/unconnected linkage
#This was calculated in step 1 of the workflow

path2linkage = paste0(proj_dir, 'Upstream/', cur_continent, '/')
linkage_HYBAS = 
  readRDS(paste0(path2linkage, 'upstream_codes_',cur_continent_abr_UP, '_wdis.rds')) %>%
  tidytable::rename(chain = 2)

##########################################################################################
#Scrip-specific pre-processing
##########################################################################################
#Create a fishnet with a unique ID
fishnet_rID = 
  fasterize(fishnet, fishnet.r %>% raster, field = 'Id') %>%
  mask(data_sub_r) %>% rast

#Get the unique fishnet values
unique_values = 
  unique(values(fishnet_rID)) %>% .[!is.na(.)]

#Get the cell numbers with cell values
cell_numbers = 
  terra::cells(fishnet_rID, unique_values, pair = T) %>% as.data.table() %>%
  dplyr::rename(cellnumber = 1, Id = 2)

#Link cell numbers to values and HYBAS ID
data_merged_fishnet = 
  merge(data_merged_fishnet, cell_numbers, by = 'Id', all.x = T)

#Get adjacent cells
focal_neighbours = 
  adjacent(fishnet_rID, cell_numbers$cellnumber, directions = 'queen', include = T) %>%
  as.data.table()
colnames(focal_neighbours) <- c("Center","NW", "N", "NE", "W", "E", "SW", "S", "SE")

#Convert to long format
focal_neighbours_long = 
  focal_neighbours %>%
  pivot_longer(cols = NW:SE, names_to = 'Direction', values_to = 'cellnumber') #%>%
  #mutate(total_area = NA, up_area = NA)

#Get the the cell area
cell_area = 
  cellSize(fishnet_rID, unit = 'ha')


##########################################################################################
##############################
#Estimate the index for upstream regions

#Split the df by HYBAS_ID
upstreamness_cell_list =
  split(focal_neighbours, by = 'Center') 

#Function to get the centroid distances
get_upstreamness <- function(df) {

  #Select the cell under consideration
  cur_cell = df$Center
  cur_neighbours = df[1, 2:9] %>% as.numeric()
  
  #Find all watersheds that are within that cell
  ws_within_cur_cell = data_merged_fishnet %>% filter(cellnumber == cur_cell)
  #Find all the upstream watersheds to the watershed within that cell
  upstream_cur_cell = linkage_HYBAS %>% filter(HYBAS_ID %in% ws_within_cur_cell$HYBAS_ID)
  #Get the spatial subset of all the upstream watersheds
  upstream_cur_cell_vec = 
    data_merged_fishnet %>% 
    filter(HYBAS_ID %in% upstream_cur_cell$chain) %>%
    #Subset to those that are part of the neighbours of the current cell
    filter(cellnumber %in% cur_neighbours)
  
  #Summarize to get total upstreamness area of each neighboring cell
  upstream_cur_cell_area = 
    upstream_cur_cell_vec %>%
    st_drop_geometry() %>%
    as.data.table() %>%
    dplyr::group_by(cellnumber) %>%
    dplyr::summarise(up_area = sum(area, na.rm = T)) %>%
    #upstreamness_across: across all the neighbouring cells, the proportion of the overall 
    #upstream area for a given cell that is coming from each neighbour
    dplyr::mutate(upstreamness_across = round(prop.table(up_area),2)) 
  
  
  #Get the total cell area - the cell area actually the sum of all ws within it
  #and not the actual raster cell size. This because we are using centroids to link cells to
  #watersheds. So a watershed centroid might be within a cell, but its boundaries might spill
  #out. This can cause issues when summing for area and calculating proportions.
  
  #Get area of neighbouring cells
  neighbour_cur_cell_area = 
    data_merged_fishnet %>% 
    #Subset to those that are part of the neighbours of the current cell
    filter(cellnumber %in% cur_neighbours) %>%
    st_drop_geometry() %>%
    as.data.table() %>%
    dplyr::group_by(cellnumber) %>%
    dplyr::summarise(total_area = sum(area, na.rm = T))
  
  #Create a merged dataset
  merged_output = 
    neighbour_cur_cell_area %>%
    mutate(Center = cur_cell) %>%
    merge(upstream_cur_cell_area, by = 'cellnumber', all.x = T) %>%
    relocate(Center)
  
  merged_output
}

#Run the process in parallel
start = Sys.time()
out_list <- pblapply(upstreamness_cell_list, get_upstreamness)
# To test: list item number 1440 is good for SA
#out_list <- parallel::mclapply(upstream_HYBAS_list[1:10000], summarize_upstream, mc.cores = 4)
end = Sys.time()

end - start

#Merge the lists
out_list_merged = rbindlist(out_list)

##########################################################################################
#Condition output

upstreamness_out = 
  focal_neighbours_long %>%
  merge(out_list_merged, by = c('Center', 'cellnumber'), all.x = T) %>% #Merge direction with upstream calculation
  merge(cell_numbers, by.x = 'Center', by.y = 'cellnumber', all.x = T) %>% #link cellnumber with ID of center
  merge(fishnet[,c('Id', 'Lon', 'Lat')] %>% st_drop_geometry(), #Add other fishnet attributes
        by = 'Id', all.x = T) %>% 
  dplyr::rename(OBJECTID = Id) %>% 
  merge(cell_numbers, by = 'cellnumber', all.x = T) %>% #Link the neighbour cellnumber to cellID
  dplyr::rename(neighbour_ID = Id) %>%
  #upstreamness_within: for a given neighbouring cell, what proportion of its area is contributing
  #downstream to the cell
  mutate(upstreamness_within = round(up_area/total_area, 2)) %>% #calculate upstreamness
  mutate(upstreamness_within = ifelse(is.na(upstreamness_within), 0, upstreamness_within)) %>%
  dplyr::select(-cellnumber, -Center, -up_area, -total_area) %>%
  dplyr::relocate(neighbour_ID) %>%
  dplyr::relocate(OBJECTID) %>%
  filter(!is.na(neighbour_ID)) %>% #remove cells where the neighbour doesn't exist in the fishnet raster
  dplyr::relocate(Lon:Lat, .before = neighbour_ID) %>%
  dplyr::arrange(OBJECTID)
  
##########################################################################################
path2output = paste0(proj_dir, 'Upstreamness/', '/')
fwrite(upstreamness_out, paste0(path2output, 'upstreamness_cell_', cur_continent_abr, '.csv'))





#Tests
all_upstream = data_sub %>% filter(HYBAS_ID %in% upstream_cur_cell$chain)
st_write(all_upstream, paste0(path2output, 'all_upstream.gpkg'))

neighbour_upstream = data_sub %>% filter(HYBAS_ID %in% upstream_cur_cell_vec$HYBAS_ID)
st_write(neighbour_upstream, paste0(path2output, 'neighbour_upstream.gpkg'))

i = 2550 #1440 good number
