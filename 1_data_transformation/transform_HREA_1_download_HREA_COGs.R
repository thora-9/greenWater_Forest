##########################################################################################
# Title:       download_HREA_COGs.R
# Description: Example script to view STAC information and download HREA data.
# Author:      Zach O'Keeffe <zokeeffe@umich.edu>
# Date:        2024-05-26
##########################################################################################
machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

rescale_raster <- function(rast_in){
  nx <- minmax(rast_in)    
  rn <- (rast_in - nx[1,]) / (nx[2,] - nx[1,])
  return(rn)
}

#########################################################################################################
#Step 1: Download the data
# Read lookup table
LUDT <- fread('https://globalnightlight.s3.amazonaws.com/HREA_aux_data/HREA_place_lookup.csv', header=TRUE, sep=',')

LUDT_list <-
  split(LUDT,1:nrow(LUDT))

i = 1

# One can also view the collection of COGs of some file_type (rade9lnmu, set_zscore, set_lightscore, set_prplit) for a cog_id:
file_type <- c('rade9lnmu', 'set_zscore', 'set_lightscore', 'set_prplit')

download_HREA_data <- function(cur_row, type_of_file = file_type[2]){
  # Set path for downloading files (the following is an example that follows the directory structure on AWS)
  COG_local_dir <- paste0('/Users/tejasvi/Dropbox/Database/SocioEconomic/ElectricityAccess_Min_2024/',
                          '/HREAv1.1_COGs/', cur_row$country_iso3, '/', cur_row$cog_id, '/', type_of_file)
  dir.create(COG_local_dir, FALSE, TRUE)
  
  # To download a particular file_type for a given year, select the year (2013-2020):
  year <- (2013:2020)
  
  #Loop over all years
  for(i in year){
    # Build the URL:
    COG_URL <- paste0('https://globalnightlight.s3.amazonaws.com/HREAv1.1_COGs/', 
                      cur_row$country_iso3, '/', cur_row$cog_id, '/', 
                      type_of_file, '/', cur_row$cog_id, '_', type_of_file, 
                      '_', i, '.tif')
    
    system(paste('wget -P', COG_local_dir, COG_URL))
  }
  return(NULL)
}


#Run the process in parallel
start = Sys.time()
out_list <- parallel::mclapply(LUDT_list, download_HREA_data, mc.cores = 7)
end = Sys.time()


#########################################################################################################
#Step 2: Process the data

##############################
#Forest Fragmentation Data
path2data = paste0(database, "SocioEconomic/ElectricityAccess_Min_2024/HREAv1.1_COGs/")

year <- 
  2013:2020 %>% as.list()

create_composite <- function(cur_year){
  data_in1 = 
    list.files(path2data, paste0(cur_year, '.tif$'), full.names = T, recursive = T) %>%
    lapply(rast) %>%
    lapply(function(x){terra::resample(x, fishnet.r, method = 'med')}) %>%
    sprc() %>%
    terra::merge(na.rm = T, overwrite=TRUE)
  
  data_fishnet2 = 
    terra::resample(data_in1, fishnet.r, method = 'med')
  
  writeRaster(data_in1, paste0(paste0(database, "SocioEconomic/ElectricityAccess_Min_2024/", 'processed/',
                                      'avg_rad', cur_year, '_med.tif')))
}

#Run the process in parallel
start = Sys.time()
out_list <- parallel::mclapply(year, create_composite, mc.cores = 7)
end = Sys.time()

#Rescale the raster values to be between 0-1
rast_rscl = lapply(out_list, rescale_raster)


