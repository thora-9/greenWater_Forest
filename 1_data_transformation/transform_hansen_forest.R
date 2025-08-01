##########################################################################
##Hansen - 2013
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
proj_dir = "~/Dropbox/WB/JustInTime/250203/"
database = "~/Dropbox/Database/"

# Set your folder path
path2data <- "/Users/tejasvi/Dropbox/Database/Forestry/forest_extent_hansen_2013/Hansen_5km"

############################################################
#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet_fixed.gpkg'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

############################################################

# Find all .tif files in the folder (non-recursive)
tif_files <- list.files(path = path2data, pattern = "\\.tif$", full.names = TRUE)

# If you want to include subdirectories, use recursive = TRUE
# tif_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Load all rasters
rasters <- lapply(tif_files, rast)

# Mosaic them together
# The `do.call` is used to pass the list of rasters as separate arguments
mosaic_raster <- do.call(mosaic, rasters)

cell_area <- cellSize(mosaic_raster, unit = 'km')

forest_area = (mosaic_raster/100) * cell_area

# Convert to fishnet raster
forest_area_05 = 
  forest_area %>%
  resample(fishnet.r, method = 'sum') %>%
  mask(fishnet.r)

cell_area_05 <- cellSize(forest_area_05, unit = 'km')

forest_pct_05 = 100*forest_area_05/cell_area_05

#Convert to dataframe
out.df = 
  as.data.frame(forest_pct_05, xy = T) %>%
  as.data.table()

##############################    
#Check with existing dataframe to see if cells exist/coverage
#X coordinates
sum(out.df$x %in% fishnet$Lon) == nrow(out.df)

#Y coordinates
sum(out.df$y %in% fishnet$Lat) == nrow(out.df)

test2 = merge(out.df, fishnet[, 1:4], by.x = c('x', 'y'), by.y = c('Lon', 'Lat'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(out.df)

############################################################
path2out = paste0(path2data ,"/processed/")

if (dir.exists(path2out) == F) {
  dir.create(path2out)
}

fwrite(out.df, paste0(path2out ,"hansen_2013_treecover2000_05.csv"))
