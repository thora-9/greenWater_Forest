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
path2data <- "/Users/tejasvi/Dropbox/Database/Forestry/forest_extent_hansen_2013/Hansen_5km_WB"

############################################################
#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet_fixed.gpkg'))

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster_30percent.tif'))

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

forest_area = (mosaic_raster) * cell_area

# Convert to fishnet raster
forest_area_05 = 
  forest_area %>%
  terra::resample(fishnet.r, method = 'sum') %>%
  mask(fishnet.r)

cell_area_05 <- cellSize(forest_area_05, unit = 'km')

forest_pct_05 = 100*(forest_area_05/cell_area_05)

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

#fwrite(out.df, paste0(path2out ,"hansen_2013_treecover_2001_2020_05.csv"))
############################################################
my_hansen = fread("/Users/tejasvi/Dropbox/Database/Forestry/forest_extent_hansen_2013/Hansen_5km/processed/hansen_2013_treecover2000_05.csv")
wb_hansen = fread("/Users/tejasvi/Dropbox/Database/Forestry/forest_extent_hansen_2013/Hansen_5km_WB/processed/hansen_2013_treecover_2001_2020_05.csv")
esa_data = fread("/Users/tejasvi/Dropbox/Database/LULC/ESA_CCI_LC_2025/1_processed/ESA_tree_cover_by_year_05.csv")

merged_hansen = 
  wb_hansen %>%
  merge(my_hansen, by = c('x', 'y'), all.x = T) %>%
  merge(esa_data, by.x = c('x', 'y'), by.y = c('lon', 'lat'), all.x = T)


plot(merged_hansen$year_2001, merged_hansen$treecover2000)
cor.test(merged_hansen$year_2005, merged_hansen$percent_tree_land_2005)
