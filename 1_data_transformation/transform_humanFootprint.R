####################################
##Human Footprint Index
####################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')

#Reference Raster
fishnet.r.03125 = 
  disagg(fishnet.r, fact = 16)

##############################
#Forest Fragmentation Data
path2data = paste0(database, "Biodiversity/HumanFootprint/HFI_Mu_2022/")

data_in1 = 
  list.files(path2data, '.tif$', full.names = T) %>%
  rast() %>%
  project(crs(fishnet.r))


#Align and resample the data
data_fishnet1 = 
  terra::resample(data_in1, fishnet.r, method = 'bilinear')

data_fishnet2 = 
  terra::resample(data_in1, fishnet.r, method = 'med')

data_fishnet = c(data_fishnet1, data_fishnet2)

##For One-time processing to 0.03125 degree resolution
data_fishnet1 = 
  terra::resample(data_in1, fishnet.r.03125, method = 'med')

#Convert to dataframe
out.df = 
  as.data.frame(data_fishnet, xy = T) %>%
  dplyr::rename(hfp2000_bil = 3, hfp2020_bil = 4, hfp2000_med = 5, hfp2020_med = 6) %>%
  mutate(across(2:5, ~ round(.x, 2))) %>%
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

##############################
#Write Output
##############################

path2out = paste0(path2data ,"processed/")

if (dir.exists(path2out) == F) {
  dir.create(path2out)
}


fwrite(out.df, paste0(path2out ,"humanFootprint_2000_2020_05deg.csv"))

##For One-time processing to 0.03125 degree resolution
i = 1
list_of_names = names(data_fishnet1)
for(i in 1:length(list_of_names)){
  
  terra::writeRaster(data_fishnet1[[i]],
                     paste0(path2data, "processed/", names(data_fishnet1)[i], '_03125.tif'),
                     overwrite = T)
}

