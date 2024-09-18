##############################
##Koppen Classification
##############################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}

path2data = paste0(database, 'Climate/Koppen_Classification/Beck_Koppen_2023/')

#Load the layer to be used - 1991-2020 climate classes
input_layer = rast(paste0(path2data, '1991_2020/koppen_geiger_0p5.tif'))

#Load the lookup table
lookup = fread(paste0(path2data, 'lookup_table.csv'))

input_layer_fishnet = 
  input_layer %>% 
  terra::resample(fishnet.r, method = 'near') %>%
  mask(fishnet.r)

#Convert to dataframe
out.df = 
  as.data.frame(input_layer, xy = T) %>%
  as.data.table() %>%
  merge(lookup, by.x = 'koppen_geiger_0p5', by.y = 'Code', all.x = T) %>%
  filter(x %in% fishnet$Lon & y %in% fishnet$Lat) %>%
  dplyr::select(x,y,Code = koppen_geiger_0p5, Class, Description)

##############################    
#Check with existing dataframe to see if cells exist/coverage
test = read_dta(paste0(proj_dir, "esha-data/econ_greenwater_paper data/fishnet_upstream_share_forest_esa_1992.dta"))

#X coordinates
sum(out.df$x %in% test$longitude) == nrow(out.df)

#Y coordinates
sum(out.df$y %in% test$latitude) == nrow(out.df)

test2 = merge(out.df, test[, 1:4], by.x = c('x', 'y'), by.y = c('longitude', 'latitude'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(out.df)

##############################
#Write Output
##############################
fwrite(out.df, paste0(path2data, "processed/koppen_classification_05deg.csv"))
