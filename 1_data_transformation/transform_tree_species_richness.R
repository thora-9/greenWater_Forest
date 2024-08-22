##############################
##Local Tree Species
##############################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}


#Tree Species Richness layers 
rast_in = 
  rast(paste0(database, "Biodiversity/Richness/Tree_Species_Richness_Liang_2022/S_mean_raster.tif"))

rast_in_fishnet = 
  rast_in %>% 
  terra::resample(fishnet.r, method = 'bilinear') 

#Convert to dataframe
out.df = 
  as.data.frame(rast_in_fishnet, xy = T) %>%
  as.data.table() %>%
  dplyr::rename(mean_tree_species_richness_per_ha = 3)


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
fwrite(out.df, paste0(database, "Biodiversity/Richness/Tree_Species_Richness_Liang_2022/processed/tree_species_richness_05deg.csv"))
