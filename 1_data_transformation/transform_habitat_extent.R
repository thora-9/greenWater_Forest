##############################
##Forest Management
##############################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}


#Habitat layers 
layer_list = 
  list.files(paste0(database, "LULC/Habitat_Jung_2020/"), ".tif$",
             full.names = T)

layer_list_names = 
  list.files(paste0(database, "LULC/Habitat_Jung_2020/"), ".tif$",
             full.names = F)

habitat_layers = 
  rast(layer_list)

#Create a raster with value 1; and use that to get the total number of cells
rast_for_proportion = habitat_layers$iucn_habitatclassification_fraction_lvl1__100_Forest__ver003
values(rast_for_proportion) = 1
names(rast_for_proportion) = "for_proportion"

habitat_layers = c(habitat_layers, rast_for_proportion)

habitat_layers_fishnet = 
  habitat_layers %>% 
  terra::resample(fishnet.r, method = 'sum') 

names(habitat_layers_fishnet) = c("Forests","Artificial",
                                  "Savanna", "Shrubland",
                                  "Grassland","Wetland_Inland",
                                  "Rocky Areas", "Desert", "for_proportion")

#Convert to dataframe
out.df = 
  as.data.frame(habitat_layers_fishnet, xy = T) %>%
  as.data.table() %>%
  tidytable::rowwise() %>%
  tidytable::mutate(sum_na = sum(is.na(c_across()))) %>%
  tidytable::filter(sum_na != 8) %>%
  tidytable::mutate(Habitat = sum(c(Forests,Savanna,Shrubland,Grassland,Wetland_Inland,`Rocky Areas`,Desert), na.rm = T)) %>%
  tidytable::mutate(across(Forests:Desert, ~(.x/(1000*for_proportion)), .names = "{.col}_proportion"),
                    Habitat_proportion = Habitat/(1000*for_proportion)) %>%
  tidytable::select(x,y,Forests_proportion:Habitat_proportion)
  

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
fwrite(out.df, paste0(database, "LULC/Habitat_Jung_2020/processed/habitat_extent_05deg.csv"))
