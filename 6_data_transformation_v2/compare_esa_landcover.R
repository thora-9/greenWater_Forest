####################################
##Compare ESA Land Cover Outputs 
####################################

machine = '' #WB_VDM
source('0_environment/setEnvironment.R')

in_dir = paste0(database, "LULC/ESA_CCI_LC_2025/1_processed/")

esa_tree_cover = fread(paste0(in_dir, "ESA_tree_cover_by_year_05.csv"))

esa_tree_cover_2015 = 
  terra::rast(esa_tree_cover[, c('lon','lat', 'tree_cover_count_2015', 
                              'percent_tree_land_2015', 'percent_tree_total_2015')], type = "xyz")


esha_esa_2015_df = 
  read_dta('/Users/tejasvi/Downloads/2015_v1.dta') %>%
  merge(fishnet %>% st_drop_geometry(), by = 'Id')

esha_esa_2015_rast = 
  terra::rast(esha_esa_2015_df[, c('Lon', 'Lat', 'count_forest_esa', 'share_forest_esa')], type = "xyz")

# Write just this layer
writeRaster(esha_esa_2015_rast[['share_forest_esa']], paste0(in_dir, "esha_2015.tif"), overwrite=TRUE)
writeRaster(esa_tree_cover_2015[['percent_tree_total_2015']]/100, paste0(in_dir, "esa_lc_2015_v2.tif"), overwrite=TRUE)
writeRaster(esa_tree_cover_2015[['percent_tree_land_2015']]/100, paste0(in_dir, "esa_lc_land_2015_v2.tif"), overwrite=TRUE)

#For Scatter-Plots
all_2015 = 
  esa_tree_cover %>%
  merge(esha_esa_2015_df, by.x = c('lat', 'lon'), by.y = c('Lat', 'Lon'))

# Plot
ggplot(all_2015, aes(x = share_forest_esa - percent_tree_total_2015/100)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Difference in Forest Share \nin 2015 (old - new)",
       x = "Value", y = "Count") +
  theme_bw()
