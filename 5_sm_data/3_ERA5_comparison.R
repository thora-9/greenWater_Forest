############################################################
##Estimating TWSA Trends 
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)
library(ggnewscale)
library(spatialEco)
library(trend)

##########################################################################################
#GRACE data

path2data = "/Users/tejasvi/Dropbox/Database/Hydrology/era5_land_soil_moisture/"
path2out = "/Users/tejasvi/Dropbox/Database/Hydrology/era5_land_soil_moisture/processed/"
path2Wang = "/Users/tejasvi/Dropbox/Database/Hydrology/Wang_2021_Soil_Moisture/processed/"


#The GRACE Mascon dataset
df_era =
  tidync(paste0(path2out,'swvl_yearly_avg.nc')) %>%
  activate("D2,D1,D0") %>%
  hyper_tibble() %>% 
  as.data.table() %>%
  .[order(lat, lon)] %>% #ensures the order is the same
  tidytable::group_by(lat,lon) %>%
  as.data.table()


dates = 
  tidync(paste0(path2out,'swvl_yearly_avg.nc')) %>%
  activate("D0")  %>%
  hyper_tibble() %>% as.data.table() %>%
  mutate(dates = as.Date(time, origin = '1979-01-01')) %>% 
  mutate(ym = substr(ymd(dates), 1, 7)) %>%
  mutate(year = substr(ymd(dates), 1, 4)) %>%
  mutate(month = substr(ymd(dates), 6, 7)) %>%
  select(-dates)

#Merged Dataset
df_era = 
  df_era %>%
  merge(dates, by = 'time', all.x = T)


df_wang = 
  tidync(paste0(path2Wang,'yearly_avg.nc')) %>%
  activate("D2,D1,D0") %>%
  hyper_tibble() %>% 
  as.data.table() %>%
  .[order(lat, lon)] %>% #ensures the order is the same
  tidytable::group_by(lat,lon) %>%
  as.data.table()

dates = 
  tidync(paste0(path2Wang,'yearly_avg.nc')) %>%
  activate("D0")  %>%
  hyper_tibble() %>% as.data.table() %>%
  mutate(dates = as.Date(time, origin = '1970-01-01')) %>% 
  mutate(ym = substr(ymd(dates), 1, 7)) %>%
  mutate(year = substr(ymd(dates), 1, 4)) %>%
  select(-dates)

#Merged Dataset
df_wang = 
  df_wang %>%
  merge(dates, by = 'time', all.x = T)

################################################################################
################################################################################
#Processing the CSV output from python script for Esha
#DF Wang (shared by Esha)
test = 
  fread("/Users/tejasvi/Dropbox/Database/Hydrology/Wang_2021_Soil_Moisture/soil_moisture.csv") %>%
  rename(lat = Lat, lon = Lon)

#DF ERA
test2 = fread("/Users/tejasvi/Dropbox/Database/Hydrology/era5_land_soil_moisture/processed/ERA5_land_sm_data_05deg_1979_2024.csv")
test_ESHA = 
  test2 %>%
  semi_join(test, by = c("lon", "lat"))

fwrite(test_ESHA, 
       "/Users/tejasvi/Dropbox/Database/Hydrology/era5_land_soil_moisture/processed/ERA5_land_sm_data_05deg_1979_2024.csv")

test3 = test_ESHA %>% filter(year == 1979)

test_raster = 
  rasterFromXYZ(test3[,.(lon, lat, swvl_0_100)]) %>%
  rast()
crs(test_raster) = crs(fishnet.r)
################################################################################
################################################################################
#Assess correlation/trend by group
cor_all = 
  merged_all %>%
  tidytable::group_by("lon", "lat") %>%
  tidytable::summarise(correlation = cor(swvl_0_100, sm_0_100),
                       mean_era = mean(swvl_0_100),
                       mean_wang = mean(sm_0_100)) %>%
  mutate(mean_diff = mean_era - mean_wang)

################################################################################
# Convert to long format
cor_all_long <- cor_all %>%
  pivot_longer(cols = c(mean_era, mean_wang),
               names_to = "metric",
               values_to = "value")

# Create box plot
# Create a boxplot to compare the distributions
ggplot(cor_all_long, aes(x = metric, y = value, fill = metric)) + 
  geom_boxplot() + 
  labs(title = "Distribution of mean annual SM (per pixel)",
       x = "Group", 
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen")) # Customize colors

# Create box plot
# Create a boxplot to compare the distributions
ggplot(cor_all, aes(x = '', y = correlation, fill = 'lightgreen')) + 
  geom_boxplot() + 
  labs(title = "Distribution of Correlation (pixel-wise)",
       x = "", 
       y = "Correlation") +
  theme_minimal()

 ################################################################################
cor_raster = 
  rasterFromXYZ(cor_all[,.(lon, lat, correlation)]) %>%
  rast()
crs(cor_raster) = crs(fishnet.r)

# Convert raster to data frame for ggplot
r_df <- as.data.frame(cor_raster, xy = TRUE, na.rm = TRUE)

# Plot the correlation raster with a custom color scale
ggplot(r_df, aes(x = x, y = y, fill = correlation)) +
  geom_tile() +
  # Custom color scale: blue (-1), white (0), red (1)
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-1, 1), 
                       name = "Correlation") +
  coord_fixed(ratio = 1) +  # To keep the aspect ratio of the plot correct
  theme_minimal() +
  labs(title = "Correlation between ERA/Wang") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom")  # Adjust legend position if needed

################################################################################
diff_raster = 
  rasterFromXYZ(cor_all[,.(lon, lat, mean_diff)]) %>%
  rast()

crs(diff_raster) = crs(fishnet.r)

# Convert raster to data frame for ggplot
r_df <- as.data.frame(diff_raster, xy = TRUE, na.rm = TRUE)

# Plot the raster data using ggplot
ggplot(r_df, aes(x = x, y = y, fill = mean_diff)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", 
                       limits = c(min(r_df$mean_diff, na.rm = TRUE), max(r_df$mean_diff, na.rm = TRUE)),
                       name = "Value") +
  coord_fixed(ratio = 1) +  # To keep the aspect ratio of the plot correct
  theme_minimal() +
  labs(title = "Difference in mean annual SM: 1979 to 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
