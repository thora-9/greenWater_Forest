############################################################
##Checking Precip/SM data
############################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')
library(ncdf4)
library(zyp)

############################################################
data = 
  fread("/Users/tejasvi/Dropbox/WB/livable_planet/greenwater_forests_biodiv/esha-data/sm_precip_check_csv.csv") %>%
  drop_na() %>%
  group_by(OBJECTID) %>%
  mutate(normalized = (olc_ors_0100 - min(olc_ors_0100))/(max(olc_ors_0100) - min(olc_ors_0100))) %>%
  filter(OBJECTID == 24)


lm1 = lm(olc_ors_0100~precip_ann_total, data = data)
summary(lm1) #Review the results

############################################################
depth_weights = 
  data.frame(depth = c(0.05, 0.2, 0.4, 0.75),
             weight = c(10, 20, 20, 50))

#load the dataset
data_monthly = 
  tidync(paste0(database, 'Hydrology/Wang_2021_Soil_Moisture/olc_ors.nc')) %>%
  hyper_tibble() %>% as.data.table() 

#Weighted average to get sm for 0-100cm
data_monthly_0_100 = 
  data_monthly %>%
  merge(depth_weights, by = 'depth', all.x = T) %>%
  tidytable::group_by(lon, lat, time) %>%
  tidytable::summarise(weighted_sm = weighted.mean(sm, weight))
  
#add the time variable
data_monthly_0_100 = 
  data_monthly_0_100 %>%
  tidytable::mutate(date = as.Date(time, origin = '1850-01-01')) %>%
  tidytable::mutate(yearmon = substr(date, 1, 7),
                    year = substr(date, 1, 4),
                    month = substr(date, 6, 7))

#rescale data between 0 and 1
data_m_0_100_rescaled = 
  data_monthly_0_100 %>%
  tidytable::group_by(lon, lat) %>%
  tidytable::mutate(sm_0_100_rescaled = (weighted_sm - min(weighted_sm))/(max(weighted_sm) - min(weighted_sm))) %>%
  select(lon, lat, yearmon, year, month, sm_0_100 = weighted_sm, sm_0_100_rescaled)

#convert to yearly
data_y_0_100_rescaled = 
  data_m_0_100_rescaled %>%
  tidytable::group_by(lon, lat, year) %>%
  tidytable::summarise(sm_0_100 = mean(sm_0_100, na.rm = T),
                       sm_0_100_rescaled = mean(sm_0_100_rescaled, na.rm = T))

##############################    
#Check with existing dataframe to see if cells exist/coverage
#X coordinates
sum(data_y_0_100_rescaled$lon %in% fishnet$Lon) == nrow(data_y_0_100_rescaled)

#Y coordinates
sum(data_y_0_100_rescaled$lat %in% fishnet$Lat) == nrow(data_y_0_100_rescaled)

test2 = merge(data_y_0_100_rescaled, fishnet[, 1:4], by.x = c('lon', 'lat'), by.y = c('Lon', 'Lat'), all.x = T)

#Get the percent of cells missing 
100*sum(is.na(test2$objectid))/nrow(data_y_0_100_rescaled)

##############################
#Write Output
##############################
path2data = paste0(database, 'Hydrology/Wang_2021_Soil_Moisture/')
fwrite(data_m_0_100_rescaled, paste0(path2data, "processed/sm_data_monthly_1970_2016.csv"))
fwrite(data_y_0_100_rescaled, paste0(path2data, "processed/sm_data_yearly_1970_2016.csv"))



