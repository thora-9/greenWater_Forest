library(tidync)
library(data.table)
library(tidyverse)
library(lubridate)
library(zyp)
library(sf)
library(raster)
library(terra)
library(RColorBrewer)
library(rasterVis)
library(xts)
library(haven)
library(foreign)
library(stars)
library(ggnewscale)
library(ggpattern)
library(gridExtra)
library(ggrepel)
library(qgisprocess)
library(rnaturalearth)
library(leaflet)

############################################################################################
pathTyp = '/Users/tejasvi/Dropbox/WB/Typology/'
pathIn = '/Users/tejasvi/Dropbox/gwflagship_typologies/'
proj_dir = "/Users/tejasvi/Dropbox/WB/GRACE_Ensemble/"
pathOut =  "/Users/tejasvi/Dropbox/WB/GRACE-Deficit/"

############################################################################################


#paths
machine = 'local'
if(machine == 'gcp'){
  proj_dir = "/home/thora/Projects/greenWater_Forest/"
  database = "/home/thora/Database/"
} else{
  proj_dir = "~/Dropbox/WB/greenWater_Forest/"
  database = "~/Dropbox/Database/"
}


#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

#Basically scale the vector using the min/max of another vector
range01_other <- function(x, y, ...){(x - min(y, ...)) / (max(y, ...) - min(y, ...))}

##############################
####Load World Regions
wb_regions = 
  st_read(paste0(database, "Admin/WB_Regions/WB_countries_Admin0_10m.shp")) %>%
  dplyr::select(WB_NAME, ISO_A2, ISO_A3, ISO_N3, TYPE, REGION_WB) %>%
  filter(TYPE != 'Dependency') %>%
  st_make_valid()

world <- 
  ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != 'Antarctica')

#Load the fishnet
fishnet = 
  st_read(paste0(database,'Fishnet_halfdegree/global_fishnet.shp')) 

fishnet.r = 
  rast(paste0(database,'Fishnet_halfdegree/global_fishnet_raster.tif'))

##############################
#Human Footprint Data
path2data = paste0(database, "Biodiversity/HumanFootprint/HFI_Mu_2022/processed/")

data_in1 = 
  list.files(path2data, '.csv$', full.names = T) %>%
  fread() %>%
  #mutate(hfp2000_bil = ifelse(hfp2000_bil<1, 1, hfp2000_bil)) %>%
  mutate(rescl_2020 = range01(hfp2020_bil, na.rm = T),
         rescl_2000 = range01_other(hfp2000_bil, hfp2020_bil, na.rm = T)) %>% #Basically scale the vector using the min/max of another vector
  mutate(cut_2020 = cut(hfp2020_bil, c(0,1,4,Inf), labels = c('Wilderness', 'Intact', 'Modified')),
         cut_2000 = cut(hfp2020_bil, c(0,1,4,Inf), labels =  c('Wilderness', 'Intact', 'Modified'))) %>%
  filter(x != 179.75) #To remove weird line in the dataset

#Get the quantile values for non-linear distributions
valQuant1 = quantile(data_in1$rescl_2020, seq(0, 1, 0.1), na.rm = T)
valLin = seq(0, 1, 0.1)

#Global Map
plot1 = 
  data_in1 %>%
  dplyr::select(1,2, cur_var = hfp2020_bil) %>%
  drop_na() %>%
  ggplot() +
  geom_sf(data = world, lwd = 0, show.legend = FALSE) + 
  geom_tile(aes(x = x, y = y, fill = cur_var), alpha = 0.85) +
  scale_fill_viridis_c(name = 'Human Footprint\n(2020; rescaled)\n',
                       option = "viridis") +
  # scale_fill_distiller(name = 'Human Footprint\n(2020; rescaled)\n',
  #                      values = valQuant1,
  #                      palette = 'RdYlGn') +
  #scale_fill_gradientn(colors = colQuant) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.15, .3),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title=element_text(size=16), 
        legend.key.size = unit(0.8, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pathOut = paste0(proj_dir, "Figures/")
  
# ggsave(paste0(pathOut, 'HumanFootprint_2020', '.png'), plot=plot1,
#        scale=1.5, dpi=300,width =34.85,height = 18, units = 'cm')

##############################
#BERI
path2data = paste0(database, "Biodiversity/BERI/Harwood_Tom_22_May_2024/processed")

data_in1 = 
  list.files(path2data, '.csv$', full.names = T) %>%
  fread() %>%
  #mutate(hfp2000_bil = ifelse(hfp2000_bil<1, 1, hfp2000_bil)) %>%
  mutate(rescl_2020 = range01(BERI2020_bil, na.rm = T),
         rescl_2000 = range01_other(BERI2000_bil, BERI2020_bil, na.rm = T)) %>% #Basically scale the vector using the min/max of another vector
  filter(x != 179.75) #To remove weird line in the dataset

#Get the quantile values for non-linear distributions
valQuant1 = quantile(data_in1$rescl_2020, seq(0, 1, 0.1), na.rm = T)
valLin = seq(0, 1, 0.1)

#Global Map
plot1 = 
  data_in1 %>%
  dplyr::select(1,2, cur_var = BERI2020_bil) %>%
  drop_na() %>%
  ggplot() +
  geom_sf(data = world, lwd = 0, show.legend = FALSE) + 
  geom_tile(aes(x = x, y = y, fill = cur_var), alpha = 0.85) +
  # scale_fill_viridis_c(name = 'BERI\n(2020)\n',
  #                      option = "viridis") +
  scale_fill_distiller(name = 'BERI\n(2020)\n',
                       values = valQuant1, direction = 1,
                       palette = 'RdYlGn') +
  #scale_fill_gradientn(colors = colQuant) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.15, .3),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title=element_text(size=16), 
        legend.key.size = unit(0.8, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pathOut = paste0(proj_dir, "Figures/")

# ggsave(paste0(pathOut, 'BERI_2020', '.png'), plot=plot1,
#        scale=1.5, dpi=300,width =30.85,height = 18, units = 'cm')

##############################
#Forest Fragmentation
path2data = paste0(database, "Forestry/forest_fragmentation_Ma_2023/processed")

data_in1 = 
  list.files(path2data, 'forest_frag_05_v2.csv$', full.names = T) %>%
  fread() %>%
  #mutate(hfp2000_bil = ifelse(hfp2000_bil<1, 1, hfp2000_bil)) %>%
  mutate(rescl_2020 = range01(FFI2020_med, na.rm = T),
         rescl_2000 = range01_other(FFI2000_med, FFI2020_med, na.rm = T)) %>% #Basically scale the vector using the min/max of another vector
  filter(x != 179.75) #To remove weird line in the dataset

#Get the quantile values for non-linear distributions
valQuant1 = quantile(data_in1$rescl_2020, seq(0, 1, 0.1), na.rm = T)
valLin = seq(0, 1, 0.1)

#Global Map
plot1 = 
  data_in1 %>%
  dplyr::select(1,2, cur_var = FFI2020_med) %>%
  drop_na() %>%
  ggplot() +
  geom_sf(data = world, lwd = 0, show.legend = FALSE) + 
  geom_tile(aes(x = x, y = y, fill = cur_var), alpha = 0.85) +
  # scale_fill_viridis_c(name = 'BERI\n(2020)\n',
  #                      option = "viridis") +
  scale_fill_distiller(name = 'FFI\n(2020)\n',
                       values = valQuant1, direction = -1,
                       palette = 'RdYlBu') +
  #scale_fill_gradientn(colors = colQuant) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.15, .3),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title=element_text(size=16), 
        legend.key.size = unit(0.8, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pathOut = paste0(proj_dir, "Figures/")

# ggsave(paste0(pathOut, 'FFI_2020', '.png'), plot=plot1,
#        scale=1.5, dpi=300,width =30.85,height = 18, units = 'cm')


##############################
#Biodiversity Intactness Index
path2data= paste0(database, "Biodiversity/Intactness/BII_Newbold_2016/processed")

data_in1 = 
  list.files(path2data, '.csv$', full.names = T) %>%
  fread() %>%
  filter(x != 179.75) #To remove weird line in the dataset

#Get the quantile values for non-linear distributions
valQuant1 = quantile(data_in1$BII_med, seq(0, 1, 0.1), na.rm = T)
valLin = seq(0, 1, 0.1)

#Global Map
plot1 = 
  data_in1 %>%
  dplyr::select(1,2, cur_var = BII_med) %>%
  drop_na() %>%
  ggplot() +
  geom_sf(data = world, lwd = 0, show.legend = FALSE) + 
  geom_tile(aes(x = x, y = y, fill = cur_var), alpha = 0.85) +
  scale_fill_viridis_c(name = 'BII\n', direction = -1,
                       option = "viridis") +
  # scale_fill_distiller(name = 'BII\n(2020)\n',
  #                      values = valQuant1, direction = -1,
  #                      palette = 'RdYlBu') +
  #scale_fill_gradientn(colors = colQuant) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position="right",
        legend.position = c(.15, .3),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text=element_text(size=16), legend.title=element_text(size=16), 
        legend.key.size = unit(0.8, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pathOut = paste0(proj_dir, "Figures/")

ggsave(paste0(pathOut, 'BII', '.png'), plot=plot1,
       scale=1.5, dpi=300,width =30.85,height = 18, units = 'cm')


