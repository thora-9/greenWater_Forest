#########################################################
##Merge Contients
#########################################################

machine = '' #WB_VDM

source('0_environment/setEnvironment.R')


cur_folder_name = '250807'
cur_run = 'upstream' #unconnected #upstream
run_name = 'ESA_LC_'
dist_threshold = 'nodist' #nodist #w100km

#########################################################
#########################################################
cur_continent = 'North America'
cur_continent_abr = 'NA'
cur_continent_abr_UP = 'na'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_a = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))

cur_continent = 'South America'
cur_continent_abr = 'SA'
cur_continent_abr_UP = 'sa'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_b = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))

cur_continent = 'Africa'
cur_continent_abr = 'AF'
cur_continent_abr_UP = 'af'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_c = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))

cur_continent = 'Asia'
cur_continent_abr = 'AS'
cur_continent_abr_UP = 'as'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_d = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))

cur_continent = 'Australasia'
cur_continent_abr = 'AU'
cur_continent_abr_UP = 'au'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_e = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))

cur_continent = 'Europe'
cur_continent_abr = 'EU'
cur_continent_abr_UP = 'eu'
path2output= paste0(proj_dir, 'Upstream/', cur_continent, '/', cur_folder_name,'/')
test_f = fread(paste0(path2output, 'upstream_', run_name, cur_continent_abr_UP, '_allWS_', 
                                 dist_threshold, '.csv'))


test_all = rbind(test_a, test_b, test_c, test_d, test_e, test_f)
length(unique(test_all$Id)) == nrow(test_all)

dupes = 
  test_all %>%
  group_by(Id) %>%
  filter(n() > 1) %>%
  ungroup()

#fwrite(dupes, paste0(proj_dir, 'Upstream/', 'dupes.csv'))

##############################################
change_raster = 
  test_all %>%
  tidytable::select(Lon, Lat, percent_tree_total_1992, percent_tree_total_2022) %>%
  tidytable::mutate(change_perc = (percent_tree_total_2022 - percent_tree_total_1992)) %>%
  tidytable::mutate(change_perc_trunc = ifelse(abs(change_perc) > 2, change_perc, NA)) %>%
  tidytable::mutate(change_perc_trunc = pmin(pmax(change_perc_trunc, -20), 20)) %>%
  as.data.frame() %>%
  rast(type = 'xyz', crs="EPSG:4326")

p <- 
  change_raster %>%
  as.data.frame(xy = T) %>%
  ggplot(aes(x=x, y=y, fill=percent_tree_total_1992)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_bw()

out_location = '/Users/tejasvi/Dropbox/WB/livable_planet/greenwater_forests_biodiv/Upstream/ESA_CCI_LC/'
ggsave(paste0(out_location, '1992_forest_cover_total', '.png'), plot=p,
       scale=1.5, dpi=300,width =24.85,height = 18, units = 'cm')


p <- 
  change_raster %>%
  as.data.frame(xy = T) %>%
  ggplot(aes(x=x, y=y, fill=percent_tree_total_2022)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_bw()

out_location = '/Users/tejasvi/Dropbox/WB/livable_planet/greenwater_forests_biodiv/Upstream/ESA_CCI_LC/'
ggsave(paste0(out_location, '2022_forest_cover_total', '.png'), plot=p,
       scale=1.5, dpi=300,width =24.85,height = 18, units = 'cm')


p <- 
  change_raster %>%
  as.data.frame(xy = T) %>%
  ggplot(aes(x=x, y=y, fill=change_perc_trunc)) +
  geom_tile() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  coord_equal() +
  ylim(-60, 60) + xlim(-130, 180) +
  theme(
  legend.position = c(0.1, 0.2),        # x, y in npc coordinates
  legend.background = element_rect(fill = alpha("white", 0.6))) # semi-transparent box

out_location = '/Users/tejasvi/Dropbox/WB/livable_planet/greenwater_forests_biodiv/Upstream/ESA_CCI_LC/'
ggsave(paste0(out_location, 'change_1992_2022', '.png'), plot=p,
       scale=1.5, dpi=300,width =24.85,height = 18, units = 'cm')


p <- 
  change_raster %>%
  as.data.frame(xy = T) %>%
  ggplot(aes(x=change_perc_trunc)) +
  geom_density(fill="skyblue", alpha=0.6, color="blue")
