make_forest_cover_plots <- function(cur_data, proj_dir, region_bound, fishnet.r, wb_regions, out_dir) {
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(forcats)
  library(qgisprocess)
  
  # --------------------------
  # 1. File paths and labels
  # --------------------------
  if (cur_data == "non_upstream") {
    data_path <- "/Users/tejasvi/Dropbox/Database/LULC/ESA_CCI_LC_2025/1_processed/ESA_tree_cover_by_year_05.csv"
    plot_legend <- "Forest Cover\n(%change 2000-2022)"
    plot_names <- c("West_Africa_nonUp_forest_change_CAT",
                    "West_Africa_nonUp_forest_change_BIN",
                    "West_Africa_nonUp_forest_change_CONT",
                    "West_Africa_nonUp_forest_change_DENSITY")
  } else if (cur_data == "upstream_100") {
    data_path <- file.path(proj_dir, "Upstream/Africa/250807/upstream_ESA_LC_af_allWS_w100km.csv")
    plot_legend <- "Upstream Forest\nCover (%change 2000-2022)"
    plot_names <- c("West_Africa_Up100_forest_change_CAT",
                    "West_Africa_Up100_forest_change_BIN",
                    "West_Africa_Up100_forest_change_CONT",
                    "West_Africa_Up100_forest_change_DENSITY")
  } else if (cur_data == "upstream_all") {
    data_path <- file.path(proj_dir, "Upstream/Africa/250807/upstream_ESA_LC_af_allWS_nodist.csv")
    plot_legend <- "Upstream Forest Cover\n(%change 2000-2022)"
    plot_names <- c("West_Africa_UpAll_forest_change_CAT",
                    "West_Africa_UpAll_forest_change_BIN",
                    "West_Africa_UpAll_forest_change_CONT",
                    "West_Africa_UpAll_forest_change_DENSITY")
  } else {
    stop("Invalid cur_data. Choose from 'non_upstream', 'upstream_100', or 'upstream_all'.")
  }
  
# --------------------------
# 2. Read & preprocess data
# --------------------------
forest_cover_change <- fread(data_path)

# handle coordinate column naming differences
if ("lon" %in% names(forest_cover_change) & "lat" %in% names(forest_cover_change)) {
  forest_cover_change <- forest_cover_change %>%
    rename(Lon = lon, Lat = lat)
} else if ("Lon" %in% names(forest_cover_change) & "Lat" %in% names(forest_cover_change)) {
  forest_cover_change <- forest_cover_change
} else {
  stop("Could not find Lon/Lat coordinate columns in dataset.")
}

forest_cover_change <- forest_cover_change %>%
  mutate(
    perc_change_land  = 100 * (percent_tree_land_2022  - percent_tree_land_2000)  / percent_tree_land_2000,
    perc_change_total = 100 * (percent_tree_total_2022 - percent_tree_total_2000) / percent_tree_total_2000,
    abs_change_total  = percent_tree_total_2022 - percent_tree_total_2000
  ) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

  
  # spatial join with regions
  qgis_join <- qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT     = forest_cover_change,
    JOIN      = region_bound,
    PREDICATE = 5 # within
  )
  
  forest_cover_change <- st_read(qgis_extract_output(qgis_join, "OUTPUT")) %>%
    mutate(
      perc_change_trunc = ifelse(perc_change_total > 100, 100, perc_change_total),
      perc_change_cat   = cut(
        perc_change_trunc,
        breaks = c(-100, -50, -10, 0, 10, 50, 102),
        labels = c("≤ -50", "-50 to -10", "-10 to 0", "0 to 10", "10 to 50", "> 50"),
        include.lowest = TRUE,
        right = TRUE
      ),
      perc_change_cat   = fct_explicit_na(perc_change_cat, na_level = "No Forest Extent (2000)"),
      abs_change_cat    = cut(
        perc_change_trunc,
        breaks = c(-100, -1, 1, 1000),
        labels = c("Forest Loss", "No Change", "Forest Gain"),
        include.lowest = TRUE,
        right = TRUE
      ),
      abs_change_cat    = fct_explicit_na(abs_change_cat, na_level = "No Forest Extent (2000)")
    ) %>%
    filter(!is.na(WB_NAME)) %>%
    cbind(st_coordinates(.))
  
  # fishnet for plotting
  fishnet_for_plot <- fishnet.r %>%
    as.data.frame(xy=TRUE) %>% as.data.table() %>%
    rename(X = 1, Y = 2) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    cbind(st_coordinates(.))
  
  # --------------------------
  # 3. Color scales
  # --------------------------
  scale_cat <- scale_fill_manual(
    values = c(
      "≤ -50"       = "#800026",  "-50 to -10"  = "#e31a1c",
      "-10 to 0"    = "#fdae61",  "0 to 10"     = "#d9f0a3",
      "10 to 50"    = "#78c679",  "> 50"        = "#1a9850",
      "No Forest Extent (2000)" = "grey90"
    ),
    drop = FALSE, guide = guide_legend(reverse = TRUE), name = plot_legend
  )
  
  scale_bin <- scale_fill_manual(
    values = c(
      "Forest Loss" = "#800026", "No Change" = "#d6c6aeff",
      "Forest Gain" = "#1a9850", "No Forest Extent (2000)" = "grey90"
    ),
    drop = FALSE, guide = guide_legend(reverse = TRUE),
    name = "Forest Cover\n(change 2000-2022)"
  )
  
  pretty_breaks <- pretty(-100:100, n = 10)
  scale_cont <- scale_fill_gradientn(
    colours = c("#800026","#e31a1c","#fdae61","#d9f0a3","#78c679","#1a9850"),
    limits  = c(-100, 100),
    oob     = scales::squish,
    na.value = "grey90",
    guide   = guide_colorbar(ticks = TRUE, nbin = 100, label = pretty_breaks),
    name    = plot_legend
  )
  
  # --------------------------
  # 4. Plots
  # --------------------------
  base_map <- list(
    geom_tile(data = fishnet_for_plot, mapping = aes(x = X, y = Y), fill = "#ffffffff"),
    geom_sf(data = wb_regions, fill = NA, linewidth = 0.3, linetype = 1),
    theme_bw(),
    xlim(-22, 32), ylim(-7, 32),
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank(),
      legend.title = element_text(size=16, vjust = 2, face="bold"),
      legend.title.align = 0,
      axis.ticks = element_blank(),
      legend.position = c(0.29, .17),
      legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6),
      legend.text = element_text(size=16), legend.key.size = unit(0.8, "cm"),
      panel.background = element_blank(), axis.line = element_blank(),
      plot.margin = unit(c(-30,-30,-10,-30),"mm")
    )
  )
  
  plot1 <- ggplot() + base_map +
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = perc_change_cat), alpha = 0.8) +
    scale_cat
  
  plot2 <- ggplot() + base_map +
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = abs_change_cat), alpha = 0.8) +
    scale_bin
  
  plot3 <- ggplot() + base_map +
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = perc_change_trunc), alpha = 0.8) +
    scale_cont
  
  plot4 <- ggplot(forest_cover_change, mapping = aes(x = perc_change_trunc)) +
    geom_density(fill = "#147bb3ff", alpha = 0.5) + ylim(0, 0.075) +
    scale_x_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 20)) +
    labs(x = "Percent change", y = "Density") + theme_bw()
  
  # --------------------------
  # 5. Save plots
  # --------------------------
  ggsave(file.path(out_dir, paste0(plot_names[1], ".png")), plot = plot1,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  ggsave(file.path(out_dir, paste0(plot_names[2], ".png")), plot = plot2,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  ggsave(file.path(out_dir, paste0(plot_names[3], ".png")), plot = plot3,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  ggsave(file.path(out_dir, paste0(plot_names[4], ".png")), plot = plot4,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  
  invisible(list(cat = plot1, bin = plot2, cont = plot3, density = plot4))
}
