make_abs_forest_cover_plots <- function(cur_data, proj_dir, region_bound, fishnet.r, wb_regions, out_dir) {
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
    plot_legend <- "Change in forest cover\n(percentage points, 1992–2022)"
    plot_names <- c("West_Africa_nonUp_absForest_change_CAT",
                    "West_Africa_nonUp_absForest_change_BIN",
                    "West_Africa_nonUp_absForest_change_CONT",
                    "West_Africa_nonUp_absForest_change_DENSITY")
  } else if (cur_data == "upstream_100") {
    data_path <- file.path(proj_dir, "Upstream/Africa/250807/upstream_ESA_LC_af_allWS_w100km.csv")
    plot_legend <- "Upstream Forest Change\n(percentage points, 1992–2022)"
    plot_names <- c("West_Africa_Up100_absForest_change_CAT",
                    "West_Africa_Up100_absForest_change_BIN",
                    "West_Africa_Up100_absForest_change_CONT",
                    "West_Africa_Up100_absForest_change_DENSITY")
  } else if (cur_data == "upstream_all") {
    data_path <- file.path(proj_dir, "Upstream/Africa/250807/upstream_ESA_LC_af_allWS_nodist.csv")
    plot_legend <- "Upstream Forest Change\n(percentage points, 1992–2022)"
    plot_names <- c("West_Africa_UpAll_absForest_change_CAT",
                    "West_Africa_UpAll_absForest_change_BIN",
                    "West_Africa_UpAll_absForest_change_CONT",
                    "West_Africa_UpAll_absForest_change_DENSITY")
  } else {
    stop("Invalid cur_data. Choose from 'non_upstream', 'upstream_100', or 'upstream_all'.")
  }
  
  # --------------------------
  # 2. Read & preprocess data
  # --------------------------
  forest_cover_change <- fread(data_path)

  if ("lon" %in% names(forest_cover_change) & "lat" %in% names(forest_cover_change)) {
    forest_cover_change <- forest_cover_change %>%
      rename(Lon = lon, Lat = lat)
  } else if (!("Lon" %in% names(forest_cover_change) & "Lat" %in% names(forest_cover_change))) {
    stop("Could not find Lon/Lat coordinate columns in dataset.")
  }
  
  forest_cover_change <- forest_cover_change %>%
    mutate(
      abs_change_total = percent_tree_total_2022 - percent_tree_total_1992
    ) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # join to regions
  qgis_join <- qgis_run_algorithm(
    "native:joinattributesbylocation",
    INPUT     = forest_cover_change,
    JOIN      = region_bound,
    PREDICATE = 5
  )
  
  forest_cover_change <- st_read(qgis_extract_output(qgis_join, "OUTPUT")) %>%
  mutate(
    abs_change_total = percent_tree_total_2022 - percent_tree_total_1992,
    
    # categorical breaks
    abs_change_cat = cut(
      abs_change_total,
      breaks = c(-100, -20, -5, -1, 1, 5, 20, 100),
      labels = c("≤ -20", "-20 to -5", "-5 to -1", "-1 to 1", "1 to 5", "5 to 20", "> 20"),
      include.lowest = TRUE, right = TRUE
    ),
    abs_change_cat = fct_explicit_na(abs_change_cat, na_level = "No Forest Extent (1992)"),
    
    # binary version
    abs_change_bin = cut(
      abs_change_total,
      breaks = c(-100, -1, 1, 100),
      labels = c("Loss", "No Change", "Gain"),
      include.lowest = TRUE, right = TRUE
    ),
    abs_change_bin = fct_explicit_na(abs_change_bin, na_level = "No Forest Extent (1992)")
  ) %>%
  filter(!is.na(WB_NAME)) %>%
  cbind(st_coordinates(.))

  
  # fishnet
  fishnet_for_plot <- fishnet.r %>%
    as.data.frame(xy=TRUE) %>% as.data.table() %>%
    rename(X = 1, Y = 2) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    cbind(st_coordinates(.))
  
  # --------------------------
  # 3. Scales
  # --------------------------
  scale_cat <- scale_fill_manual(
    values = c(
      "≤ -20" = "#800026", "-20 to -5" = "#e31a1c",
      "-5 to -1" = "#fd8d3c", "-1 to 1" = "#f7f7f7",
      "1 to 5" = "#a1d99b", "5 to 20" = "#31a354",
      "> 20" = "#006837",
      "No Forest Extent (1992)" = "grey90"
    ),
    drop = FALSE, guide = guide_legend(reverse = TRUE),
    name = plot_legend
  )
  
  scale_cont <- scale_fill_gradientn(
    colours = c("#800026","#e31a1c","#fdae61","#f7f7f7","#a1d99b","#31a354","#006837"),
    limits = c(-30, 30),
    oob = scales::squish,
    na.value = "grey90",
    name = plot_legend
  )
  
  scale_bin <- scale_fill_manual(
  values = c(
    "Loss" = "#800026",        # dark red
    "No Change" = "#d6c6aeff", # light neutral
    "Gain" = "#1a9850",        # dark green
    "No Forest Extent (1992)" = "grey90"
  ),
  drop = FALSE,
  guide = guide_legend(reverse = TRUE),
  name = "Forest Cover\n(change 1992-2022)"
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
      axis.text = element_blank(), axis.title = element_blank(),
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
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = abs_change_cat), alpha = 0.8) +
    scale_cat   # categorical

  plot2 <- ggplot() + base_map +
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = abs_change_bin), alpha = 0.8) +
    scale_bin   # binary

  plot3 <- ggplot() + base_map +
    geom_tile(data = forest_cover_change, mapping = aes(x = X, y = Y, fill = abs_change_total), alpha = 0.8) +
    scale_cont  # continuous

  plot4 <- ggplot(forest_cover_change, mapping = aes(x = abs_change_total)) +
    geom_density(fill = "#147bb3ff", alpha = 0.5) +
    scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 5)) +
    ylim(0, 0.2) +
    labs(x = "Absolute change in forest cover (%)", y = "Density") +
    theme_bw()

  
  # --------------------------
  # 5. Save
  # --------------------------
  ggsave(file.path(out_dir, paste0(plot_names[1], ".png")), plot = plot1,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  ggsave(file.path(out_dir, paste0(plot_names[2], ".png")), plot = plot2,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  ggsave(file.path(out_dir, paste0(plot_names[3], ".png")), plot = plot3,
         scale=1.5, dpi=300, width=24.85, height=18, units="cm")
  
  invisible(list(cat = plot1, cont = plot2, density = plot3))
}
