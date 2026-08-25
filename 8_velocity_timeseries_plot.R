# Plotting median climate velocity timeseries per SSP-term combo
  # Written by Alice P
    # 2 April 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") 
  df_fol <- make_folder(source_disk, "3_velocity_decadal_median_timeseries", "dfs") 
  plot_fol <- make_folder(source_disk, "6_velocity_aus_plot", "timeseries")
  fig_fol <- make_folder(source_disk, "_figures", "")
  

  
  
# Get the extent into thirds to split into 3 regions ---------------------------
  
  all_files <- dir(vocc_fol, full.names = TRUE)
  
  f <- all_files[2]
  d <- readRDS(f) # Can use any file
  ext(d) # SpatExtent : 105, 175, -50, -5 (xmin, xmax, ymin, ymax)
  
  q <- quantile(c(ext(d)[3], ext(d)[4]), c(1/3, 2/3))
  q 
    # 33.33333% 66.66667% 
    #       -35       -20 
  ## Roughly splitting data into regions based on these lats
    # North: -5 to -20
    # Mid: -20 to -35
    # South: -35 to -50
  
  
  

# Create plotting df -----------------------------------------------------------
  
  df_comb <- readRDS(paste0(df_fol, "/velocity_decadal_timeseries-median_cropped_df_combined_ssp-esm-term.RDS"))
  
  df_comb %>% 
    filter(term != "recent")
  
  
  
  ## *Maybe* should weight it by number of grid cells (rows?) in each cat, if they are very different? -----------
    df_comb %>%
      distinct(x, y, cat) %>%
      count(cat)
    # # A tibble: 3 × 2
    #     cat       n
    #     <chr> <int>
    #   1 mid    4640
    #   2 north  3604
    #   3 south  3201
      ## Ehh. I'll just state it on the figure for now.
  
  
  
  ## Get median of each ESM per year, per region (and ssp, term) -----------
  
  esm_meds <- df_comb %>%
    group_by(year, ssp, term, cat, esm) %>%
    summarise(esm_median = median(velocity, na.rm = TRUE), .groups = "drop")
  
  
  
  ## Get min and max of the ESM medians for the ribbon, per region per year (and ssp, term) -----------
  
  plot_df <- esm_meds %>%
    group_by(year, ssp, term, cat) %>%
    summarise(
      med_velocity = median(esm_median, na.rm = TRUE),  # med of ESM medians
      lo_velocity  = min(esm_median, na.rm = TRUE), # min ESM median 
      hi_velocity  = max(esm_median, na.rm = TRUE), # max ESM median
      .groups = "drop"
    )
  
  plot_df %>% 
    filter(year == 2021)
  
  ssp_cols <- c(#"historical" = "#5F5E5A",
    "ssp126" = col_ssp126,
    "ssp245" = col_ssp245,
    "ssp370" = col_ssp370,
    "ssp585" = col_ssp585)
  
  
  
  
# Getting stats for quoting in result text -------------------------------------
  
  # min and max ribbon and line values at 2090
    plot_df %>%  
      filter(year == 2090) %>% 
      group_by(ssp) %>% 
      summarise(min_ribbon = min(lo_velocity),
                max_ribbon = max(hi_velocity),
                min_median = min(med_velocity),
                max_median = max(med_velocity)) %>% 
      mutate(year = 2090) 
  
  # Same for the historical period
    plot_df %>% 
      group_by(ssp) %>% 
      filter(ssp == "historical") %>% 
      summarise(min_median = min(med_velocity),
                max_median = max(med_velocity)) 
  
  # Across the whole timeseries, min/max values
    plot_df %>% 
      group_by(ssp) %>% 
      summarise(min_ribbon = min(lo_velocity),
                max_ribbon = max(hi_velocity),
                min_median = min(med_velocity),
                max_median = max(med_velocity)) %>% 
      mutate(year = "whole_timeseries")

  # By region (high, mid, low latitudes)
    plot_df %>% 
      group_by(cat, ssp) %>% 
      summarise(min_ribbon = min(lo_velocity),
                max_ribbon = max(hi_velocity),
                min_median = min(med_velocity),
                max_median = max(med_velocity)) %>% 
      mutate(filter = "lat_region")
    
  # By region (high, mid, low latitudes) + only the final year
    plot_df %>% 
      filter(year == 2090) %>% 
      group_by(cat, ssp) %>% 
      summarise(min_ribbon = min(lo_velocity),
                max_ribbon = max(hi_velocity),
                # min_median = min(med_velocity),
                median = max(med_velocity)) %>% 
      mutate(filter = "lat_region") %>% 
      mutate(year = 2090) 
    
    
  # By region (high, mid, low latitudes) + only historic
    plot_df %>% 
      filter(ssp == "historical") %>% 
      group_by(cat, ssp) %>% 
      summarise(min_median = min(med_velocity),
                max_median = max(med_velocity)) %>% 
      mutate(filter = "lat_region")
    
  
  
# Plot timeseries --------------------------------------------------------------
  
  p <- ggplot() +
    # geom_ribbon(data = plot_df %>% filter(term == "recent"),
    #             aes(x = year, ymin = lo_velocity, ymax = hi_velocity),
    #             alpha = 0.1, colour = NA) +
    geom_line(data = plot_df %>% filter(term == "recent"),
              aes(x = year, y = med_velocity),
              colour = "grey40", lwd = 0.8) +
    geom_ribbon(data = plot_df %>% filter(term != "recent"),
                aes(x = year, ymin = lo_velocity, ymax = hi_velocity, fill = ssp),
                alpha = 0.1, colour = NA) +
    geom_line(data = plot_df %>% filter(term != "recent"),
              aes(x = year, 
                  y = med_velocity, 
                  colour = ssp),
              lwd = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", lwd = 0.5, col = "grey50", alpha = 0.6) +
    scale_colour_manual(values = ssp_cols, name = NULL) +
    scale_fill_manual(values = ssp_cols, name = NULL) +
    facet_wrap(~cat, ncol = 1) +
    labs(y = "Median climate velocity (km/decade)", x = "Year") +
    theme_classic(base_size = 10)
  p
  
  ggsave(p, file = paste0(plot_fol, "/velocity_decadal_timeseries-median_north-mid-south_ssp.pdf"),
         width = 8, height = 10, dpi = 300)
  ggsave(p, file = paste0(plot_fol, "/velocity_decadal_timeseries-median_north-mid-south_ssp.png"),
         width = 8, height = 10, dpi = 300)
  
  
  
  
  
# Plot an Oceania with region lines for the figure -----------------------------

  ## Make hlines at the latitude splits ------------
    lonmin <- ext(oceania_stanford_shp)[1]
    lonmax <- ext(oceania_stanford_shp)[2]
    target_lat1 <- -20
    target_lat2 <- -35

    line1 <- st_linestring(matrix(c(lonmin, target_lat1, lonmax, target_lat1), 
                                  ncol = 2, byrow = TRUE))
    line1_sf <- st_sfc(line1, crs = 4326)
    
    line2 <- st_linestring(matrix(c(lonmin, target_lat2, lonmax, target_lat2), 
                                  ncol = 2, byrow = TRUE))
    line2_sf <- st_sfc(line2, crs = 4326)
  
    
    
  ## Plot it  ------------
    a <- ggplot() +
      geom_sf(data = eez_shp,
              lwd = NA,
              fill = "grey85",
              alpha = 0.5) +
      geom_sf(data = oceania_stanford_shp,
              lwd = NA,
              fill = "grey80") +
      geom_sf(data = line1_sf, color = "grey30", linetype = "dashed", size = 0.5) +
      geom_sf(data = line2_sf, color = "grey30", linetype = "dashed", size = 0.5) +
      theme_void()
    a
    
    ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.pdf"),
           width = 8, height = 10, dpi = 300)
    ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.png"),
           width = 8, height = 10, dpi = 300)
  
  