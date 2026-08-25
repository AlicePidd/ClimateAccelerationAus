# Plotting climate acceleration spatially
  # Written by Alice P
    # 6 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 



# Ideas: 
  # Plotting acceleration spatially as normal, but only show 245 mid term, with rest in sups
  # Acceleration divergence from previous term? Like an anomaly
  # Latitudinal acceleration per SSP - median of all ESMs as the main line, ribbons as min-max across ESMs, lines per SSP, plot per term?
  # 



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  median_dfs_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "dfs")
  plot_fol <- make_folder(source_disk, "7_acceleration_aus_plot", "spatial") # Aus files
  
  
  
  
# Lists ------------------------------------------------------------------------

  term_order <- term_list
  ssp_order <- ssp_list
  
  
  
  
# Files ------------------------------------------------------------------------
  
  ## Load and bind all the median df files ------------
  
  future_files <- dir(median_dfs_fol, full.names = TRUE, pattern = ".RDS") %>% 
    str_subset(., "historical", negate = TRUE) # Gotta keep these out so that it doesn't skew the quantiles
  future_files # Use for computing lims etc.
    

    
    
# Palettes ---------------------------------------------------------------------
  
  pal_div_heat <- colorRampPalette(c("#001219", "#005F73", "#0a9396", "#C9E7DD",
                                     "#F7FBFF",
                                     "#FFEDCB", "#ee9b00", "#ca6702", "#79080C"))(100) # As a colour ramp
  
  
  pal_div_heat <- c("#001219", "#005F73", "#0a9396", "#94d2bd", "#F7FBFF", "#ee9b00", "#ca6702", "#ae2012", "#9b2226") # Manual version


  
  
  
# Calculate global quantiles by which to scale the plots -----------------------
    
  ## Projected data
    future_df <- map(future_files, ~{
      readRDS(.x)
      }) %>% 
      bind_rows()
    future_df
    
    future_range <- range(future_df$median_accel, na.rm = TRUE) # no historical: -203.9491  111.5277, with historical -401.0729  490.0310
    future_range
    
    ### IQR lims -------
      q25q75 <- quantile(future_df$median_accel, c(0.25, 0.75), na.rm = TRUE)
      q25q75
        # No Historical
          #          25%       75% 
          #   -1.4510885  0.7507432
        # With Historical
          #         25%       75% 
          #   -1.4625752  0.7817445 
      
      future_lims <- round(max(abs(q25q75)), digits = 1)  # ~1.46 so would go with ± 1.5 around 0
      future_lims
      
  
    ### 5th-9th percentile lims -------
      ##**This one**
      q5q95 <- quantile(future_df$median_accel, c(0.05, 0.95), na.rm = TRUE)
      q5q95
        # No Historical
          #          5%       95% 
          #   -5.488570  3.071301 
        # With historical
          #          5%       95% 
          #   -5.422862  3.360769 
    
      future_lims <- round(max(abs(q5q95)), digits = 1)  # 5.4, so will go with ± 5.5 around 0
      future_lims
      
  
  

# Plot spatially per SSP-term combination --------------------------------------

  plot_accel_future <- function(ssp, files, lims, lim_method, pal, pal_name) {
    
    files_ssp <- files %>% 
      str_subset(ssp) %>% 
      .[order(map_int(., ~ detect_index(term_order, \(t) grepl(t, .x))))]
    
    r <- map(files_ssp, readRDS) %>% 
      bind_rows() %>% 
      mutate(term = factor(term, levels = paste0(term_order, "-term")))
    
    p <- ggplot() +
      geom_raster(data = r, aes(x = lon, y = lat, fill = median_accel)) +
      scale_fill_gradientn(colours = pal,
                           limits = c(-lims, lims),
                           na.value = "transparent",
                           name = expression(paste("Median climate acceleration (km decade"^{-2}, ")")),
                           oob = squish,
                           guide = guide_colorbar(barwidth  = 12,
                                                  barheight = 0.5,
                                                  title.position = "top",
                                                  title.hjust    = 0.5,
                                                  direction = "horizontal")) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.3) +
      geom_sf(data = oceania_stanford_shp, fill = "grey80", colour = NA) +
      coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
      facet_wrap(~ term, ncol = 1) +
      labs(title = paste0("Median decadal acceleration -- ", ssp)) +
      theme_classic(base_size = 10) +
      theme(legend.position = "bottom",
            legend.title    = element_text(size = 8),
            legend.text     = element_text(size = 7),
            plot.subtitle   = element_text(size = 9, colour = "grey30"),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent")
            )
    
    o_nm <- paste0(plot_fol, "/spatial_median_acceleration_decadal_cropped_", ssp, 
                   "_pal-", pal_name, "_", lim_method, "_lims", lims)
    ggsave(filename = paste0(o_nm, ".png"), plot = p, width = 8, height = 20, dpi = 600, bg = "transparent")
    ggsave(filename = paste0(o_nm, ".pdf"), plot = p, width = 8, height = 20, dpi = 600)
    
  }
      
  ## Heat div palette -----------------
    walk(ssp_list, ~ plot_accel_future(.x, future_files, 
                                    lims = 5.5, lim_method = "Q5-Q95",
                                    pal = pal_div_heat, pal_name = "div_heat"))


      
# Calculate global quantiles by which to scale the plots -----------------------
      
  ## Historical data
    hist_files <- dir(median_dfs_fol, full.names = TRUE, pattern = "historical")
      
    hist_file <- readRDS(hist_files)
    hist_file # Use for computing lims etc.
  
    ### IQR lims -------
      q25q75_hist <- quantile(hist_file$median_accel, c(0.25, 0.75), na.rm = TRUE)
      q25q75_hist
      #         25%       75% 
      #   -1.701465  1.714993 
      
      hist_lims <- round(max(abs(q25q75_hist)), digits = 1)  # ~1.71 so will go with ± 2 around 0
      hist_lims
    
    
    ### 5th-9th percentile lims -------
      q5q95_hist <- quantile(hist_file$median_accel, c(0.05, 0.95), na.rm = TRUE)
      q5q95_hist
      #          5%       95% 
      #   -4.659361  9.744097 
      
      hist_lims <- round(max(abs(q5q95_hist)), digits = 1)  # 9.74, so will go with ± 10 around 0
      hist_lims
      
      
      
    
# Plot spatially for historical only -------------------------------------------
  
  
  plot_accel_hist <- function(file, lims, lim_method, pal, pal_name) {
    
    r <- readRDS(file) %>%
      mutate(term = factor(term, levels = paste0(term_order, "-term")))
    
    p <- ggplot() +
      geom_raster(data = r, aes(x = lon, y = lat, fill = median_accel)) +
      scale_fill_gradientn(colours = pal,
                           limits = c(-lims, lims),
                           na.value = "transparent",
                           name = expression(paste("Median climate acceleration (km decade"^{-2}, ")")),
                           oob = squish,
                           guide = guide_colorbar(barwidth  = 12,
                                                  barheight = 0.5,
                                                  title.position = "top",
                                                  title.hjust    = 0.5,
                                                  direction = "horizontal")) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.3) +
      geom_sf(data = oceania_stanford_shp, fill = "grey70", colour = NA) +
      coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
      labs(title = "Median decadal acceleration -- historical (OISST)") +
      theme_classic(base_size = 10) +
      theme(legend.position = "bottom",
            legend.title    = element_text(size = 8),
            legend.text     = element_text(size = 7),
            plot.subtitle   = element_text(size = 9, colour = "grey30"))
    
    o_nm <- paste0(plot_fol, "/spatial_median_acceleration_decadal_historical",
                   "_pal-", pal_name, "_", lim_method, "_lims", lims, ".png")
    ggsave(filename = o_nm, plot = p, width = 8, height = 6, dpi = 600)
  }
  
  # Do it
  plot_accel_hist(hist_files, 
                  lims = 10, lim_method = "Q5-Q95", 
                  pal = pal_div_heat, pal_name = "div_heat")
  plot_accel_hist(hist_files, 
                  lims = 1.7, lim_method = "IQR", 
                  pal = pal_div_heat, pal_name = "div_heat")
  plot_accel_hist(hist_files, 
                  lims = 25, lim_method = "arbitrary", 
                  pal = pal_div_heat, pal_name = "div_heat")
  