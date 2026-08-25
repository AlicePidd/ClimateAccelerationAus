# Plotting climate acceleration by latitude - spatial spread of median acceleration per 1°, with 1° min and max\
  # Written by Alice P
    # 5 Apr 2026

  # Median acceleration at each 1° of latitude for each ESM - plotting the median across all ESM medians for each 1°, and a ribbon with the min and max value at each 1° lat.

  # This plot the SPATIAL SPREAD of acceleration by latitude


  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  acc_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "decadal")
  plot_fol <- make_folder(source_disk, "7_acceleration_aus_plot", "latitude") # Aus files
  
  
  
  
# Read all files, extract values + lat --------------------------
  
  files <- dir(acc_fol, full.names = TRUE, pattern = ".RDS")

  raw_dat <- map(files, function(f) {
    r <- readRDS(f)
    
    vals <- values(r, dataframe = TRUE) %>%
      as_tibble() %>%
      bind_cols(crds(r, df = TRUE, na.rm = FALSE) %>% 
                  as_tibble()) %>%
      pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "accel") %>%
      mutate(ssp  = str_split_i(layer, "_", 3),
             esm  = str_split_i(layer, "_", 4),
             term = str_split_i(layer, "_", 5)) %>%
      dplyr::select(lat = y, accel, ssp, esm, term)
    
    med_vals <- vals %>%
      filter(!is.na(accel)) %>% 
      mutate(lat_cat = ceiling(lat)) %>% 
      group_by(ssp, term, lat_cat) %>% 
      mutate(med_lat_accel = median(accel, na.rm = TRUE),
             lo_5_lat_accel = quantile(accel, 0.05, na.rm = TRUE),
             hi_95_lat_accel = quantile(accel, 0.95, na.rm = TRUE),
             lo_25_lat_accel = quantile(accel, 0.25, na.rm = TRUE),
             hi_75_lat_accel = quantile(accel, 0.75, na.rm = TRUE),
             min_lat_accel = min(accel, na.rm = TRUE),
             max_lat_accel = max(accel, na.rm = TRUE))
    
    return(med_vals)
  }) %>%
    bind_rows()

  raw_dat

  # Factor terms in both datasets
    term_levels <- term_list
    term_labels  <- c("Recent-term", "Near-term", "Mid-term", "Intermediate-term", "Long-term")

  raw_dat <- raw_dat %>%
    mutate(term = factor(term, levels = term_levels, labels = term_labels))
  raw_dat
  

  
  
# Tidy up for plotting data ----------------------------------------------------
  
  select_med <- raw_dat %>%
    group_by(ssp, term, lat_cat) %>% 
    slice(1) %>% 
    dplyr::select(-esm)

  select_med
  range(select_med$med_lat_accel) # [1] -9.206717  7.964705


  
  
# Plot -------------------------------------------------------------------------
  
  ssp_cols <- c(#"historical" = "#5F5E5A",
                "ssp126" = col_ssp126,
                "ssp245" = col_ssp245,
                "ssp370" = col_ssp370,
                "ssp585" = col_ssp585)
  
  future_med <- select_med %>% filter(ssp != "historical")
  future_med
  hist_line <- select_med %>% 
    filter(ssp == "historical")
  hist_line
  
  hist_line_all <- hist_line %>% # Make it so the histline appears on all the terms, not just recent
    ungroup() %>% 
    dplyr::select(-term)
  hist_line_all
  

  p <- ggplot() +
      geom_ribbon(data = future_med,
                  aes(x = lat_cat, 
                      ymin = lo_25_lat_accel, ymax = hi_75_lat_accel, 
                      fill = ssp), # IQR
                  alpha = 0.2, colour = NA) +
      geom_line(data = future_med,
                aes(x = lat_cat, y = med_lat_accel, 
                  colour = ssp),
                linewidth = 0.5) +
      geom_ribbon(data = hist_line_all,
                  aes(x = lat_cat,
                      ymin = lo_25_lat_accel, ymax = hi_75_lat_accel),
                  fill = "grey20",
                  alpha = 0.2, colour = NA) +
      geom_line(data = hist_line_all, # Make it so the histline is on all terms
                aes(x = lat_cat, y = med_lat_accel),
                colour = "grey20", linewidth = 0.5, linetype = "dashed") +
      geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3, linetype = "dotted") +
      scale_x_continuous(breaks = seq(-50, -5, by = 10),
                         labels = function(x) paste0(abs(x), "°S")) +
      coord_flip() +
      facet_wrap(~ term, nrow = 1) +
      scale_colour_manual(values = ssp_cols, name = NULL) +
      scale_fill_manual(values = ssp_cols, name = NULL) +
      labs(title = "Spatial spread of acceleration by latitude",
           x = "Latitude",
           y = expression("Climate acceleration (km decade"^{-2}*")")) +
      theme_linedraw(base_size = 10) +
      theme(strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(4, "pt"))
  
  p
  o_nm <- paste0(plot_fol, "/median_IQR_acceleration_across_latitude_ssp-term.pdf") 
  ggsave(filename = o_nm, plot = p, height = 6, width = 6, dpi = 300)
  o_nm <- paste0(plot_fol, "/median_IQR_acceleration_across_latitude_ssp-term.png") 
  ggsave(filename = o_nm, plot = p, height = 6, width = 6, dpi = 300)
  
