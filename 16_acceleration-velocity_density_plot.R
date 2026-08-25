# Plotting density plots of velocity and acceleration inside and outside
  # Written by Alice P
    # 8 April 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



  
# Folders ----------------------------------------------------------------------

  density_fol <- make_folder(source_disk, "9_density_aus_dfs", "")
  png_fol <- make_folder(source_disk, "10_density_aus_plot", "pngs") # Aus files
  pdf_fol <- make_folder(source_disk, "10_density_aus_plot", "pdfs") # Aus files
  
  

    
# Load data --------------------------------------------------------------------
  
  plot_df <- readRDS(paste0(density_fol, "median_values_velocity-accel_combined_df_allSSP_allterms.RDS"))
  plot_df
  hist_df <- readRDS(paste0(density_fol, "median_values_velocity-accel_combined_df_historical.RDS"))
  hist_df
  
  
# Find data range --------------------------------------------------------------
  
  range(plot_df$value) # -408.2199 3565.2945
  range(hist_df$value) # -401.0729 1937.1861
  
  
  
# Truncate data at 5-95th percentile -----------
  
  q_future <- plot_df %>%
    group_by(var) %>%
    summarise(q5 = round(quantile(value, 0.05)),
              q95 = round(quantile(value, 0.95)))
  q_future
    # # A tibble: 2 × 3
    #     var             q5   q95
    #     <chr>        <dbl> <dbl>
    #   1 acceleration    -5     3
    #   2 velocity         1   298
  
  q_hist <- hist_df %>%
    group_by(var) %>%
    summarise(q5 = round(quantile(value, 0.05)),
              q95 = round(quantile(value, 0.95)))
  q_hist
    # # A tibble: 2 × 3
    #     var             q5   q95
    #     <chr>        <dbl> <dbl>
    #   1 acceleration    -5    10
    #   2 velocity         9   216

    
# Truncate per var at q5 and q95
    plot_df2 <- plot_df %>%
      left_join(q_future, by = "var") %>%
      mutate(value = pmin(pmax(value, q5), q95)) %>%
      dplyr::select(-q5, -q95)
  
    hist_df2 <- hist_df %>%
      left_join(q_hist, by = "var") %>%
      mutate(value = pmin(pmax(value, q5), q95)) %>%
      dplyr::select(-q5, -q95)
  

    
  
# Plot -------------------------------------------------------------------------
  
  term_order <- c("recent", "near", "mid", "intermediate", "long")
  zone_cols   <- c("mpas" = "#0a9396", "nonmpas" = "#EE9B00")
  df <- plot_df2
  d_var <- "velocity"
  d_var <- "acceleration"
  
  
  make_plot <- function(df, d_var) {
    
    ssps <- unique(df$ssp)
    terms <- unique(df$term)
    
    hist_expanded <- hist_df2 %>%
      filter(var == d_var) %>%
      dplyr::select(-ssp, -term) %>% 
      cross_join(tibble(ssp = ssps, term_new = terms)) %>%
      rename(term = term_new) %>%
      mutate(zone = factor(zone, levels = c("nonmpas", "mpas")),
             term = factor(term, levels = term_order))
    
    med_df <- df %>%    #**Printed this for each d_var (velocity/accel) to populate int-text results**
      filter(var == d_var) %>%
      group_by(var, ssp, term, zone) %>%
      summarise(med = median(value, na.rm = TRUE), .groups = "drop")
    
    med_h_df <- hist_expanded %>%
      group_by(var, ssp, term, zone) %>%
      summarise(med = median(value, na.rm = TRUE), .groups = "drop")
    
    df %>%
      filter(var == d_var) %>%
      mutate(var = factor(var),
             zone = factor(zone, levels = c("nonmpas", "mpas"))) %>%
      ggplot(aes(x = value, fill = zone, colour = zone)) +
      geom_density(data = hist_expanded,
                   aes(x = value, colour = zone),
                   alpha = 0.1,
                   linetype = "dashed", linewidth = 0.2) +
      geom_density(alpha = 0.35, linewidth = 0.5) +
      geom_vline(data = med_h_df,
                 aes(xintercept = 0), # Added a line at 0 on x-axis
                 colour = "black", linetype = "solid", linewidth = 0.4) +
      geom_vline(data = med_h_df,
                 aes(xintercept = med, colour = zone),
                 linetype = "dashed", linewidth = 0.2) +
      geom_vline(data = med_df,
                 aes(xintercept = med, colour = zone),
                 linetype = "solid", linewidth = 0.4) +
      facet_grid(ssp ~ var, scales = "free_x") +
      scale_fill_manual(values = zone_cols) +
      scale_colour_manual(values = zone_cols) +
      labs(x = NULL, y = "Density", fill = NULL, colour = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.2),
            legend.position = "top",
            strip.background = element_rect(fill = "grey90"))
  }
  
  
  # Main text (mid-term only)
    p_velocity <- plot_df2 %>% 
      filter(term == "mid") %>% 
      make_plot(., "velocity")
    p_velocity
    
    p_accel <- plot_df2 %>% 
      filter(term == "mid") %>% 
      make_plot(., "acceleration")
    p_accel
    
    ggsave(paste0(png_fol, "/density_velocity_mid_0line.png"), p_velocity, width = 7,  height = 6)
    ggsave(paste0(pdf_fol, "/density_velocity_mid_0line.pdf"), p_velocity, width = 7,  height = 6)
    ggsave(paste0(png_fol, "/density_accel_mid_0line.png"), p_accel, width = 7,  height = 6)
    ggsave(paste0(pdf_fol, "/density_accel_mid_0line.pdf"), p_accel, width = 7,  height = 6)
    
  
  # Supplement (all terms, facet by term too)
    p_velocity_ssp <- plot_df2 %>%
      mutate(term = factor(term, levels = term_order)) %>%
      group_by(ssp) %>%
      group_map(~ make_plot(.x, "velocity") + 
                  facet_wrap(~ term, nrow = 1) +
                  ggtitle(.y$ssp),
                .keep = TRUE)
    p_velocity_ssp <- wrap_plots(p_velocity_ssp, ncol = 1)
    p_velocity_ssp

    p_accel_supp <- plot_df2 %>%
      mutate(term = factor(term, levels = term_order)) %>%
      group_by(ssp) %>%
      group_map(~ make_plot(.x, "acceleration") + 
                  facet_wrap(~ term, nrow = 1) +
                  ggtitle(.y$ssp),
                .keep = TRUE)
    p_accel_supp <- wrap_plots(p_accel_supp, ncol = 1)
    p_accel_supp
    
    ggsave(paste0(png_fol, "/density_velocity_all_terms_0line.png"), p_velocity_ssp, width = 10,  height = 12)
    ggsave(paste0(pdf_fol, "/density_velocity_all_terms_0line.pdf"), p_velocity_ssp, width = 10,  height = 12)
    ggsave(paste0(png_fol, "/density_accel_all_terms_0line.png"), p_accel_supp, width = 10,  height = 12)
    ggsave(paste0(pdf_fol, "/density_accel_all_terms_0line.pdf"), p_accel_supp, width = 10,  height = 12)
    

    