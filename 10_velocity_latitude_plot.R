# Plotting climate velocity by 1° latitude
  # Written by Alice P
    # 7 March 2026

# Climate velocity at each 1° of latitude - median of each band, per SSP and term



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_term_fol <- make_folder(source_disk, "2_velocity_rolling_annual_termsplit", "")
  plot_fol <- make_folder(source_disk, "6_velocity_aus_plot", "latitude")


  
# Get median acceleration (slope) per 1° latitude ------------------------------

  files <- dir(vocc_term_fol, full.names = TRUE) %>% 
    str_subset(., "534-over", negate = TRUE) %>% 
    str_subset(., "ssp119", negate = TRUE)

  slope_lat <- function(f) {
    
    r <- readRDS(f)
    term <- basename(f) %>% str_split_i(., "_", 5)
    
    # Get lat for each cell
    lat_df <- crds(r, df = TRUE, na.rm = FALSE) %>% # Keep NAs so same number of cols
      as_tibble() %>% # don't want a df
      mutate(lat_band = ceiling(y)) # Round up to 1° band (cus we're in negatives)
    
    # Get values for all layers, pivot and summarise
    vals <- values(r, dataframe = TRUE) %>% 
      as_tibble() %>% 
      bind_cols(lat_df) %>% 
      pivot_longer(cols = -c(x, y, lat_band), 
                   names_to = "layer", 
                   values_to = "slope") %>% 
      group_by(lat_band, layer) %>% 
      summarise(median_vocc = median(slope, na.rm = TRUE), # Get median slope across all ESMs at each lat
                .groups = "drop") %>% 
      mutate(ssp  = str_split_i(layer, "_", 2),
             esm  = str_split_i(layer, "_", 3),
             term = term) %>% 
      dplyr::select(lat = lat_band, median_vocc, ssp, term, esm)
    
    return(vals)
  }
  
  out <- map(files, slope_lat) %>% 
    bind_rows %>% 
    mutate(term = if_else(term == "historical", "recent", term)) %>% 
    { bind_rows( # ChatGPT helped to duplicate the recent term for each SSP
      filter(., ssp != "OISST"),
      expand_grid(ssp = c("ssp126", "ssp245", "ssp370", "ssp585"),
                  filter(., ssp == "OISST") %>% 
                    dplyr::select(-ssp))
      )}
  out
  
  
  
# Get it in a tibble format for plotting ---------------------------------------
  
  out_summary <- out %>% 
    group_by(lat, ssp, term) %>% 
    summarise(median_vocc_yr = median(median_vocc, na.rm = TRUE),
              median_vocc_dec = median(median_vocc, na.rm = TRUE) * 10,
              .groups = "drop") %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  recent_rows <- out_summary %>% 
    filter(ssp == "historical") %>% 
    dplyr::select(-ssp) # Otherwise it plots it as historical, not with the ssps

  out_summary <- out_summary %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  ssp_summary <- out_summary %>% 
    filter(ssp != "historical")

  # Checking the spread for the lims
  range(ssp_summary$median_vocc_yr) # -2.205539 42.721813
  range(ssp_summary$median_vocc_dec) # -22.05539 427.21813
  
  
  

# Plot -------------------------------------------------------------------------
  
  pal <- c("#FFFDF4", "#f4c659", "#d9792e", "#af2213", "#871c0f")

  plot_lat_velocity <- function(m, lim, lab) {
    lat_plot <- ggplot() +
      geom_tile(data = recent_rows, aes(x = term, y = lat, fill = .data[[m]])) + 
      geom_tile(data = ssp_summary, aes(x = term, y = lat, fill = .data[[m]])) + 
      scale_fill_gradientn(colours = pal, 
                           limits = lim) +
      facet_wrap(~ssp, nrow = 1) +
      scale_y_continuous(breaks = seq(-50, 0, by = 10)) +
      labs(fill = paste0("Climate velocity\n(km/", lab, ")"), y = "Latitude", x = NULL) + 
      theme_few(base_size = 10)
    
    ggsave(paste0(plot_fol, "/median_climate velocity_by_1deglatitude_km", lab, ".pdf"),
           lat_plot, width = 14, height = 8, dpi = 300)
  }
  
  params <- list(list(m = "median_vocc_dec", lim = c(-50, 450), lab = "decade"),
                 list(m = "median_vocc_yr",  lim = c(-5, 45),   lab = "year"))

  walk(params, function(p) {
    plot_lat_velocity(m = p$m, lim = p$lim, lab = p$lab)
  })
