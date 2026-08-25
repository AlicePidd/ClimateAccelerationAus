# Get the median velocity across all ESMs per year
  # Written by Alice P
    # 2nd April 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") 
  vocc_med_timeseries_fol <- make_folder(source_disk, "3_velocity_decadal_median_timeseries", "rasts") 
  df_fol <- make_folder(source_disk, "3_velocity_decadal_median_timeseries", "dfs") # Use the rasts so can crop them

  
  
  
# Get median velocity per year and SSP, across all ESMs ------------------------
  
  esm_files <- dir(vocc_fol, full.names = TRUE) %>% 
    str_subset(., pattern = "historical", negate = TRUE)
  esm_files
  hist_files <- dir(vocc_fol, full.names = TRUE, pattern = "historical")
  hist_files
  
  files <- esm_files
  ssp <- "ssp245"
  term <- "intermediate"
  
  get_median_timeseries <- function(files, ssp, term) {

    rast_list <- files %>%
      str_subset(ssp) %>%
      str_subset(term) %>%
      map(readRDS) %>%
      map(function(r) { # For every layer 
        crs(r) <- "EPSG:4326"; # Change crs
        mask(r, eez_shp) # And mask it to EEZ
        })
    
    # Get pixel-wise median across ESMs for each year layer
      n_layers   <- nlyr(rast_list[[1]]) # Get first layer just to count number of layers
      median_rast <- map(1:n_layers, function(l) rast_list %>% # For every layer in each rast
                           map(function(r) r[[l]]) %>% # Subset it
                           rast() %>% # Stack them
                           app(median, na.rm = TRUE)) %>% # Compute median
        rast()
    
    # Name layers and save
      yr1 <- names(rast_list[[1]])[1] %>% 
        str_split_i("_", 1) %>% 
        as.numeric()
      yr2 <- yr1 + n_layers - 1
      names(median_rast) <- paste0(yr1:yr2, "_med_dec_velocity_", ssp)
      
      saveRDS(median_rast, paste0(vocc_med_timeseries_fol, "/velocity_decadal_median_cropped_rast_", ssp, "_", term, "_", yr1, "-", yr2, ".RDS"))
  
  }  
  
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])
  pwalk(combos, function(ssp, term) get_median_timeseries(esm_files, ssp, term))
  beep(2)
  
  
  
  
# Same to the historical file to the same folder (no median calc needed) -------
  
  hist_file <- readRDS(hist_files)
  hist_file <- mask(hist_file, eez_shp) # mask it to EEZ
  crs(hist_file) <- "EPSG:4326"
  
  saveRDS(hist_file, paste0(vocc_med_timeseries_fol, "/velocity_decadal_median_cropped_rast_historical_recent_1995-2014.RDS")) 


  

# Get rasters in df format -----------------------------------------------------
  
  all_files <- dir(vocc_fol, full.names = TRUE)

  do_mask_df <- function(f){
    
    ssp <- basename(f) %>%
      str_split_i(., "_", 3)
    esm <- basename(f) %>%
      str_split_i(., "_", 4)
    term <- basename(f) %>%
      str_split_i(., "_", 5)
    
    d <- readRDS(f) # Read it in
    m <- mask(d, eez_shp) # Mask it
    crs(m) <- "EPSG:4326"
    
    cat_df <- m %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      pivot_longer(cols = -c(x, y), # Multilayer files so need to pivot 
                   names_to = "year",
                   values_to = "velocity") %>%
      mutate(year = str_split_i(year, "_", 1) %>% as.numeric(),
             ssp  = ssp,
             term = term,
             esm  = esm,
             cat  = case_when(y <= q[[1]] ~ "south",
                              y > q[[1]] & y <= q[[2]] ~ "mid",
                              y > q[[2]] ~ "north"))
    
    return(cat_df)
  }
  
  df_comb <- map(all_files, do_mask_df) %>%
    bind_rows()
  df_comb
  saveRDS(df_comb, file = paste0(df_fol, "/velocity_decadal_timeseries-median_cropped_df_combined_ssp-esm-term.RDS"))

