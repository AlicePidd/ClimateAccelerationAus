# Computing median decadal climate velocity across ALL 11 ESMs, NOT an ensemble, just the median value
  # Written by Alice P
    # 7 March 2026


# This script:
  # Makes dfs of decadal acceleration for each SSP-term combo, and each of the 11 ESMs, saves it per combo
  # Uses this to compute the median decadal acceleration and Q25/Q75 for each SSP-term combo, saves it as a df per combo
  # Computes the same things as the second df, but keeps it as a spatraster just in case.



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  accel_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "decadal")
  esm_fol <- make_folder(source_disk, "4_acceleration_aus_ESMs_terms", "dfs")
  median_df_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "dfs")
  median_rast_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  
  
  
  
# Get the median climate acceleration across all ESMs together, per SSP-term combo -------------

  ## Do it for projected data -----------
  
    files <- dir(accel_fol, full.names = TRUE, pattern = ".RDS") %>% 
      str_subset("historical", negate = TRUE)

    get_accel_dfs <- function(f) {
      ssp <- basename(f) %>% 
        str_split_i(., "_", 3)
      term <- basename(f) %>% 
        str_split_i(., "_", 4)
      
      message("Processing: ", ssp, ", ", term)
      
      r <- readRDS(f)
  
      # Make df of acceleration per ESM, for each SSP-term combo, and keep the lat/lon
      df1 <- r %>% 
        as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
        as_tibble() %>% 
        rename(lon = 1, lat = 2) %>% 
        pivot_longer(cols = -c(lon, lat),
                     names_to  = "col_name",
                     values_to = "accel") %>% 
        mutate(ssp  = str_split_i(col_name, "_", 3),
               esm  = str_split_i(col_name, "_", 4),
               term = str_split_i(col_name, "_", 5) %>% paste0(., "-term"),
               rate = "decadal") %>% 
        dplyr::select(-col_name)
      saveRDS(df1, paste0(esm_fol, "/acceleration_decadal_ESMs_df_", ssp, "_", term, ".RDS"))
      
      # Get median acceleration and Q25/Q75 for each SSP-term combo
        # Each row is one 0.25° pixel with its median, Q25, and Q75 across the 11 ESMs
      df2 <- df1 %>% 
        group_by(lon, lat, ssp, term) %>% 
        summarise(median_accel = median(accel, na.rm = TRUE),  
                  q25 = quantile(accel, 0.25, na.rm = TRUE), 
                  q75 = quantile(accel, 0.75, na.rm = TRUE), 
                  .groups = "drop") %>% 
        mutate(ssp = ssp, term = term)
      saveRDS(df2, paste0(median_df_fol, "/acceleration_decadal_median_df_", ssp, "_", term, ".RDS"))
  
      # Get median acceleration and the Q25/Q75 but keep it as a rast
      r_median <- app(r, median, na.rm = TRUE)
      r_q25 <- app(r, \(x) quantile(x, 0.25, na.rm = TRUE)) # \(x) is a cool alternative to function(x)!!! Doesn't work in old R though
      r_q75 <- app(r, \(x) quantile(x, 0.75, na.rm = TRUE))
  
      stack <- c(r_median, r_q25, r_q75)
      names(stack) <- c(paste0("median_", ssp, "_", term),
                             paste0("q25_", ssp, "_", term),
                             paste0("q75_", ssp, "_", term))
      saveRDS(stack, paste0(median_rast_fol, "/acceleration_decadal_median_rast_", ssp, "_", term, ".RDS"))
  
    }
    
    tic()
    walk(files, get_accel_dfs)
    toc() # Takes 2 mins on Alice's machine
    beep(2)
  
  
  
  ## Do it for the OISST file -----------
    
    files <- dir(accel_fol, full.names = TRUE, pattern = "historical")
    files
    
    r <- readRDS(files)
    
    # df version
      df_hist <- r %>%
        as.data.frame(xy = TRUE, na.rm = TRUE) %>%
        as_tibble() %>%
        rename(lon = 1, lat = 2, median_accel = 3) %>%
        mutate(ssp  = "historical",
               term = "recent-term",
               q25  = NA_real_,
               q75  = NA_real_)
      saveRDS(df_hist, paste0(median_df_fol, "/acceleration_decadal_median_df_historical_recent-term.RDS"))
    
    # rast version
      names(r) <- "median_historical_recent-term"
      saveRDS(r, paste0(median_rast_fol, "/acceleration_decadal_median_rast_historical_recent-term.RDS"))
  
