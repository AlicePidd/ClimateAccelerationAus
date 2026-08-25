# Computing median DECADAL climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  vocc_term_dec_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") # Yearly climate velocity (km/year)
  esm_fol <- make_folder(source_disk, "3_velocity_decadal_ESMs_terms", "dfs/cropped")
  median_df_crop_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "dfs/cropped") 
  median_rast_crop_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts/cropped") 
  

  
    
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  ## Projected data files ------------
  
    files <- dir(vocc_term_dec_fol, full.names = TRUE, pattern = ".RDS") %>% 
      str_subset("historical", negate = TRUE)
    files
    ssp <- "ssp245"
    term <- "mid"
    rate <- "decadal"
    
    r <- readRDS(files[1])
  
    
  ## Do it ------------
  
    get_velocity_dfs <- function(ssp, term, files, rate) {
      message("Processing: ", ssp, ", ", term)
      
      # Get all ESM files for this SSP-term combo, read, mask and set CRS
      combo_files <- files %>% 
        str_subset(ssp) %>% 
        str_subset(term)
      rast_list <- map(combo_files, readRDS) %>%
        map(function(r) { 
          crs(r) <- "EPSG:4326"; 
          mask(r, eez_shp) 
          })
      
      # Make df of velocity per ESM
      df1 <- map_dfr(seq_along(combo_files), function(l) {
        esm_name <- basename(combo_files[[l]]) %>% str_split_i("_", 4)
        rast_list[[l]] %>%
          as.data.frame(xy = TRUE, na.rm = TRUE) %>%
          as_tibble() %>%
          rename(lon = 1, lat = 2) %>%
          pivot_longer(cols = -c(lon, lat), names_to = "col_name", values_to = "velocity") %>%
          mutate(ssp = str_split_i(col_name, "_", 2),
                 esm = esm_name,
                 term = paste0(term, "-term"),
                 rate = rate) %>%
          dplyr::select(-col_name)
      })
      saveRDS(df1, paste0(esm_fol, "/velocity_", rate, "_ESMs_cropped_df_", ssp, "_", term, ".RDS"))
      
      # Median df across ESMs
      df2 <- df1 %>%
        group_by(lon, lat, ssp, term) %>%
        summarise(median_velocity = median(velocity, na.rm = TRUE),
                  q25 = quantile(velocity, 0.25, na.rm = TRUE),
                  q75 = quantile(velocity, 0.75, na.rm = TRUE),
                  .groups = "drop")
      saveRDS(df2, paste0(median_df_crop_fol, "/velocity_", rate, "_median_cropped_df_", ssp, "_", term, ".RDS"))
      
      # Median raster stack across ESMs
      r_stack <- rast(rast_list)
      out_stack <- c(app(r_stack, median, na.rm = TRUE),
                     app(r_stack, function(x) quantile(x, 0.25, na.rm = TRUE)),
                     app(r_stack, function(x) quantile(x, 0.75, na.rm = TRUE)))
      names(out_stack) <- c(paste0("median_", ssp, "_", term),
                            paste0("q25_", ssp, "_", term),
                            paste0("q75_", ssp, "_", term))
      saveRDS(out_stack, paste0(median_rast_crop_fol, "/velocity_", rate, "_median_cropped_rast_", ssp, "_", term, ".RDS"))
    }
    
      combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])
      combos
      tic()
      walk2(combos$ssp, combos$term, get_velocity_dfs, 
            files = files, rate = "decadal")
      toc()
      beep(2)

    
    
    
  ## Do it for the OISST file -----------
    
    hist_files <- dir(vocc_term_dec_fol, full.names = TRUE, pattern = ".RDS") %>% 
      str_subset("historical")
    hist_files # Should just be the one
    
    # No median across ESMs here, but will need median for the term
      r <- readRDS(hist_files)
      r
    
    # Mask to EEZ
      r <- mask(r, eez_shp)
      crs(r) <- "EPSG:4326"

    # Get median for the recent-term (rast version)  
      hist_rast <- c(app(r, median, na.rm = TRUE),
                     app(r, function(x) quantile(x, 0.25, na.rm = TRUE)),
                     app(r, function(x) quantile(x, 0.75, na.rm = TRUE)))
      names(hist_rast) <- c("median_historical_recent", "q25_historical_recent", "q75_historical_recent")
      saveRDS(hist_rast, paste0(median_rast_crop_fol, "/velocity_decadal_median_cropped_rast_historical_recent.RDS"))

    # df version 
      ## (same structure as df2 from the projected data)
      hist_df <- hist_rast[[1]] %>%
        as.data.frame(xy = TRUE, na.rm = TRUE) %>%
        as_tibble() %>%
        rename(lon = 1, lat = 2, median_velocity = 3) %>%
        mutate(ssp = "historical", 
               term = "recent-term", 
               rate = "decadal")
        
      saveRDS(hist_df, paste0(median_df_crop_fol, "/velocity_decadal_median_cropped_df_historical_recent.RDS"))
      
  