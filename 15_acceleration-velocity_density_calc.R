# Getting data for density plots of velocity and acceleration inside and outside MPAs
  # Written by Alice P
    # 8 April 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts/cropped") 
  accel_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  density_fol <- make_folder(source_disk, "9_density_aus_dfs", "")

  
  
  
# Load rasters -----------------------------------------------------------------
  
  ## For projected data ------------
    vel_files <- dir(vocc_fol, full.names = TRUE, pattern = ".RDS") %>%
      str_subset("historical", negate = TRUE)
   
    accel_files <- dir(accel_fol,  full.names = TRUE, pattern = ".RDS") %>%
      str_subset("historical", negate = TRUE)
    
      load_stack <- function(files) {
        f <- files
        readRDS(f)[[1]]
      }
  
    vel_stack <- map(vel_files, load_stack)
    accel_stack <- map(accel_files, load_stack)
    accel_stack <- map(accel_stack, ~ {
      names(.x) <- str_remove(names(.x), "-term")
      .x
    })
    
    
    
  ## Same for historical ------------
    vel_hist <- dir(vocc_fol, full.names = TRUE, pattern = "historical") 
    accel_hist <- dir(accel_fol, full.names = TRUE, pattern = "historical") 
    
    vel_hist_rast <- readRDS(vel_hist)
    accel_hist_rast <- readRDS(accel_hist)
    names(accel_hist_rast) <- str_remove(names(accel_hist_rast), "-term")
    
  
  
# Make masks for eez, mpas and and non-mpas ----------------------------------------------
  
  # Make a template raster from teh stacks
    template <- vel_stack[[1]][[1]] # one lyr
  
  # Make vectors
    eez_vect <- vect(eez_shp)
    mpa_vect <- vect(mpa_shp)
    aus_vect <- vect(aus_detailed_shp)
    
  # Rasterize shapefiles to match template
    aus_mask <- rasterize(aus_vect, template, field = 1, background = NA)
    eez_mask <- rasterize(eez_vect, template, field = 1, background = NA)
    eez_ocean_mask <- mask(eez_mask, aus_mask, inverse = TRUE)
    
    mpa_mask <- rasterize(mpa_vect, template, field = 1, background = NA)
    non_mpa_mask <- mask(eez_ocean_mask, mpa_mask, inverse = TRUE)
  
    
    
# Mask rasts to MPAs and non-MPAS ----------------------------------------------
    
  extract_vals <- function(stack, var_name, r_mask, label) {
    nm <- names(stack[[1]]) # Get the name of the median layer
    ssp <- str_split_i(nm, "_", 2) 
    term <- str_split_i(nm, "_", 3)
    
    vals <- as.vector(values(mask(stack[[1]], r_mask), na.rm = TRUE)) # Get the values of the masked input spatraster
    tibble(value = vals, zone = label, # Make tibble with labelled columns
           ssp = ssp,
           term = term,
           var = var_name) # Name it with the var
  }
  
  ## Projected ------------
    
    plot_df <- bind_rows(
      map_dfr(vel_stack, extract_vals, var_name = "velocity", r_mask = mpa_mask, label = "mpas"),
      map_dfr(vel_stack, extract_vals, var_name = "velocity", r_mask = non_mpa_mask, label = "nonmpas"),
      map_dfr(accel_stack, extract_vals, var_name = "acceleration", r_mask = mpa_mask, label = "mpas"),
      map_dfr(accel_stack, extract_vals, var_name = "acceleration", r_mask = non_mpa_mask, label = "nonmpas")
    )
    plot_df
    saveRDS(plot_df, paste0(density_fol, "median_values_velocity-accel_combined_df_allSSP_allterms.RDS"))
  
    
  ## Historical ------------
    
    hist_df <- bind_rows(
      extract_vals(vel_hist_rast, var_name = "velocity", r_mask = mpa_mask, label = "mpas"),
      extract_vals(vel_hist_rast, var_name = "velocity", r_mask = non_mpa_mask, label = "nonmpas"),
      extract_vals(accel_hist_rast, var_name = "acceleration", r_mask = mpa_mask, label = "mpas"),
      extract_vals(accel_hist_rast, var_name = "acceleration", r_mask = non_mpa_mask, label = "nonmpas")
    )
    hist_df
    saveRDS(hist_df, paste0(density_fol, "median_values_velocity-accel_combined_df_historical.RDS"))
  
    