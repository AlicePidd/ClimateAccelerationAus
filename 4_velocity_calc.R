# Computing annual AND decadal climate velocity (km.decade) from annual SST layers, using a 21-year rolling window
  # Written by Dave S and Alice P
    # 5 March 2026



# This script:
  # Makes rolling 21-year windows for all the years in each term, where each term year is the center year +- 10 years each side
  # Sources the VoCC functions needed (spatGrad, tempTrend, gVoCC)
  # Uses yearly SST data from 11 ESMs
  # Subsets the yearly tos data for the corresponding 21-year windows for each term (e.g., for near-term 2021-2040, SST data from 2031-2050)
    # Long-term is only 2081-2090 as we only had projected SST data up to 2100. Velocity computation up to 2100 would require projected SST data to 2110.
  # Calculates the spatGrad and tempTrend for these years, and uses these to calculate gVoCC, which corresponds to the middle year (11th of 21) of each 21-year rolling window
  # Each layer is named with that 11th year, to yield 1 file of annual climate velocity (km/year) for each ESM under each SSP, for the full 2021-2090
  # These files are also saved as the term splits
  # The same is done for the historical period (1995-2014)




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  sst_fol <- make_folder(source_disk, "1_CMIP_regridded_OISST_DBC_sst_annual_Aus", "") # From Dave
  oisst_fol <- make_folder(source_disk, "1_OISST_annual_Aus", "") # From Dave
  fn_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  # vocc_fol <- make_folder(source_disk, "2_velocity_rolling", "")
  vocc_yr_fol <- make_folder(source_disk, "2_velocity_rolling", "annual")
  vocc_dec_fol <- make_folder(source_disk, "2_velocity_rolling", "decadal")
  vocc_term_yr_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "annual")
  vocc_term_dec_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal")
  
  
  

# A sliding window and get the years for one period ----------------------------
  
  make_rolling_seq <- function(yr) {
    seq(yr-10, yr+10, by = 1)
  }
  
  all_range <- 2021:2090 # Projection timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  intermediate_range <- 2061:2080
  long_range <- 2081:2090
  
  all_years <- map(all_range, make_rolling_seq)
  recent_years <- map(recent_range, make_rolling_seq)
  near_years <- map(near_range, make_rolling_seq)
  mid_years <- map(mid_range, make_rolling_seq)
  intermediate_years <- map(intermediate_range, make_rolling_seq)
  long_years <- map(long_range, make_rolling_seq)
  

  
  
# Source the velocity functions ------------------------------------------------
  
  dir(fn_fol, full.names = TRUE) %>% 
    map(source)

  

# Compute velocity -------------------------------------------------------------
    # Compute grid cell velocity for each period, by calculating the rolling velocity for the surrounding 10 years of each year that is within each period (i.e., to calculate the velocity for the first year in the near-term, 2021, we calculate the velocities each of the 10 years prior and after, so between 2011-2031)
    # Use these rolling velocities for each year in each period to compute the acceleration of each period as the slope of those velocities

  files <- dir(sst_fol, full.names = TRUE) %>% 
    str_subset(., "ssp119|ssp534-over|CESM2-WACCM|GFDL-ESM4", negate = TRUE) # None of these files
  oisst_files <- dir(oisst_fol, full.names = TRUE)
  
  get_velocity <- function(f, yrs, term) {
    
    esm <- basename(f) %>% 
      str_split_i(., "_", 3)
    ssp <- basename(f) %>% 
      str_split_i(., "_", 4)
    term <- term
    
    if(term == "all") {
      range <- all_range # Full time series
      yrs <- all_years
    } else {
      range <- get(paste0(term, "_range")) # Series of years in the term
      yrs   <- get(paste0(term, "_years")) # Years +10 and -10 either side of the term range
    }

    indexing <- seq_along(yrs)
    
    spt_raster <- rast(f) %>% 
      crop(., e1) # Crop
    spt_raster <- mask(spt_raster, mask_land) # Mask out land
    names(spt_raster) <- time(spt_raster) %>% 
      year()
    
      compute <- function(i) {
        r <- terra::subset(spt_raster, yrs[[i]] %>% unlist() %>% as.character()) # Get only the data for the years we need
        spt_grad <- spatGrad(r) # How quickly the SST changes across space (°C/km) i.e., how steep is the temp landscape at each grid cell
        tmp_trend <- tempTrend(r, 3) # How quickly SST is changing through time (°C/year) i.e., how fast is temp changing over time at each grid cell
        out <- gVoCC(tmp_trend, spt_grad) %>% # velocity = (°C/year) / (°C/km) = km/year
          terra::subset(1) # Only need VoCC not the other layers
        nm_yr <- yrs[[i]][11] # The year we're calculating for
        
        if(term == "recent") {
          names(out) <- paste0(nm_yr, "_historical_", esm)
        } else {
          names(out) <- paste0(nm_yr, "_", ssp, "_", esm)
        }
        return(out)
      }
    
    result <- map(indexing, compute) %>% rast()  # km/year
    result_decadal <- result * 10 # km/decade

    make_path <- function(rate) {
      fol <- if(rate == "annual") {
        if(term == "all") vocc_yr_fol else vocc_term_yr_fol
      } else {
        if(term == "all") vocc_dec_fol else vocc_term_dec_fol
      }
      
      if(term == "all") {
        paste0(fol, "/vocc_", rate, "_", ssp, "_", esm, "_", min(range), "-", max(range), ".RDS")
      } else if(term == "recent") {
        paste0(fol, "/vocc_", rate, "_historical_", esm, "_", term, "_", min(range), "-", max(range), ".RDS")
      } else {
        paste0(fol, "/vocc_", rate, "_", ssp, "_", esm, "_", term, "_", min(range), "-", max(range), ".RDS")
      }
    }

    saveRDS(result, make_path("annual")) # km/year
    saveRDS(result_decadal, make_path("decadal")) # km/decade
      
  }
     
  # Full projected time series 2015-2100
  tic()
  walk(files, ~get_velocity(.x, years, "all")) 
  toc() # 20 min on Alice's machine

  # By terms
  tic()
  walk(term_list[2:5], function(term) { # No recent-past yet
    message("Processing: ", term, "-term")
    walk(files, ~get_velocity(.x, years, term))
  })
  toc() # 19 mins on Alice's machine

  # For the recent term (using OISST data instead of CMIP)
  tic()
  walk(oisst_files, ~get_velocity(.x, years, "recent")) 
  toc() # 8 sec
  
  beep(2)
  
  