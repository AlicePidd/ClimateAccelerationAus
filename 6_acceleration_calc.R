# Computing decadal climate acceleration from the decadal velocity layers
  # Written by Dave S and Alice P
    # 5 March 2026


# This script:
  # Takes inputs of annual climate velocity computed on the rolling 21-year windows (script #4)
  # Masks these to the EEZ only
  # Uses these masked velocities to compute climate acceleration (annual and decadal) per term, by calculating the slope (temptrend) of the velocities for this period
  # Saves the acceleration outputs as both annual and decadal, and splits them into SSP-term files, as well as keeping them as 4 files per SSP with 44 layers (per ESM (x11) and term (x4))
  # OISST (recent/historical term) is included




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  fn_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  vocc_dec_fol <- make_folder(source_disk, "2_velocity_rolling", "decadal")
  
  accel_dec_fol <- make_folder(source_disk, "4_acceleration_aus", "decadal") # Aus files
  accel_term_dec_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "decadal") # Aus files
  
  
  
  
# Source the velocity functions ------------------------------------------------
  
  dir(fn_fol, full.names = TRUE, pattern = "tempTrend") %>% # Get just the one fn
    map(source)

  
  
  
# Make IPCC term year ranges ---------------------------------------------------
  
  term_list[2:5]
  
  all_range <- 2021:2090 # Whole timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  intermediate_range <- 2061:2080
  long_range <- 2081:2090
  

    
# Acceleration function --------------------------------------------------------
  
  get_fastness <- function(ssp) {
    
    files <- dir(vocc_dec_fol, full.names = TRUE, pattern = ssp)
    oisst_files <- dir(vocc_dec_fol, full.names = TRUE, pattern = "OISST")
    
      do_ssps <- function(f, term, rate) { 
        
        ssp <- basename(f) %>% 
          str_split_i(., "_", 3)
        esm <- basename(f) %>% 
          str_split_i(., "_", 4)
        
          if (basename(f) %>% str_detect("OISST")) {
            term <- "recent"
          }

        # Populate the range of years (corresponding to the relevant term) to subset the full timeseries by 
          if(term == "all") {
            range <- all_range # Full time series
          } else if (term == "recent") {
            range <- recent_range
          } else {
            range <- get(paste0(term, "_range")) # Series of years in the term
          }
        
        # Subset timeseries
        r <- readRDS(f) %>% 
          terra::subset(., str_detect(names(.), paste(range, collapse = "|"))) # Get only the relevant layers per the term years
        
        # Mask it to the EEZ
        rr <- mask(r, reez)
        crs(rr) <- "EPSG:4326"

        # Calculate the slope i.e., acceleration of velocity, for each ssp, term, and esm
        slope <- tempTrend(rr, 3)[[1]]

        names(slope) <- paste0("slope_", rate, "-velocity_", ssp, "_", esm, "_", term)
        return(slope)
      }
    
        ssp_out_dec <- map(term_list[2:5], function(term) { # decadal acceleration
          message("Processing decadal: ", term, "-term")
          map(files, ~do_ssps(.x, term, "decadal"))
        }) %>%
          flatten() %>% 
          rast()
        saveRDS(ssp_out_dec, file = paste0(accel_dec_fol, "/acceleration_decadal-velocity_", ssp, "_aus.RDS"))

        ssp_term_dec <- map(term_list[2:5], ~{
          tt <- terra::subset(ssp_out_dec, grep(.x, names(ssp_out_dec), value = TRUE))
          saveRDS(tt, file = paste0(accel_term_dec_fol, "/acceleration_decadal-velocity_", ssp, "_", .x, "-term_aus.RDS"))
        })
        
      # OISST (historical)
        oisst_out <- do_ssps(oisst_files, "historical", "decadal") # Not super clean code, this will be re-run 4 times, but ehhh
        saveRDS(oisst_out, file = paste0(accel_dec_fol, "/acceleration_historical_recent-term_aus.RDS")) # Save in both places
        saveRDS(oisst_out, file = paste0(accel_term_dec_fol, "/acceleration_decadal-velocity_historical_recent-term_aus.RDS"))
        
  }
  
  tic()
  walk(ssp_list, get_fastness)
  toc() # 25 seconds on Alice's machine

  
  