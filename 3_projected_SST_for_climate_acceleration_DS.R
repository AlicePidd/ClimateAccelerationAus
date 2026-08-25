# Prepping CMIP_regridded_OISST_DBC_sst for use in climate velocity acceleration
  # Written by Dave S
    # 4 March 2026


source("Helpers.R") # Alice's helpers



# Folders ----------------------------------------------------------------------

  input_folder <- "/Volumes/Giga_Disk/CMIP_regridded_OISST_DBC_sst"
  output_folder <- "/Volumes/Jet_Drive_too/CMIP_regridded_OISST_DBC_sst_annual_Aus"

  
# Compute annual means and crop ------------------------------------------------
  
  files <- dir(input_folder, full.names = TRUE) %>% 
    str_subset("_ensemble_", negate = TRUE)
  
  anmean_aus <- function(f) {
    outname <- f %>%
      str_replace(input_folder, output_folder) %>% 
      str_replace("_Oday_", "_Oyear_") %>% 
      str_replace("_RegriddedBiasCorrected_", "_RegriddedBiasCorrectedAus_")
    cdo_code <- str_c("cdo -L -yearmean -sellonlatbox,103,177,-52,-3 ", f, " ", outname)
      system(cdo_code)
    }
  
  plan(multisession, workers = 5)
    future_walk(files, anmean_aus)
  plan(sequential)
