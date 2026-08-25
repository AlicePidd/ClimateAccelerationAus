# Prepping OISST data for use in climate velocity acceleration
  # Written by Dave S
    # 5 March 2026


source("Helpers.R") # Alice's helpers



# Standardise files ------------------------------------------------------------

  input_folder <- "/Volumes/Jet_Drive/OISST"

# Standardise with old format of variable being sst  
  files <- dir(str_c(input_folder, "/days"), full.names = TRUE)
  fix_netcdf <- function(f) {
    outname <- f %>% 
      str_replace("/days", "/days_std")
    cdo_code <- str_c("cdo -L -selvar,sst ", f, " ", outname)
      system(cdo_code)  
    }
  cores  <-  12 # How many cores can I deploy at the same time, leaving some resources for the OS?
  plan(multisession, workers = cores) # Allow those cores to do the work  
    future_walk(files, fix_netcdf) # For each month, get process the daily files
  plan(sequential) # Back to single-core mode [default mode]
  
  
# Combine daily files with exisiting days --------------------------------------
  
  files <- c(
    str_c(input_folder, "/OISST_1982_2021.nc"),
    dir(str_c(input_folder, "/days_std"),
        full.names = TRUE)
    )
  outname <- files[1] %>% 
    str_replace("1982_2021", "1982-2025")
  cdo_code <- str_c("cdo -L -mergetime ", str_c(files, collapse = " "), " ", outname)
    system(cdo_code)

      
# Compute annual means and crop ------------------------------------------------
    
  input_file <- outname
  outname <- str_c(input_folder,
                   "/tos_Oyear_OISST_Aus_19820101-20251231.nc")
  cdo_code <- str_c("cdo -L -setctomiss,-999 -yearmean -sellonlatbox,103,177,-52,-3 ", input_file, " ", outname)
    system(cdo_code)
    
 