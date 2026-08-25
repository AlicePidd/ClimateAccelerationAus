# Script to loop through OISST HTML pages and download the data for a set time period
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# Caveat Emptor
		# April 2021 modified for March 2026


source("Helpers.R") # Alice's helpers



# Set destination folder -------------------------------------------------------

download_folder <- "/Volumes/Jet_Drive/OISST/days" # Provide a path to store the downloaded OISST; DO NOT end with a "/"...I add that below


# Details for download ----------------------------------------------------

	yrs <- 2022:2025 # The years we want
	
		
# Set base URL for OISST downloads ---------------------------------------------
	
	url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr"

	
# Get the links that appear there as yyyymm dates ------------------------------
	
	pg_html <- read_html(url) # Read the HTML on the page
	month_nodes <- html_attr(html_nodes(pg_html, "a"), "href") %>%  # Extract the links
	  str_subset("\\d{6}") %>%  # Just the folders with 6 digits (i.e., one per month)
	  str_subset(str_c(yrs, collapse = "|")) # Subset again for only where the years we want appear...not that `str_c` pastes the years together, separated by "|" (OR)

# Function to get netCDFs for a month ------------------------------------------
	
	get_netCDF <- function(f) {
	  url_month <- str_c(url, "/", f)
	  day_html <- read_html(url_month) # Read the HTML
	  file_links <- html_attr(html_nodes(day_html, "a"), "href") %>%  # Extract the links
	    str_subset(".nc") # Just the netCDFs
	  file_links %>% 
	    walk(\(x) { # Initiate an internal "anonymous" function that runs for each element of `file_links`
	      out_file <- str_c(download_folder, "/", x) # The name of the file to output, including path
  	    if(!file.exists(out_file)) { # Only if the file does not exist
  	      download.file(str_c(url_month, "/", x), out_file)
  	      # tmp_file <- out_file %>% # Make a name for a temporary file
  	      #   str_replace(".nc", "_tmp.nc") # The file name is the same as the output file's, just with "_tmp" at the end
  	      # file.rename(out_file, tmp_file) # Rename the output file to be the temp file
  	      # e <- ext(130, 165, -50, -0) # Extent of the study area
  	      # out <- rast(tmp_file) %>% # Load the temp file
  	      #   crop(e) %>%  # Crop to extent
  	      #   terra::subset(str_detect(names(.), "sst")) # Select only the layer for sst (i.e, not the ones for "anom" or "err")
  	      # writeCDF(out, out_file, varname = "sst", longname = "Sea-surface temperature", unit = "Celcius") # Write the cropped file
  	      # terminal_code <- str_c("rm ", str_c(tmp_file)) # Terminal code to delete file — this is needed to avoid filling the "trash" on a Mac, as would happen with `file.remove`
  	        # system(terminal_code) # Execute the command in the terminal
  	      }
	      }
      )
    }

	
# Download in parallel ---------------------------------------------------------
	
	cores  <-  12 # How many cores can I deploy at the same time, leaving some resources for the OS?
	plan(multisession, workers = cores) # Allow those cores to do the work  
	future_walk(month_nodes, get_netCDF) # For each month, get process the daily files
	plan(sequential) # Back to single-core mode [default mode]

	beep(2)
