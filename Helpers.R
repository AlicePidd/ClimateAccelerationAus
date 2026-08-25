# Helper functions for thesis chapter 3: Climate connectivity
	# For working with climate velocity tracers
    # Adapted from Chp2 Helpers.R
  	# Updated by Alice, Feb 2025



# Packages ---------------------------------------------------------------------

pacman::p_load(RCurl, xml2, rvest, tidyverse, furrr, future, terra, parallel, beepr, tictoc, sf, scales, patchwork, ggthemes, RColorBrewer)



# Palettes ----------------------------------------------------------------

  col_ssp126 <- rgb(0, 52, 102, maxColorValue = 255)
  col_ssp245 <- rgb(112, 160, 205, maxColorValue = 255)
  col_ssp370 <- rgb(196, 121, 0, maxColorValue = 255)
  col_ssp585 <- rgb(153, 0, 2, maxColorValue = 255)
  
  IPCC_pal <- c("ssp126" = col_ssp126, "ssp245" = col_ssp245, "ssp370" = col_ssp370, "ssp585" = col_ssp585)




# Folder functions -------------------------------------------------------------
  	
	make_folder <- function(d, m, fol_dir_name) {
	  
	  folder_path <- file.path(paste0(d, "/", m, "/", fol_dir_name))
	  if (!dir.exists(folder_path)) {
	    dir.create(folder_path, recursive = TRUE)
	    message("✅ Folder created: ", folder_path)
	  } else {
	    message("📂 Folder already exists: ", folder_path)
	  }
	  return(folder_path)
	}
	

  
		