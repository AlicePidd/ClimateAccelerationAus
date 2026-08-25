# Background data
  # Written by Alice Pidd (alicempidd@gmail.com)
    # Mar 2026 for Climate Acceleration



# Source the helpers and necessary bits -----------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"

  shps_fol <- make_folder(source_disk, "", "_shapefiles")
  helper_fol <- make_folder(source_disk, "", "_helpers")
  
  
  
# Source data and shapefiles  --------------------------------------------------
  
  e1 <- ext(105, 175, -50, -5) # EXTENT WITHOUT COCOS KEELING/XMAS ISLAND
  base_r <- rast(ext = e1, res = 0.25) # Res of the new tracers
  
  ssp_list <- c("ssp126", "ssp245", "ssp370", "ssp585")
  term_list <- c("recent", "near", "mid", "intermediate", "long")
  
  
  
# Source shapefiles  -----------------------------------------------------------
  
  aus_detailed_shp <- readRDS(paste0(shps_fol, "/aus_shapefile_detailed.RDS")) # Very detailed! Includes all islands offshore!
  oceania_stanford_shp <- readRDS(paste0(shps_fol, "/oceania_shapefile.RDS"))
  eez_shp <- readRDS(paste0(shps_fol, "/EEZ_shapefile.RDS"))
  mpa_shp <- readRDS(paste0(shps_fol, "/mpas_joined_shapefile_newarea.RDS"))

  raus <- terra::rasterize(aus_detailed_shp, base_r, touches = TRUE)
  raus <- ifel(is.na(raus), 1, NA) # Flip it though
  reez <- terra::rasterize(eez_shp, base_r) # Base raster for just MPAs
  rmpa <- terra::rasterize(mpa_shp, base_r, touches = TRUE) # Base raster for all MPAs, including GBR

  
  
# Create landsea mask using the OISST file (it's better) -----------------------
  
  # # Make it
  # mask <- rast("/Volumes/AliceShield/acceleration_data/1_OISST_annual_Aus/tos_Oyear_OISST_Aus_19820101-20251231.nc")[[1]]
  # plot(mask)
  # mask <- ifel(is.na(mask), NA, 1) %>% 
  #   crop(., e1)
  # mask
  # plot(mask)
  # saveRDS(mask, file = paste0(shps_fol, "/mask.RDS"))
  
  # Load it
  mask_land <- readRDS(paste0(shps_fol, "/mask.RDS"))

  
