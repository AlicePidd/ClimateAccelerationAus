# Plotting velocity and acceleration overlap spatially as categories
  # Written by Alice P
    # 30 March 2026
  # Inspired by Brito-Morales et al. 2020



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts/cropped") 
  accel_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial/pngs") # Aus files
  pdf_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial/pdfs") # Aus files
  
  
  
# Load rasters -----------------------------------------------------------------
  ## Projected
    vel_files <- dir(vocc_fol, full.names = TRUE, pattern = ".RDS") %>%
      str_subset("historical", negate = TRUE)
    accel_files <- dir(accel_fol,  full.names = TRUE, pattern = ".RDS") %>%
      str_subset("historical", negate = TRUE)
  
  ## Historical
    vel_hfiles <- dir(vocc_fol, full.names = TRUE, pattern = "historical")
    accel_hfiles <- dir(accel_fol,  full.names = TRUE, pattern = "historical") 
  
  load_stack <- function(files) {
    f <- files
    readRDS(f)[[1]]
  }

  ## Stack projected -------------
    vel_stack <- map(vel_files, load_stack)
    accel_stack <- map(accel_files, load_stack)
    accel_stack <- map(accel_stack, function(r) {
      names(r) <- str_remove(names(r), "-term")
      r
    })
    
  ## Stack historical -------------
    vel_hstack <- map(vel_hfiles, load_stack)
    accel_hstack <- map(accel_hfiles, load_stack)

  
  
  
# Bivariate classification by mixed thresholds based on each metric ------------
    ## Was previously using terciles
      # Splits into terciles, combines into codes 11-33
        # Classifies data as 1 = low, 2 = med, 3 = high)
 
  ## Pool values across every combo to get breaks -------------
    vel_vals <- map(vel_stack, function(r) values(r, na.rm = TRUE)) %>%
      unlist()
    accel_vals <- map(accel_stack, function(r) values(r, na.rm = TRUE)) %>%
      unlist()

    vel_hvals <- map(vel_hstack, function(r) values(r, na.rm = TRUE)) %>%
      unlist()
    accel_hvals <- map(accel_hstack, function(r) values(r, na.rm = TRUE)) %>% 
      unlist() 
    
      
  ## Breaking the data -------------
    ### For velocity: 
      # 1 = vel <0; 
      # 2 = vel ≥ 0 < quantile(., 0.25); 
      # 3 = vel ≥ quantile(., 0.25) i.e., everything else
      # vel_breaks <- quantile(vel_vals, 0.25) # 24.75361 (25th percentile of ALL values)
      # vel_breaks <- quantile(vel_vals[vel_vals >= 0], 0.25) # 27.7718 (25th percentile of non-negative values only)
      vel_breaks <- quantile(vel_vals[vel_vals >= 0], 0.30) # 32.16665 (30th percentile of non-negative values only)
      vel_breaks 
      vel_hbreaks <- quantile(vel_hvals[vel_hvals >= 0], 0.30) # 35.79337 (30th percentile of non-negative historical values only)
      vel_hbreaks 

    ### For accel: Middle-ish 30% method (for accel only) -------------
      # (abs val gives the abs max, then just make it negative for symmetrical breaks)
      accel_breaks <- quantile(abs(accel_hvals), 0.15) 
      accel_breaks # 0.3965666
      

  
# Classify each rast using fixed global breaks ---------------------------------
    
  ## For velocity -------------
    get_vel_brks <- function(r, breaks) { # Tertile-ish method
      classify(r, rcl = matrix(c(-Inf, 0, 1,  # vel < 0
                                 0, breaks[[1]], 2,  # vel >= 0 and < q25
                                 breaks[[1]], Inf, 3),  # vel >= q25
                               ncol = 3, byrow = TRUE),
               right = FALSE) # intervals closed on left, open on right
    }

      
  ## For acceleration -------------
    get_accel_brks <- function(r, breaks) {
      classify(r, rcl = matrix(c(-Inf, -breaks[[1]], 1, # Infinity to the negative conversion of teh abs 15%
                                 -breaks[[1]], breaks[[1]], 2, # Between negative 15% to positive 15% (mid-ish 30%)
                                 breaks[[1]], Inf, 3), # positive 15% and above
                               ncol = 3, byrow = TRUE))
    }
      
      # NEW CLASSIFICATION WHERE NEGATIVE-ACCELERATION (pos) = GOOD/STABILISING, NEGATIVE-DECELERATION (neg) = BAD/INTENSIFYING
    get_accel_brks_neg <- function(r, breaks) {  
      classify(r, rcl = matrix(c(-Inf, -breaks[[1]], 3, # positive 15% and above (more negative = accelerating)
                                 -breaks[[1]], breaks[[1]], 2, # Between negative 15% to positive 15% (mid-ish 30%)
                                 breaks[[1]], Inf, 1), # Infinity to the negative conversion of teh abs 15% (trending to 0 = decelerating)
                               ncol = 3, byrow = TRUE))
    }
  
      
  ## Function to do it -------------

    bivar_classify_global <- function(r_vel, r_acc) { # NEW CLASSIFICATION WHERE NEGATIVE-ACCELERATION = GOOD, NEGATIVE-DECELERATION
      # vel_class <- get_vel_brks(r_vel, vel_breaks)
      vel_class <- get_vel_brks(r_vel, vel_hbreaks)
      acc_class_pos <- get_accel_brks(r_acc, accel_breaks)
      acc_class_neg <- get_accel_brks_neg(r_acc, accel_breaks)
      acc_class <- ifel(vel_class == 1, acc_class_neg, acc_class_pos)
      vel_class * 10 + acc_class
    }
    

      
  ## Checking name order and fixing -------------
    term_order <- c("near", "mid", "intermediate", "long")
    combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])  # adjust indices as needed
    combos
      
    nms <- paste0("median_", combos$ssp, "_", combos$term) # names we need in combos order
    nms
    
    vel_nms <- map_chr(vel_stack, names) # get actual names of each stack
    accel_nms <- map_chr(accel_stack, names)
    
    vel_stack_ordered <- vel_stack[match(nms, vel_nms)] # reorder both stacks to match combos order
    accel_stack_ordered <- accel_stack[match(nms, accel_nms)]
    
      map_chr(vel_stack_ordered, names) # checking order
      map_chr(accel_stack_ordered, names)
    
    bivar_rasts <- map2(vel_stack_ordered, accel_stack_ordered, bivar_classify_global)

    
  
  
# Plot -------------------------------------------------------------------------
  ssp <- ssp_list[2]
  plot_bivariate <- function(ssp, pal_name) {
 
    # Order panels by term_order
    panel_idx <- which(combos$ssp == ssp)
 
    # Maps
      map_dfs <- map(bivar_rasts[panel_idx], function(r) {
        as.data.frame(r, xy = TRUE) %>% # Turn it into a df
          rename(code = 3) %>% # rename 3rd column to code
          drop_na() %>%
          mutate(code = factor(code)) # change it to a factor
      })
    
      make_map <- function(df, ssp, term) {
        ggplot(data = df) + 
          geom_raster(aes(x, y, 
                          fill = code)) +
          scale_fill_manual(values = bivar_pal, drop = FALSE, guide = "none") +
          geom_sf(data = eez_shp, fill = NA, 
                  colour = "black", linewidth = 0.3, 
                  inherit.aes = FALSE) +
          geom_sf(data = oceania_stanford_shp, 
                  fill = "grey80", colour = NA, 
                  inherit.aes = FALSE) +
          coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
          labs(title = toupper(ssp), subtitle = term) +
          theme_void(base_size = 9) +
          theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, colour = "grey40"))
      }
      
      combos_ssp <- combos %>%
        filter(ssp == !!ssp) # Filter by ssp, need !! so it knows its a separate variable that has been passed
      combos_ssp$r <- bivar_rasts[panel_idx]
      combos_ssp <- combos_ssp %>%
        mutate(df = map(r, \(r) as.data.frame(r, xy = TRUE) %>%
                          rename(code = 3) %>%
                          drop_na() %>%
                          mutate(code = factor(code))))
      
      map_panels <- pmap(list(combos_ssp$df, combos_ssp$ssp, combos_ssp$term), make_map)

    # Legend
      legend <- expand_grid(vel = 1:3, acc = 1:3) %>%
        mutate(code  = factor(vel * 10 + acc),
               vel_l = factor(vel, 1:3, c("Negative", "Slow", "Fast")),
               acc_l = factor(acc, 1:3, c("Decel",  "Stable", "Accel"))) %>%
        ggplot(aes(acc_l, vel_l, fill = code)) +
        geom_tile(colour = "white", linewidth = 0.8) +
        scale_fill_manual(values = bivar_pal, guide = "none") +
        labs(x = "Acceleration \u2192", y = "Velocity \u2192") +
        theme_minimal(base_size = 8) +
        theme(panel.grid = element_blank(), aspect.ratio = 1)
 
    # Bar chart
      corner_codes <- c("Negative velocity, Decelerating"  = 11, "Negative velocity, Accelerating" = 13,
                        "Fast velocity, Decelerating"  = 31, "Fast velocity, Accelerating" = 33)
      
      bar_data <- combos_ssp %>%
        mutate(term = factor(term, levels = term_order)) %>%
        rowwise() %>%
        reframe(term = term,
                category = names(corner_codes),
                prop = map_dbl(corner_codes, \(code) mean(values(r, na.rm = TRUE)[, 1] == code) * 100))
   
      bar <- bar_data %>%
        mutate(category = factor(category, levels = names(corner_pal))) %>%
        ggplot(aes(term, prop, fill = category)) +
        geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
        scale_fill_manual(values = corner_pal, name = NULL) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = NULL, y = "Proportion of cells (%)") +
        theme_classic(base_size = 9) +
        theme(axis.text.x     = element_text(angle = 35, hjust = 1),
              legend.position = "bottom",
              legend.text     = element_text(size = 7)) +
        guides(fill = guide_legend(ncol = 1))
 
    # Assemble and save
      fig <- wrap_plots(
        wrap_plots(map_panels, ncol = 1),
        wrap_plots(legend, bar, plot_spacer(), ncol = 1, heights = c(1, 1, 2)),
        ncol = 2, widths = c(4, 1)
      ) +
        plot_annotation(tag_levels = "a") &
        theme(plot.margin = margin(10, 10, 10, 10))
   
      o_nm <- paste0(plot_fol, "/velocity_acceleration_bivariate_", ssp, "_pal-", pal_name, "_mixedthresholds_30vhist_15ahist_revaccel.png")
      ggsave(filename = o_nm, plot = fig, width = 12, height = 20)
      
      o_nm_pdf <- paste0(pdf_fol, "/velocity_acceleration_bivariate_", ssp, "_pal-", pal_name, "_mixedthresholds_30vhist_15ahist_revaccel.pdf")
      ggsave(filename = o_nm_pdf, plot = fig, width = 12, height = 20)
      
      message("Saved: ", basename(o_nm))
  }
 
  

    
# Palettes ----------------------------------------------------------------------
  # First digit = velocity (1 = neg, 3 = fast)
  # Second digit = acceleration (1 = decel, 3 = accel)
 
  ## tealochre1
  ##**THIS ONE**

  bivar_pal <- c("11"="#F8F4E4","12"="#F0C050","13"="#DA9500",
                 "21"="#60C8D0","22"="#789080","23"="#501700", #513700
                 "31"="#008089","32"="#003F5A","33"="#001911")
  corner_pal <- c("Negative velocity, Decelerating" ="#F8F4E4",
                  "Negative velocity, Accelerating"="#DA9500",
                  "Fast velocity, Decelerating" ="#008089",
                  "Fast velocity, Accelerating"="#001911")
  walk(ssp_list, ~ plot_bivariate(.x, pal_name = "tealochre1"))
  
