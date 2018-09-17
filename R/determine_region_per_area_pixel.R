#' Determine region per area pixel
#' 
#' The maps created with \code{\link{create_area_maps}} are broken down by the maps created with \code{\link{create_region_maps}}. For each pixel in the first maps, the region id from the second maps are determined.
#' 
#' @param dm dotmap_meta object
#' @param i tile row. If \code{NULL} (default) all rows are processed
#' @param j tile column. If \code{NULL} (default) all columns are processed
#' @param logfile logfile
#' @param dens_ub upperbound of the density per class (e.g. urbanization level)
#' @param dens_lb lowerbound of the density per class (e.g. urbanization level)
#' @import png
determine_region_per_area_pixel <- function(dm, i=NULL, j=NULL, logfile=NULL, pkg="pkg") {
  message("Determine region per area pixel")
  
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  
  dens_ub <- dm$dens_ub
  dens_lb <- dm$dens_lb
  
  dir <- dm$dir_tiles_areas

  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)

  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)

  lookup <- dm$lookup
  
  useArea2 <- (any(substr(list.files(dir), 1, 5) == "area2"))

  cat("useArea2", useArea2, "\n")
  tabs <- foreach(i=seti, .combine='+') %dopar% { 
    devtools::load_all(pkg)
    library(png)
    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    
    tab1 <- rep(0L, dm$n)
    tab2 <- rep(0L, dm$n)
    for (j in setj) {
      b <- readPNG(file.path(dir, paste0("region_", i, "_", j, ".png")))
      
      message(i, " ", j, " (worker ", Sys.getpid(), ")")
      
      if (any(b!=1)) {
        
        ids1 <- as.integer(rgb2num(b[,,1], b[,,2], b[,,3], lookup = lookup))
        ids1[ids1==16^6] <- NA
        ids2 <- ids1
        rm(b)
        
        if (file.exists(file.path(dir, paste0("area1_", i, "_", j, ".png")))) {
          w <- readPNG(file.path(dir, paste0("area1_", i, "_", j, ".png")))
          w <- !as.logical(as.vector(w[,,1]))
          
          ids1[!w] <- NA
          rm(w)
        }
        
        if (file.exists(file.path(dir, paste0("area2_", i, "_", j, ".png")))) {
          w2 <- readPNG(file.path(dir, paste0("area2_", i, "_", j, ".png")))
          w2 <- !as.logical(as.vector(w2[,,1]))
          
          ids2[!w2] <- NA
          rm(w2)
        }
        
        tab1 <- tab1 + tabulate(ids1, nbins=dm$n)
        tab2 <- tab2 + tabulate(ids2, nbins=dm$n)
        save(ids1, file=file.path(dir, paste0("area_region1_", i, "_", j, ".rdata")))
        save(ids2, file=file.path(dir, paste0("area_region2_", i, "_", j, ".rdata")))
        
        gc()
      } else {
        rm(b)
      }
      gc()
    }
    if (!is.null(logfile)) closeLog(f)
    c(tab1, tab2)
  }
  
  tab1 <- tabs[1:dm$n]
  tab2 <- tabs[(dm$n+1):(2*dm$n)]
  
  rmeta <- dm$pop_totals
  

  # number of available pixels
  rmeta$pixels1 <- tab1
  rmeta$pixels2 <- tab2
  
  # calculate area size
  rmeta$area1_km2 <- rmeta$pixels1 * dm$area_1pix / 1e6
  rmeta$area2_km2 <- rmeta$pixels2 * dm$area_1pix / 1e6

  # calculate pop density
  rmeta$pop_area1_km2 <- rmeta$pop / rmeta$area1_km2
  
  if (is.null(dens_ub)) {
    rmeta$pop1 <- pmin(rmeta$pop, rmeta$pixels1)
    rmeta$pop2 <- if (useArea2) {
      pmin(rmeta$pop - rmeta$pop1, rmeta$pixels2)
    } else {
      0
    }
  } else {
    rmeta$clip1 <- rmeta$pop_area1_km2 > (dens_ub[rmeta$class])
    rmeta$too_sparse1 <- rmeta$pop_area1_km2 < (dens_lb[rmeta$class])
    
    rmeta$pop1 <- ifelse(rmeta$clip1, (dens_ub[rmeta$class]) * rmeta$area1_km2, 
                         ifelse(rmeta$too_sparse1, 0, rmeta$pop))
    
    # calculate density of remaining population for area2
    rmeta$pop_area2_km2 <- (rmeta$pop - rmeta$pop1) / rmeta$area2_km2
    
    rmeta$clip2 <- rmeta$pop_area2_km2 > (dens_ub[rmeta$class])
    rmeta$too_sparse2 <- rmeta$pop_area2_km2 < (dens_lb[rmeta$class])
    
    rmeta$sample_area2 <- rmeta$clip1 & (!rmeta$too_sparse2)
    
    rmeta$pop2 <- ifelse(rmeta$sample_area2, 
                         ifelse(rmeta$clip2, (dens_ub[rmeta$class]) * rmeta$area2_km2, rmeta$pop-rmeta$pop1), 0)
    
  }
  
    
  saveRDS(rmeta, file = file.path(dir, "rmeta_pixels.rds"))
}