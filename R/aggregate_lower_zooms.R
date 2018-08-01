#' Aggregate population totales per pixel for lower zoom levels
#' 
#' @param dm dotmap_meta object
#' @param i tile row. If \code{NULL} (default) all rows are processed
#' @param j tile column. If \code{NULL} (default) all columns are processed
aggregate_lower_zooms <- function(dm, i=NULL, j=NULL, logfile=NULL, pkg="pkg") {
  
  
  nvars <- length(dm$m)
  nagg <- length(dm$z_res)
  
  for (a in 1:nagg) {
    for (k in 1:nvars) {
      dmk <- dm
      dmk$m <- dmk$m[k]
      
      dmk$z_from <- dmk$z_from[a]
      dmk$z_to <- dmk$z_to[a]
      dmk$z_res <- dmk$z_res[a]
      dmk$resname <- paste0("res", dmk$z_res)
      dmk$pop_table_name <- names(dmk$pop_tables)[k]
      
      if (dmk$z_from < dmk$z_res) aggregate_lower_zooms_one(dmk, i=i, j=j, logfile=logfile, pkg=pkg)
    }
  }
}


aggregate_lower_zooms_one <- function(dm, i, j, logfile, pkg="pkg") {
  
  zmin <- dm$z_from
  
  
  message("aggregate poppulation per pixel counts to zoom levels ", dm$z_res-1, " to ", zmin, logfile=NULL)
  
  dir <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)
  lapply(file.path(dir, (dm$z_res-1):zmin), function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
  
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  ri_res <- dm$ri[[paste0("z", dm$z_res)]]
  ri_min <- dm$ri[[paste0("z", zmin)]]
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
  
  pop_per_pix_matrix <- foreach(i=seti, .combine='+') %do% { 
    devtools::load_all(pkg)
    pop_per_pix_matrix <- matrix(0L, nrow=1001, ncol=dm$z_res-zmin)  
    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    for (j in setj) {
      

      message(i, " ", j, " (worker  ", Sys.getpid(), ")")
      filename <- file.path(dir, dm$z_res, paste0("pop_", i, "_", j, ".rdata"))
      if (file.exists(filename)) {
        load(filename)
        a <- array(a, dim=c(ri_res$px / ri_arr$nx, ri_res$py / ri_arr$ny, ncol(a)))  
        dima <- dim(a)
        b <- raster::brick(a)
        rm(a); gc()
        
        for (z in (dm$z_res-1):zmin) {
          b <- raster::aggregate(b, fact=2, fun=sum)
          a <- matrix(as.integer(raster::as.array(b)), ncol=dima[3])
          
          pop_per_pix_ij <- tabulate(rowSums(a), nbins = 1001)
          pop_per_pix_ij[1001] <- pop_per_pix_ij[1001] + (nrow(a) - sum(pop_per_pix_ij))
          
          pop_per_pix_matrix[,dm$z_res-z] <- pop_per_pix_matrix[,dm$z_res-z] + pop_per_pix_ij

          save(a, file = file.path(dir, z, paste0("pop_", i, "_", j, ".rdata")))
        }
      }
    }
    if (!is.null(logfile)) closeLog(f)
    pop_per_pix_matrix
  }
  
  for (z in (dm$z_res-1):zmin) {
    pop_per_pix <- pop_per_pix_matrix[,dm$z_res-z]
    save(pop_per_pix, file=file.path(dir, z, "pop_per_pixel_table.rdata"))
  }
}
