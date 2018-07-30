#' Plot dotmap
#' 
#' @param dm dotmap_meta object
#' @param delta parameter
#' @param w parameter
#' @param ... arguments passed on to get_HCL_colors
#' @import png
plot_dotmap <- function(dm, i=NULL, j=NULL, z=NULL, logfile=NULL, pkg="pkg") {
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
      
      plot_dotmap_i(dmk, i=i, j=j, logfile=logfile, pkg=pkg)
    }
  }
  
}
plot_dotmap_i <- function(dm, i=NULL, j=NULL, z=NULL, logfile=NULL, pkg="pkg") {
  
  zmin <- dm$z_from
  
  setup <- dm$settings
    
  message("Plot dotmap")
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  setz <- if (is.null(z)) {
    dm$z_res:zmin
  } else z
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
  
  dir <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)
  lapply(file.path(dir, (dm$z_res-1):zmin), function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
  
  #dirdotmap <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)

  subset_pop <- !all(setup$sub.pops)
  foreach(i=seti) %dopar% { 
    devtools::load_all(pkg)
    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    for (j in setj) {
      for (z in setz) {
        rz <- paste0("z", z)
        cat(z, i, j, " (worker ", Sys.getpid(), ")\n")
        filename <- file.path(dir, z, paste0("pop_", i, "_", j, ".rdata"))
        if (file.exists(filename)) {
          load(filename)
          if (subset_pop) a[, !setup$sub.pops] <- 0L
          x <- get_HCL_colors(a, L.delta=setup$L.delta, L.w=setup$L.w, zf=dm$z_res-z, transparent=dm$transparent, output = "rgb", L.lim = setup$L.lim, H1 = setup$H1, H.method=setup$H.method, C.max=100, palette=setup$palette)
          rm(a); gc()
          a2 <- array(x, dim=c(dm$ri[[rz]]$px / ri_arr$nx, dm$ri[[rz]]$py / ri_arr$ny, ncol(x)))
          
          png::writePNG(a2, target=file.path(dir, z, paste0("dotmap_", i, "_", j, ".png")))
        }
      }
    }
    if (!is.null(logfile)) closeLog(f)
  }
  invisible()
}
