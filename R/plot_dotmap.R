#' Plot dotmap
#' 
#' @param dm dotmap_meta object
#' @param i rows. By default \code{NULL} which means all rows are taken
#' @param j colums. By default \code{NULL} which means all columns are taken
#' @param logfile logfile
#' @import png
#' @export
plot_dotmap <- function(dm, i=NULL, j=NULL, logfile=NULL) {
  nvars <- length(dm$m)
  nagg <- length(dm$z_res)

  message("Plot dotmap")
  
  for (a in 1:nagg) {
    for (k in 1:nvars) {
      dmk <- dm
      dmk$m <- dmk$m[k]
      
      dmk$z_from <- dmk$z_from[a]
      dmk$z_to <- dmk$z_to[a]
      dmk$z_res <- dmk$z_res[a]
      dmk$resname <- paste0("res", dmk$z_res)
      dmk$pop_table_name <- dmk$vars[k]
      dmk$settings <- dmk$settings[[k]]
      
      plot_dotmap_i(dmk, i=i, j=j, logfile=logfile)
    }
  }
}

plot_dotmap_i <- function(dm, i=NULL, j=NULL, logfile=NULL) {
  
  zmin <- dm$z_from
  
  setup <- dm$settings
    
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  setz <- dm$z_res:zmin

  setz_zoom <- if (dm$z_res < dm$z_arr) {
    (dm$z_res+1):dm$z_arr
  } else NULL
  
  if (!is.null(setz_zoom)) {
    patts <- lapply(setz_zoom, function(z) {
      fact <- (dm$z_arr - z) * 2
      
      if (fact==0) return(1)
      
      patt <- get_pattern(mx=fact, dm$tile_size, fact = 1/fact)
      if (!dm$transparent) patt <- array(rep(patt,3), dim=c(ts, ts, 3))
      patt
    })
  }
  
  
  
  

  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
  
  dir <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)
  #lapply(file.path(dir, (dm$z_res-1):zmin), function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
  
  #dirdotmap <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)

  subset_pop <- !all(setup$sub.pops)
  foreach(i=seti, .packages = c("png", "dotmap")) %dopar% { 
  #for (i in seti) {
    #devtools::load_all(pkg)
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
          
          #if (z==12 && i==1 && j==2) browser()
          
          x <- get_HCL_colors(a, L.delta=setup$L.delta, L.w=setup$L.w, zf=dm$z_res-z, transparent=dm$transparent, output = "rgb", L.lim = setup$L.lim, H1 = setup$H1, H.method=setup$H.method, C.max=100, palette=setup$palette)
          rm(a); gc()
          
          nr <- dm$ri[[rz]]$px / ri_arr$nx
          nc <- dm$ri[[rz]]$py / ri_arr$ny
          
          a2 <- array(x, dim=c(nr, nc, ncol(x)))
          
          png::writePNG(a2, target=file.path(dir, z, paste0("dotmap_", i, "_", j, ".png")))
          
          if (z == dm$z_res && !is.null(setz_zoom)) {
            for (zi in 1L:length(setz_zoom)) {
              z2 <-  setz_zoom[zi]
              
              fact <- (z2-z)*2
              a3 <- a2[rep(1L:nr, each = fact),
                       rep(1L:nc, each = fact), ]
              
              

              if (dm$transparent) {
                
                a4 <- a3
                a4[,,4] <- a4[,,4] * patts[[zi]]
              } else {
                a4 <- a3 * patts[[zi]] + (1-patts[[zi]])
              }
              
              
              dir.create(file.path(dir, z2), showWarnings = FALSE, recursive = TRUE)
              png::writePNG(a4, target=file.path(dir, z2, paste0("dotmap_", i, "_", j, ".png")))
            }
          }
          
        }
      }
      
    }
    if (!is.null(logfile)) closeLog(f)
  }
  invisible()
}
