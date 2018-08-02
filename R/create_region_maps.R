#' Create region maps
#' 
#' Create maps of the regions at the resolution of specific zoom level, divided in tiles according to the arrangement of a lower zoom level.
#' 
#' @param dm dotmap_meta object
#' @export
create_region_maps <- function(dm, i=NULL, j=NULL, logfile=NULL, pkg="pkg") {
  region <- NULL
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  ri_res <- dm$ri[[paste0("z", max(dm$z_res))]]
  
  message("Creating region tiles at resolution ", ri_res$zoom, " and arrangement ", ri_arr$zoom)
  dir.create(dm$dir_tiles_areas, recursive = TRUE, showWarnings = FALSE)
  region <- readRDS(dm$file_shp_region)
  
  lookup <- dm$lookup
#browser()
  region <- st_sf(region)
  
 # st_bbox(region) <- ri_res$bbx
  
  region$color <- num2hexcol(1:dm$n, lookup = lookup)
  
  # if (inherits(region, "Spatial")) {
    # region@bbox <- ri_res$bbx
    # region <- SpatialPolygonsDataFrame(region, data=data.frame(color = num2hexcol(1:dm$n, lookup = lookup), stringsAsFactors = FALSE), match.ID = FALSE)
    
  # } else {
    #region$color = num2hexcol(1:dm$n, lookup = lookup)
  # }
  
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)

  get_abs_value <- function(a, b) {
    b[1] + a * (b[2] - b[1])
  }
  
  foreach(i=seti) %dopar% { 
    devtools::load_all(pkg)
    library(tmap)
    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    x1 <- ((i-1) / ri_arr$nx)
    x2 <- x1 + (1 / ri_arr$nx)
    for (j in setj) {
      y2 <- (ri_arr$ny-j+1) / ri_arr$ny
      y1 <- y2 - (1 / ri_arr$ny)
      message(x1," ", x2, " ", y1," ", y2, " (worker ", Sys.getpid(), ")")
      
      png(filename=file.path(dm$dir_tiles_areas, paste0("region_", i , "_", j, ".png")), width = ri_res$py/ri_arr$ny, height = ri_res$px/ri_arr$nx, antialias="none")
      
      X1 <- get_abs_value(x1, ri_res$bbx[c(1,3)])
      X2 <- get_abs_value(x2, ri_res$bbx[c(1,3)])
      Y1 <- get_abs_value(y1, ri_res$bbx[c(2,4)])
      Y2 <- get_abs_value(y2, ri_res$bbx[c(2,4)])
      
      #grid::grid.rect(gp=grid::gpar(fill="black"))
      print(tmap::tm_shape(region, xlim=c(X1,X2), ylim=c(Y1,Y2), relative=FALSE) + tmap::tm_fill(col="color") + tmap::tm_layout(inner.margins = 0, outer.margins = 0, frame=FALSE))
      dev.off()
      
      # m <- tmap::tm_shape(region, xlim=c(x1,x2), ylim=c(y1,y2), relative=TRUE) + tmap::tm_fill(col="color") + tmap::tm_layout(inner.margins=0, frame=FALSE)
      # tmap::save_tmap(m, filename=file.path(dm$dir_tiles_areas, paste0("region_", i , "_", j, ".png")), width = ri_res$py/ri_arr$ny, height = ri_res$px/ri_arr$nx, units = "px", outer.margins = 0)
    }
    if (!is.null(logfile)) closeLog(f)
  }
}
