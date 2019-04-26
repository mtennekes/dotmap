#' Create area maps
#' 
#' Create maps of the available area at the resolution of specific zoom level, divided in tiles according to the arrangement of a lower zoom level.
#' 
#' @param dm dotmap_meta object
#' @param i tile row. If \code{NULL} (default) all rows are processed
#' @param j tile column. If \code{NULL} (default) all columns are processed
#' @param primary plot primary areas (\code{"area1"}), or secundary (\code{"area2"})?
#' @param logfile logfile
#' @import grid
#' @import png
#' @import tmap
#' @import tmaptools
#' @import dplyr
#' @import tidyr
#' @importFrom data.table data.table setkey setkeyv ':='
#' @importFrom raster brick aggregate
#' @import randtoolbox
#' @importFrom fastmatch fmatch
#' @import abind
#' @import doParallel
#' @import parallel
#' @import foreach
#' @import entropy 
#' @rdname create_area_maps
#' @export
create_area_maps <- function(dm, i=NULL, j=NULL, primary=TRUE, logfile=NULL) {
  area1 <- area2 <- NULL
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  ri_res <- dm$ri[[paste0("z", max(dm$z_res))]]
  
  message("Creating area tiles at resolution ", ri_res$zoom, " and arrangement ", ri_arr$zoom)
  dir.create(dm$dir_tiles_areas, recursive = TRUE, showWarnings = FALSE)
  
  if (primary) {
    area1 <- readRDS(dm$file_shp_area1)
    area1 <- sf::st_transform(area1, crs = 3857)
    # area1@bbox <- ri_res$bbx
    #attr(area1, "bbox")[1:4]  <- as.vector(ri_res$bbx)[1:4]
  } else {
    area2 <- readRDS(dm$file_shp_area2)
    area2 <- sf::st_transform(area2, crs = 3857)
    # load(dm$file_shp_area2)
    # area2@bbox <- ri_res$bbx
  }
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
  openLog <- openLog; closeLog <- closeLog
  
  foreach(i=seti, .packages = c("dotmap", "tmap", "tmaptools")) %dopar% { 
    #devtools::load_all(pkg)

    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    x1 <- ((i-1) / ri_arr$nx)
    x2 <- x1 + (1 / ri_arr$nx)
    for (j in setj) {
      y2 <- (ri_arr$ny-j+1) / ri_arr$ny
      y1 <- y2 - (1 / ri_arr$ny)
      message(x1," ", x2, " ", y1," ", y2, " (worker ", Sys.getpid(), ")")
      
      if (primary) {
        
        bb_tile <- tmaptools::bb(ri_res$bbx, xlim=c(x1,x2), ylim=c(y1,y2), relative=TRUE)
        
        png(filename=file.path(dm$dir_tiles_areas, paste0("area1_", i , "_", j, ".png")), width = ri_res$py/ri_arr$ny, height = ri_res$px/ri_arr$nx, antialias="none")
        
        par(mar=rep(0,4), xaxs="i", yaxs="i")
        
#        grid::grid.rect(gp=grid::gpar(fill="black"))
        print(tmap::tm_shape(area1, bbox = bb_tile) + tmap::tm_polygons(col="black", border.col="black") + tmap::tm_layout(inner.margins = 0, outer.margins = 0, frame=FALSE))
        
        # par(mar=rep(0,4), xaxs="i", yaxs="i")
        # 
        # area1$dummy <- 1 # Error in .subset2(x, i, exact = exact) : attempt to select less than one element in get1index when set col
        # 
        # plot(area1, xlim=bb_tile[1,], ylim=bb_tile[2,], setParUsrBB=TRUE, col="black", border="black", lwd=1, main=NULL)
        dev.off()
        
      } else {
        
        bb_tile <- tmaptools::bb(ri_res$bbx, xlim=c(x1,x2), ylim=c(y1,y2), relative=TRUE)
        
        png(filename=file.path(dm$dir_tiles_areas, paste0("area2_", i , "_", j, ".png")), width = ri_res$py/ri_arr$ny, height = ri_res$px/ri_arr$nx, antialias="none")
        
        par(mar=rep(0,4), xaxs="i", yaxs="i")
        
        #        grid::grid.rect(gp=grid::gpar(fill="black"))
        print(tmap::tm_shape(area2, bbox = bb_tile) + tmap::tm_polygons(col="black", border.col="black") + tmap::tm_layout(inner.margins = 0, outer.margins = 0, frame=FALSE))
        
        dev.off()
      }
    }
    if (!is.null(logfile)) closeLog(f)
  }
  invisible()
}

#' @name create_area_maps_sec
#' @rdname create_area_maps
create_area_maps_sec <- function(dm, i=NULL, j=NULL, primary=FALSE, logfile=NULL) {
  create_area_maps(dm=dm, i=i, j=j, primary=primary, logfile=logfile)
}

#' @name subtract_area_maps
#' @rdname create_area_maps
subtract_area_maps <- function(dm, i=NULL, j=NULL, logfile=NULL) {
  f1 <- "area1"
  f2 <- "area2"
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  ri_res <- dm$ri[[paste0("z", max(dm$z_res))]]
  
  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
  openLog <- openLog; closeLog <- closeLog
  
  foreach(i=seti, .packages = "dotmap") %dopar% { 
    #devtools::load_all(pkg)
    for (j in setj) {
      a1 <- png::readPNG(file.path(dm$dir_tiles_areas, paste0(f1, "_", i , "_", j, ".png")))
      a2 <- png::readPNG(file.path(dm$dir_tiles_areas, paste0(f2, "_", i , "_", j, ".png")))
      a2[a1==0] <- 1
      png::writePNG(a2, target = file.path(dm$dir_tiles_areas, paste0(f2, "_", i , "_", j, ".png")))
    }
  }
}


