js_write <- function(objects, vars, file) {
  #fs <- letters[1:length(objects)]
  
  tmp <- tempfile()
  
  lns <- unlist(mapply(function(o, v) {
    if (inherits(o, c("sf", "sfc"))) {
      geojson_write(o, file = tmp)    
      lns <- suppressWarnings(readLines(tmp))
      unlink(tmp)
    } else {
      lns <- toJSON(o, digits = 4)
    }
    
    lns[1] <- paste0("var ", v, " = ", lns[1])  
    n <- length(lns)
    lns[n] <- paste0(lns[n], ";")
    lns
  }, objects, vars))
  
  writeLines(lns, con = file)
  invisible()
}



#' Write json data to website
#' 
#' Write json data to website
#' 
#' @param dm dm
#' @param localhost localhost
#' @import geojsonio
#' @import jsonlite
write_json_data <- function(dm, localhost) {
  wdir <- dm$dir_website
  unlink(wdir)
  dir.create(wdir, showWarnings = FALSE)
  file.copy(file.path(system.file(package = "dotmap"), "website_template/index_files"), wdir, recursive=TRUE)
  file.copy(file.path(system.file(package = "dotmap"), "website_template/index.html"), wdir)
  
  
  shp <- shp_to_4326_round(readRDS(dm$file_shp_region), 4)
  
  x1 <- unname(dm$bbx_shp[1])
  x2 <- unname(dm$bbx_shp[3])
  y1 <- unname(dm$bbx_shp[2])
  y2 <- unname(dm$bbx_shp[4])
  
  x <- (x1 + x2) / 2
  y <- (y1 + y2) / 2
  
  
  zmin <- min(dm$z_from)
  zmax <- max(dm$z_to)
  zcurrent <- zmin

  num2char <- function(x) formatC(x, format = "f", digits=3)
  
  settings <- list(x = num2char(x),
                   y = num2char(y),
                   x1 = num2char(x1),
                   x2 = num2char(x2),
                   y1 = num2char(y1),
                   y2 = num2char(y2),
                   zmin = zmin,
                   zmax = zmax,
                   zcurrent = zcurrent)
  
  
  
  meta <- unname(mapply(name = dm$vars, label = dm$var_titles, levels = dm$var_labels, is_div = rep(TRUE, length(dm$vars)), list, SIMPLIFY = FALSE))
  js_write(list(shp, meta, settings, localhost), c("shp", "meta", "settings", "localhost"), file.path(wdir, "index_files/vars.js")) 
  
  create_icons(dm, 16, 4)
}


shp_to_4326_round <- function(shp, digits) {
  shp <- st_transform(shp, crs = 4326)
  co <- st_coordinates(shp)
  co[,1] <- round(co[,1], digits = 2)
  co[,2] <- round(co[,2], digits = 2)
  
  g <- st_geometry(shp)
  
  g2 <- lapply(g, function(gi) {
    gi2 <- lapply(gi, function(gii) {
      lapply(gii, function(giii) {
        giii[] <- round(giii, digits = digits)
        giii
      })
    })
    class(gi2) <- class(gi)
    gi2
  })
  
  class(g2) <- class(g)
  st_sfc(g2, crs = 4326)
}