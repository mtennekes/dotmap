js_write <- function(objects, vars, file) {
  #fs <- letters[1:length(objects)]
  
  tmp <- tempfile()
  lns <- unlist(mapply(function(o, v) {
    if (inherits(o, c("sf", "sfc"))) {
      suppressMessages(geojsonio::geojson_write(o, file = tmp))
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



#' Create dotmap website
#' 
#' Create dotmap website
#' 
#' @param dm project, created with \code{\link{dotmap_project}}
#' @param localhost localhost (i.e. location) of the tileserver. If \code{NULL} (default), the tiles are copied to the folder \code{index_files}. It is recommended to specify \code{localhost} for locally runned websites.
#' @importFrom  geojsonio geojson_write
#' @importFrom jsonlite toJSON
#' @export
create_dotmap_website <- function(dm, localhost = NULL) {
  wdir <- dm$dir_website
  unlink(wdir)
  dir.create(wdir, showWarnings = FALSE)
  file.copy(file.path(system.file(package = "dotmap"), "website_template/index_files"), wdir, recursive=TRUE)
  file.copy(file.path(system.file(package = "dotmap"), "website_template/index.html"), wdir)
  
  
  if (!is.null(localhost)) {
    check_localhost(path = dm$dir_htmlserver, var = dm$vars[1], localhost = localhost, result = "warning")
  } else {
    target_dir <- file.path(dm$dir_website, "index_files")
    dir.create(target_dir, showWarnings = FALSE)
    file.copy(dm$dir_htmlserver, target_dir, recursive=TRUE)
    localhost <- "index_files/htmlserver"
  }
  
  
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
  
  titles <- list(title = dm$title,
                 region_title = dm$region_title,
                 labels_title = dm$labels_title)
  
  
  dots_text_ids <- unlist(lapply(1:length(dm$z_from), function(k) {
    rep(k, (dm$z_to[k] - dm$z_from[k]) + 1)
  }))
  
  dots_text <- as.list(dm$dots_text[dots_text_ids])
  names(dots_text) <- paste0("z", zmin:zmax)
  
  dotmap_attr <- dm$dotmap_attr

  meta <- unname(mapply(name = dm$vars, label = dm$var_titles, levels = dm$var_labels, is_div = rep(TRUE, length(dm$vars)), list, SIMPLIFY = FALSE))
  js_write(list(shp, meta, settings, titles, dots_text, dotmap_attr, localhost), c("shp", "meta", "settings", "titles", "dots_text", "dotmap_attr", "localhost"), file.path(wdir, "index_files/vars.js")) 
  
  create_icons(dm, 16, 4)
  
  message("Dotmap website created at ", wdir)
  invisible(NULL)
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