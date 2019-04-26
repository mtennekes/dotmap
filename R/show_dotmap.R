#' Show dotmap
#' 
#' Show dotmap
#' 
#' @param dm dm
#' @param localhost localhost
#' @param show.region show.region
#' @export
show_dotmap <- function(dm, localhost = "http://127.0.0.1", show.region = TRUE) {
  region <- readRDS(dm$file_shp_region)
  zmin <- min(dm$z_from)
  zmax <- max(dm$z_to)
  
  
  nms <- dm$vars
  label.vars <- dm$var_titles
  
  if (is.na(label.vars[1])) {
    label.vars <- nms
  } else {
    if (length(nms) != length(label.vars)) stop("label.vars has ", length(label.vars), " values, whereas there are ", length(nms), " variables: ", paste(nms, collapse = ", "))
  }
  
  
  check_localhost(path = dm$dir_htmlserver, var = nms[1], localhost = localhost)
  
  md <- tmap_mode("view")
  
  tm <- tm_basemap("Esri.WorldGrayCanvas", group = NULL)
  
  for (i in 1:length(nms)) {
    nm <- nms[i]
    lb <- label.vars[i]
    tm <- tm + tm_basemap(file.path(localhost, nm, "{z}/{x}/{y}.png"), group = lb)
  }

  if (show.region) {
    tm <- tm + tm_shape(region) + tm_borders(group = dm$region_title)
  }
  
    
  if (show.region) {
    tm <- tm + tm_view(set.zoom.limits = c(zmin, zmax))
  } else {
    bbx <- bb(region, projection = "longlat")
    lng <- mean(bbx[c(1,3)])
    lat <- mean(bbx[c(2,4)])
    tm <- tm + tm_view(bbox = bb(region), set.zoom.limits = c(zmin, zmax))
  }

  tm
}


working_localhost <- function (url) 
{
  if (!capabilities(what = "http/ftp")) 
    return(FALSE)
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  !inherits(test, "try-error")
}


check_localhost <- function(path, var, localhost) {
  d1 <- list.files(file.path(path, var))[1]
  d2 <- list.files(file.path(path, var, d1))[1]
  d3 <- list.files(file.path(path, var, d1, d2))[1]
  
  onetile <- file.path(localhost, var, d1, d2, d3)
  chk <- working_localhost(onetile)
  
  if (!chk) stop("Cannot find tile server. Please check if the files are copied from ", path, " to ", localhost, " correctly. You can test it by opening ", onetile, " in your browser, which should show you a single png tile.")
  invisible()
}

