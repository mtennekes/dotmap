dotmap <- function(dm, localhost = "http://127.0.0.1", label.region = NA, label.vars = NA) {
  region <- readRDS(dm$file_shp_region)
  zmin <- min(dm$z_from)
  zmax <- max(dm$z_to)
  
  
  nms <- names(dm$pop_tables)
  
  if (is.na(label.vars[1])) {
    label.vars <- nms
  } else {
    if (length(nms) != length(label.vars)) stop("label.vars has ", length(label.vars), " values, whereas there are ", length(nms), " variables: ", paste(nms, collapse = ", "))
  }
  
  
  check_localhost(path = dm$dir_htmlserver, var = nms[1], localhost = localhost)
  
  md <- tmap_mode("view")
  
  if (is.na(label.region)) {
    tm <- tm_shape(region) + tm_borders()
  } else {
    tm <- tm_shape(region) + tm_borders(group = label.region)
  }
  
  
  for (i in 1:length(nms)) {
    nm <- nms[i]
    lb <- label.vars[i]
    tm <- tm + tm_tiles(file.path(localhost, nm, "{z}/{x}/{y}.png"), group = lb)
  }
  tm <- tm + tm_view(set.zoom.limits = c(zmin, zmax))
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

