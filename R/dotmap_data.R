check_shape <- function(dir,
                        shp,
                        required) {
  
  object <- deparse(substitute(shp))
  f <- file.path(dir, "source", paste(object, "rds", sep = "."))
  if (is.null(shp)) {
    if (!file.exists(f)) {
      if (required) stop(object, " not found. Either specify it, or save it as ", f)
      return(NULL)
    } 
  } else {
    if (inherits(shp, "sf")) {
      shp <- sf::st_geometry(shp)
    }
    if (!inherits(shp, "sfc")) stop(object, " is neither an sf nor an sfc object")
    saveRDS(shp, file = f)
  }
  f
}

#' Create dotmap project
#' 
#' Create dotmap project
#' 
#' @param dir dir
#' @param area1 area1
#' @param area2 area2
#' @param region region
#' @param pop_totals pop_totals
#' @param pop_tables pop_tables
#' @param dens_ub dens_ub
#' @param dens_lb dens_lb
#' @param bbx bbx
#' @param z z
#' @param z_arr z_arr
#' @param tile_size tile_size
#' @param transparent transparent
#' @param crs crs
#' @param settings settings
#' @export
#' @import grid
#' @import png
#' @import tmap
#' @import tmaptools
#' @import sf
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
dotmap_project <- function(dir,
                        area1 = NULL,
                        area2 = NULL,
                        region = NULL,
                        pop_totals,
                        pop_tables,
                        # project,
                        dens_ub=NULL,
                        dens_lb=NULL,
                        bbx=NA,
                        z,
                        z_arr,
                        tile_size=256,
                        transparent=TRUE,
                        crs,
                        settings
                        #vars
                        ) {

  
  dir.create(file.path(dir, "source"), recursive = TRUE, showWarnings = FALSE)
  
  file_shp_area1 <- check_shape(dir, area1, required = TRUE)
  file_shp_area2 <- check_shape(dir, area2, required = FALSE)
  file_shp_region <- check_shape(dir, region, required = TRUE)
  
  
  

  # dotmap_data_vars <- function(settings, vars) {
  #   rmeta <- settings$file_region_meta
  #   n <- nrow(rmeta)
  #   
  #   nc <- nchar(settings$file_pop)
  #   lapply(vars, function(v) {
  #     
  #     settings$file_pop <- paste0(substr(settings$file_pop, 1, nc-6),
  #                                 "_",
  #                                 v,
  #                                 ".rdata")
  #     
  #     if (!file.exists(settings$file_pop)) {
  #       stop("File ", settings$file_pop, " does not exist.")
  #     }
  #     load(settings$file_pop)
  #     if (n!=nrow(pop)) stop("Number of regions (from ", settings$file_region_meta, " ) does not correspond to number of rows in pop from pop_data.rdata")
  #     
  #     settings$dir_dotmap_data <- paste0(settings$dir_dotmap_data, "_", v)
  #     settings
  #   })
  # }
  
  if (is.vector(pop_totals)) pop_totals <- data.frame(pop = pop_totals)
  
  ### check region
  region <- readRDS(file_shp_region)
  if (!inherits(region, "sfc")) stop(file_shp_region, " is not an sfc object")
  n <- nrow(pop_totals)
  if (n != length(region)) stop("number of rows in pop_table (", n, ") does not correspond with number of regions in the region shape (", length(region),")")

  
  ### check pop_totals
  if (!inherits(pop_totals, "data.frame")) stop("pop_totals should be a vector or a data.frame")
  if (!all(names(pop_totals) %in% c("pop", "class")) || !("pop" %in% names(pop_totals))) stop("pop_totals should contain the columns \"pop\" (required) and \"class\" (optional).")
  if (any(is.na(pop_totals))) stop("pop_totals contains NAs")

  
  ### check pop_tables
  m <- mapply(function(tab, name) {
    if (nrow(tab) != n) stop("number of rows in table ", name, "(", nrow(tab), ") does not correspond to the number of regions (", n, ")")
    if (any(is.na(tab))) stop("table ", name, " contains NAs")
    ncol(tab)
  }, pop_tables, names(pop_tables))
  
  
  #file_pop <- file.path(project, "source/pop_data.rdata")
  dir_tiles_areas <- file.path(dir, "tiles_area")
  dir_dotmap_data <- file.path(dir, "dotmap_data")
  
  dir_htmlserver <- file.path(dir, "htmlserver")
  file_lookup <- file.path(dir, "lookup.rdata")

  ### check classes and dens_ub and dens_lb  
  hasclass <- "class" %in% names(pop_tables) && !missing(dens_ub) && !missing(dens_lb)
  if (hasclass) {
    if (!missing(dens_ub)) {
      k <- length(dens_ub)    
      if (length(dens_lb)!=k) stop("Lengths of dens_ub and dens_lb are not the same")
    } 
    if (!is.numeric(pop_tables$class)) stop("class column of rmeta is not numeric")
    if (!all(pop_tables$class %in% 1L:k)) stop("values in rmeta$class do not match 1:length(dens_ub)")
  } else {
    k <- 1
  }
  
  ### generate loopup table (numbers that have a one to one mapping to colors in hex format)
  lookup <- get_lookup(n)
  #save(lookup, file = file_lookup)
  
  
  if (is.na(bbx[1])) {
    bbx <- tmaptools::bb(region, projection = "longlat")
  } else {
    bbx <- tmaptools::bb(bbx, projection = "longlat")
  }
  
  ### check z
  zm <- do.call(rbind, z)
  
  zn <- nrow(zm)
  
  z_res <- zm[,1]
  z_from <- zm[,2]
  z_to <- zm[,3]
  
  if (any(z_from > z_to)) stop("incorrect z: for each list item, the second value should be less than or equal to the third value")
  
  if (any(z_res > z_to))  stop("incorrect z: for each list item, the first value should be less than or equal to the third value")
  

  if (zn > 1) {
    for (i in zn - 1) {
      if (z_to[i] != (z_from[i+1] - 1L)) stop("second number of list item ", i + 1, " should be identical to the third number of list item ", i, " plus one")
    } 
  }


  z_min <- min(zm, z_arr)
  z_max <- max(zm)
  
  bbx2 <- tmaptools::bb(rasterInfo(z_min, bbx, pixels=tile_size)$bbx, current.projection="merc", projection = "longlat", ext=.99999999)
  
  ri <- rasterInfo(zoom=z_min:z_max, bbx2, pixels = tile_size)
  
  # approximate persons per km2
  
  #area_pix <- lapply(z_res, function(z_r) {
  z_r <- max(z_res)
  
  res <- ri[[paste0("z", z_r)]]
  bbx <- tmaptools::bb(res$bbx, current.projection = "merc", projection = crs)
  tile_size_m <- c(bbx[c(3,4)] - bbx[c(1,2)]) / c(res$nx, res$ny)
  tile_size_px <- c(res$px/res$nx, res$py/res$ny) 
  area_1pix <- prod(tile_size_m / tile_size_px)
  pix_1km2 <- prod(tile_size_px / tile_size_m * 1000)
  # list(area_1pix = area_1pix, pix_1km2 = pix_1km2, bbx = bbx)
  #})
  
  # area_1pix <- sapply(area_pix, "[[", 1)
  # pix_1km2 <- sapply(area_pix, "[[", 2)
  # bbx <- area_pix[[1]][[3]]

  if (setequal(names(settings), names(pop_tables))) {
    settings <- lapply(settings, function(s) do.call(dotmap_settings, s))[names(pop_tables)]
  } else {
    s <- do.call(dotmap_settings, settings)
    settings <- lapply(names(pop_tables), function(nm) s)
    names(settings) <- names(pop_tables)
  }
  
  list(bbx_orig=bbx,
       bbx=bbx2,
       n=n,
       m=m,
       k=k,
       dens_ub=dens_ub,
       dens_lb=dens_lb,
       z_from=z_from,
       z_to=z_to,
       z_res=z_res,
       z_arr=z_arr,
       ri=ri,
       tile_size=tile_size,
       area_1pix=area_1pix,
       pix_1km2=pix_1km2,
       transparent=transparent,
       lookup=lookup,
       file_shp_area1=file_shp_area1,
       file_shp_area2=file_shp_area2,
       file_shp_region=file_shp_region,
       dir_tiles_areas=dir_tiles_areas,
       dir_dotmap_data=dir_dotmap_data,
       dir_htmlserver=dir_htmlserver,
       pop_totals = pop_totals,
       pop_tables = pop_tables,
       settings = settings)
}


dotmap_settings <- function(L.delta, L.w, L.lim, H1, H.method="cat", sub.pops = c(TRUE, TRUE, TRUE), C.max=100, C.method="triangle", palette=NA) {
  if (!is.numeric(L.delta)) stop("delta should be a numeric")
  if (!is.numeric(L.w)) stop("w should be a numeric")
  if (!is.numeric(L.lim)) stop("L.lim should be a numeric")
  if (!is.numeric(H1)) stop("H1 should be a numeric")
  
  if (length(L.delta)!=1) stop("delta should be of length 1")
  if (length(L.w)!=1) stop("w should be of length 1")
  if (length(L.lim)!=2) stop("L.lim should be of length 2")
  if (length(H1)!=1) stop("H1 should be of length 1")
  
  if (L.delta < 0 || L.delta > 1) stop("delta should be between 0 and 1")
  if (L.w < 1) stop("w should be at least 1")
  if (L.lim[1] < 0 || L.lim[1] > 100 || L.lim[2] < 0 || L.lim[2] > 100 ) stop("both L.lim values should be between 0 and 100")
  if (H1 < 0 || H1 > 360) stop("H1 should be between 0 and 360")
  
  if (!is.logical(sub.pops)) stop("sub.pops is not a logical vector")
  
  res <- list(L.delta=L.delta, L.w=L.w, L.lim=L.lim, H1=H1, H.method=H.method, sub.pops=sub.pops, C.max=C.max, C.method=C.method, palette=palette)
  class(res) <- "dotmap_settings"
  res
}
# 
# dotmap_settings_perm <- function(settings) {
#   k <- length(settings$sub.pops)
#   p <- 2^k
#   tab <- e1071::bincombinations(k)[c(p, 2:(p-1)),]
#   
#   apply(tab, MARGIN = 1, FUN = function(r) {
#     rstring <- paste(r, collapse="")
#     settings$sub.pops <- as.logical(r)
#     #settings$name <- paste(settings$name, rstring, sep="_")
#     settings$dir_dotmap <- paste(settings$dir_dotmap, rstring, sep="_")
#     settings$dir_tile_server <- paste(settings$dir_tile_server, rstring, sep="_")
#     settings
#   })
#   
# }
# 
# dotmap_settings_vars <- function(settings, vars) {
#   nc <- nchar(settings$file_pop)
#   lapply(vars, function(v) {
#     settings$dir_dotmap <- paste(settings$dir_dotmap, v, sep="_")
#     settings$dir_tile_server <- paste(settings$dir_tile_server, v, sep="_")
#     settings
#   })
# }




