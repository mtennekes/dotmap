check_shape <- function(dir,
                        shp,
                        required) {
  
  object <- deparse(substitute(shp))
  f <- file.path(dir, "input", paste(object, "rds", sep = "."))
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
#' This function creates a dotmap project.  
#' 
#' @param dir The project directory. It has/will have the following subdirectories: source (raw input files), input (preprocessed input files), tiles_area (processed data regarding the canvas available for the dots), dotmap_data (processed data regarding the dotmap), htmlserver (the output tile server), website (the dotmap website).
#' @param area1 A spatial polygons object (class \code{sfc} or \code{sf}) that defines main the canvas for the dots. For instance, the OSM land use category residential.
#' @param area2 (optional) A spatial polygons object (class \code{sfc} or \code{sf}) that defines second-stage canvas for the dots. Only used if not all dots can be placed on the main canvas (defined by \code{area1}).
#' @param region A spatial polygons object (class \code{sfc} or \code{sf}) that defines the regions of the dots. The data (see \code{pop_totals} and \code{pop_tables}) should be provided per region. Note that \code{region} can be specified by the same spatial object as \code{area1}, but not necessarily. For instance, \code{region} could be the municiaplity borders, while \code{area1} are residential areas. 
#' @param vars Character vector that specifies the variables. These names are used within the process, so please do no use whitespaces.
#' @param var_titles Character vector that specifies the variable titles (shown in the visualization).
#' @param var_labels List of category labels per variable. So each list item corresponds to a variable and should be a vector of category labels.
#' @param pop_totals Either a number vector that contains the population sizes per region, or a data.frame with a column \code{pop} (with population numbers) and optionally a column \code{class} which defines the dot density class (see \code{dens_ub} and \code{dens_lb}) 
#' @param pop_tables pop_tables For each variable, a data.frame where the rows correspond to the regions and the columns to the categories.  
#' @param dens_ub Upper bound for the dot densities per class (see also \code{pop_totals}). If the number of dots exceeds this number, the remaining dots will be placed in the \code{area2} region if defined. Otherwise, they are ignored.
#' @param dens_lb dens_lb Lower bound for the dot densities per class. If the number of dots is lower than this number, they are not drawn.
#' @param bbx Bounding box. By default, the bounding box of \code{region} is taken.
#' @param z List of zoom levels at which the dots are distributed and drawn. Each list item corresponds a distribution level. If only one distribution level is specified, the dots are distributed at a specific zoom level, where for other zoom levels, they are blended to darker pixel colors (zooming out) or enlarged (zooming in). If multiple zoom levels are specified, the dots are distributed at the highest zoom level, and after that aggregated to super dots for the other distribution levels. For each list item, a numeric vector of three should be specified: the zoom level at which the dots are distributed, the lowest zoom level at which this distribution is rendered, the highest zoom level at which this distribution is rendered. Note that the distribution zoom level does not have to be the same as one of the other two zoom levels. However, across all distribution levels, all zoom levels have to be present. The order of list items should be increasing, so the first vector should specify the distribution level of the lowest zoom level.
#' @param z_arr The zoom level at which the tiles are first created. If set to the highest distribution zoom level (see \code{z}), the tiles will be as large as \code{tile_size}. By default, they are two zoom levels lower (so the tiles are 4x4 as large).
#' @param scale Vector of numbers that specify how many small dots a super dot represents. Only applicable if \code{z} contains more than 1 list items. If so, it should be the same length of \code{z}, where the last value should be 1 (since it represents the small dots). By default, it is 4 for one zoom level difference, 16 for two zoom levels difference, etc.
#' @param tile_size  Tile size. By default 256
#' @param transparent Should the tiles be transparent? By default \code{TRUE}
#' @param settings A list, with per variable, a list of color settings.
#' @param title Title of the dotmap
#' @param region_title Title of the region boundary layer.
#' @param labels_title Title of the map labels layer.
#' @param dots_text dots_text Text that explains the dots in the legend (e.g. \code{"one dot represents one person"}). When multiple distribution levels are used, it should be a vector.
#' @param dotmap_attr dotmap_attr The map attribute for the dotmap layer.
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
                           vars,
                           var_titles = NULL,
                           var_labels = NULL,
                           pop_totals = NULL,
                           pop_tables,
                           dens_ub=NULL,
                           dens_lb=NULL,
                           bbx=NA,
                           z,
                           z_arr = NA,
                           scale = NA,
                           tile_size=256,
                           transparent=TRUE,
                           settings,
                           title = "Dotmap",
                           region_title = "Borders",
                           labels_title = "Labels",
                           dots_text = "",
                           dotmap_attr = "") {
  
  
  dir.create(file.path(dir, "input"), recursive = TRUE, showWarnings = FALSE)
  
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
  
  
  
  
  ### check pop_totals
  if (is.null(pop_totals)) pop_totals <- unname(rowSums(pop_tables[[1]]))
  if (is.vector(pop_totals)) pop_totals <- data.frame(pop = pop_totals)
  if (!inherits(pop_totals, "data.frame")) stop("pop_totals should be a vector or a data.frame")
  if (!all(names(pop_totals) %in% c("pop", "class")) || !("pop" %in% names(pop_totals))) stop("pop_totals should contain the columns \"pop\" (required) and \"class\" (optional).")
  if (any(is.na(pop_totals))) stop("pop_totals contains NAs")
  
  ### check region
  region <- readRDS(file_shp_region)
  if (!inherits(region, "sfc")) stop(file_shp_region, " is not an sfc object")
  n <- nrow(pop_totals)
  if (n != length(region)) stop("number of rows in pop_table (", n, ") does not correspond with number of regions in the region shape (", length(region),")")
  
  
  ### check var names
  if (length(pop_tables) != length(vars)) stop("Lengths of vars and pop_tables do not match")
  if (!is.null(names(pop_tables))) {
    if (setequal(names(pop_tables), vars)) {
      pop_tables <- pop_tables[vars]
    } else stop("names pop_tables do not correspond to vars")
  } else {
    names(pop_tables) <- vars
  }
  
  ### check pop_tables
  m <- mapply(function(tab, name) {
    if (nrow(tab) != n) stop("number of rows in table ", name, "(", nrow(tab), ") does not correspond to the number of regions (", n, ")")
    if (any(is.na(tab))) stop("table ", name, " contains NAs")
    ncol(tab)
  }, pop_tables, names(pop_tables))
  
  ### check titles and labels
  if (is.null(var_titles)) {
    var_titles <- vars
  } else {
    if (length(var_titles) != length(vars)) stop("var_titles does not have the same length as vars")
  }
  
  if (is.null(var_labels)) {
    var_labels <- lapply(pop_tables, names)
  } else {
    if (!is.list(var_labels)) stop("var_labels should be a list")
    if (length(var_labels) != length(vars)) stop("var_labels does not have the same length as vars")
    mapply(function(a, b) {
      if (length(a) != length(b)) stop("some var_labels have incorrect length: ", paste(a, collapse = ", "), " ---- ", paste(b, collapse = ", "))
    }, var_labels, pop_tables)
  }
  
  
  
  #file_pop <- file.path(project, "source/pop_data.rdata")
  dir_tiles_areas <- file.path(dir, "tiles_area")
  dir_dotmap_data <- file.path(dir, "dotmap_data")
  
  
  dir_website <- file.path(dir, "website")
  
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
  
  z_r <- max(z_res)
  
  if (is.na(z_arr)) {
    z_arr <- max(min(zm), z_r - 2)
    #message("z_arr set to ", z_arr)
  }

  z_min <- min(zm, z_arr)
  z_max <- max(zm)
  
  if (is.na(scale)) {
    scale <- 4^(z_r - z_res)
  } else {
    if (length(scale) != length(z_res)) stop("scale length should be ", length(z_res))
    if (scale[length(z_res)] != 1) stop("last scale number should be 1")
  }
  
  bbx2 <- tmaptools::bb(rasterInfo(z_min, bbx, pixels=tile_size)$bbx, current.projection="merc", projection = "longlat", ext=.99999999)
  
  ri <- rasterInfo(zoom=z_min:z_max, bbx2, pixels = tile_size)
  
  dots_text <- rep(dots_text, length.out=zn)
  
  # approximate persons per km2
  
  #area_pix <- lapply(z_res, function(z_r) {
  
  res <- ri[[paste0("z", z_r)]]
  
  dist_res <- tmaptools::approx_distances(res$bbx, projection = "merc")

  get_hv_dist <- function(bbx, projection) {
    #derived from tmaptools::approx_distances
    pW <- st_transform(st_sfc(st_point(c(bbx[1], (bbx[2] + bbx[4])/2)), crs = projection), crs = 4326)
    pE <- st_transform(st_sfc(st_point(c(bbx[3], (bbx[2] + bbx[4])/2)), crs = projection), crs = 4326)
    pS <- st_transform(st_sfc(st_point(c((bbx[1] + bbx[3])/2, bbx[2])), crs = projection), crs = 4326)
    pN <- st_transform(st_sfc(st_point(c((bbx[1] + bbx[3])/2, bbx[4])), crs = projection), crs = 4326)
    as.numeric(c(lwgeom::st_geod_distance(pW, pE), lwgeom::st_geod_distance(pN, pS)))
  }
  tile_size_m <- get_hv_dist(res$bbx, 3857) / c(res$nx, res$ny)
  tile_size_px <- c(res$px/res$nx, res$py/res$ny) 
  area_1pix <- prod(tile_size_m / tile_size_px)
  pix_1km2 <- prod(tile_size_px / tile_size_m * 1000)
  # list(area_1pix = area_1pix, pix_1km2 = pix_1km2, bbx = bbx)
  #})
  
  # area_1pix <- sapply(area_pix, "[[", 1)
  # pix_1km2 <- sapply(area_pix, "[[", 2)
  # bbx <- area_pix[[1]][[3]]
  
  if (setequal(names(settings), vars)) {
    settings <- lapply(settings, function(s) do.call(dotmap_settings, s))[vars]
  } else {
    s <- do.call(dotmap_settings, settings)
    settings <- lapply(vars, function(nm) s)
    names(settings) <- vars
  }
  
  list(bbx=bbx2,
       bbx_shp = bbx,
       n=n,
       m=m,
       k=k,
       dens_ub=dens_ub,
       dens_lb=dens_lb,
       z_from=z_from,
       z_to=z_to,
       z_res=z_res,
       z_arr=z_arr,
       scale=scale,
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
       dir_website=dir_website,
       dir_htmlserver=dir_htmlserver,
       vars = vars,
       var_titles = var_titles,
       var_labels = var_labels,
       pop_totals = pop_totals,
       pop_tables = pop_tables,
       settings = settings,
       title = title,
       region_title = region_title,
       labels_title = labels_title,
       dots_text = dots_text,
       dotmap_attr = dotmap_attr)
}


dotmap_settings <- function(L.delta, L.w, L.lim, H1 = 0, H.method="cat", sub.pops = c(TRUE, TRUE, TRUE), C.max=100, C.method="triangle", palette=NA) {
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




