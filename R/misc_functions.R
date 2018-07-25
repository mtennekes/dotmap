rasterInfo <- function(zoom, bbx, pixels=256) {
  res <- lapply(zoom, function(z) {
    df <- num2bbx(zoom=z, bbx)
    
    xmin <- min(df$x)
    xmax <- max(df$x) + 1
    ymin <- min(df$y)
    ymax <- max(df$y) + 1
    
    bbx_merc <- tmaptools::bb(xlim = range(c(df$lon1, df$lon2)),
                   ylim = range(c(df$lat1, df$lat2)),
                   current.projection = "longlat",
                   projection="merc")
    nx <- (xmax-xmin)
    ny <- (ymax-ymin)
    px <- nx * pixels
    py <- ny * pixels
    list(zoom=z, bbx=bbx_merc, nx=nx,ny=ny, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, px=px, py=py)
  })
  names(res) <- paste0("z", zoom)
  if (length(res)==1) res[[1]] else res
}


num2bbx <- function(zooms, bbx) {
  res <- lapply(zooms, function(zoom) {
    c1 <- deg2num(bbx[1], bbx[2], zoom=zoom)
    c2 <- deg2num(bbx[3], bbx[4], zoom=zoom)
    s1 <- sort(c1[1]:c2[1])
    s2 <- sort(c1[2]:c2[2])
    data.frame(zoom=zoom, x=rep(s1, each=length(s2)),
               y=rep(s2, times=length(s1)))
  })
  df <- do.call(rbind, res)

  df$y <- df$y + 1
  
  df$x2 <- df$x + 1
  df$y2 <- df$y - 1
  
  co1 <- t(mapply(num2ll, zoom=df$zoom, x=df$x, y=df$y))
  colnames(co1) <- c("lon1", "lat1")
  
  co2 <- t(mapply(num2ll, zoom=df$zoom, x=df$x2, y=df$y2))
  colnames(co2) <- c("lon2", "lat2")
  
  df <- cbind(df, co1, co2)
  df$x2 <- df$y2 <- NULL

  df
}

# from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
num2ll <- function(zoom, x, y) {
  n = 2.0 ^ zoom
  lon_deg <- x / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * y / n)))
  lat_deg = lat_rad * 180.0 / pi
  c(lon_deg, lat_deg)
}
deg2num<-function(lon_deg, lat_deg, zoom){
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( c(xtile, ytile))
  #  return(paste(paste("http://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

write2tiles <- function(ri, m=NA, filename=NA) {
  require(png)
  
  if (is.na(filename)) {
    tmpfile <- "total.png"
    tmap::save_tmap(m, width = ri$px, height = ri$py, units = "px", filename = tmpfile)
  } else {
    tmpfile <- paste0(filename, ri$zoom, ".png")
    if (!file.exists(tmpfile)) stop(tmpfile, " does not exist")
  }
  
  r <- readPNG(tmpfile)
  z <- ri$zoom
  lapply(0:(ri$nx-1), function(x) {
    xrange <- (x * 256 + 1):((x+1) *256)
    tx <- ri$xmin + x
    lapply(0:(ri$ny-1), function(y) {
      yrange <- (y * 256 + 1) :((y+1)*256)
      ty <- ri$ymin + y - 1
      dir <- file.path("tiles", z, tx)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      r2 <- r[yrange, xrange, ]
      writePNG(r2, file.path(dir, paste(ty, "png", sep=".")))
    })
  })
  invisible()
}


get_lookup <- function(n) {
  set.seed(123456)
  maxhex <- 16^6 - 1
  sample(maxhex, n)
}


num2hexcol <- function(x, lookup) {
  y <- lookup[x]
  paste0("#", toupper(format(as.hexmode(y), width=6)))
}

rgb2num <- function(r,g,b, lookup) {
  cols <- substr(rgb(r,g,b), 2, 7)
  match(as.integer(as.hexmode(cols)), lookup)
}

