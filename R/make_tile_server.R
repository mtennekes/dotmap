#' Make tile server
#' 
#' Make tile server from dotmap tiles that were created with plot_dotmap. The tiles are split to \code{dm$tile_size} by \code{dm$tile_size} bitmap images, and arranged according to the standard convention implemented in the Google Maps API.
#' @param dm dotmap_meta object
#' @param logfile logfile
#' @export
#' @import png
#' @export
#' @seealso \url{http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/}
make_tile_server <- function(dm, logfile=NULL) {
  nvars <- length(dm$m)
  nagg <- length(dm$z_res)
  
  for (k in 1:nvars) {
    dmk <- dm
    dmk$m <- dmk$m[k]
    
    dmk$pop_table_name <- dmk$vars[k]
    
    dmk$dir <- file.path(dmk$dir_htmlserver, dmk$pop_table_name) #dmk$resname, 
    unlink(dmk$dir, recursive = TRUE, force = TRUE)      
    
    for (a in 1:nagg) {
      dmk$z_from <- dm$z_from[a]
      dmk$z_to <- dm$z_to[a]
      dmk$z_res <- dm$z_res[a]
      dmk$resname <- paste0("res", dmk$z_res)
      make_tile_server_i(dmk, logfile=logfile)
    }
    
  }
  
}

make_tile_server_i <- function(dm, logfile=NULL) {
  
  zmin <- dm$z_from
  zmax <- dm$z_to
  
  zall <- zmin:zmax
  
  message("Make tile server files")
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  ri_res <- dm$ri[[paste0("z", dm$z_res)]]
  
  
  zcomb <- if (zmin < dm$z_arr) intersect((dm$z_arr-1):zmin, zall) else NULL
  zsplt <- intersect(max(dm$z_res, dm$z_arr):dm$z_arr, zall)
  zxtra <- if (zmax > max(dm$z_res, dm$z_arr)) intersect((max(dm$z_res, dm$z_arr)+1):zmax, zall) else NULL


  cat("zcomb ", paste(zcomb, collage=","), "\n")
  cat("zsplt ", paste(zsplt, collage=","), "\n")
  cat("zxtra ", paste(zxtra, collage=","), "\n")

  src <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name)
  dir <- dm$dir
  
  ts <- dm$tile_size
  transparent <- dm$transparent
  

  for (z in zsplt) {
    message("Process zoom level ", z)
    
    dir.create(file.path(dir, z), recursive = TRUE, showWarnings = FALSE)
    
    rz <- dm$ri[[paste0("z", z)]]
    
    nx <- rz$nx / ri_arr$nx
    ny <- rz$ny / ri_arr$ny
    lapply(rz$xmin:rz$xmax, function(x) dir.create(file.path(dir, z, x), recursive = TRUE, showWarnings = FALSE))
    
    if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)
    for (i in 1:ri_arr$nx) {
    #foreach(i=1:ri_arr$nx) %dopar% { 
      #devtools::load_all(pkg)
      if (!is.null(logfile)) {
        #logfile <- paste0(substr(logfile, 1, nchar(logfile)-4), Sys.getpid(), ".txt")
        f <- openLog(logfile)
      }
      
      for (j in 1:ri_arr$ny) {
        message(z, " ", i, " ", j)
        filename <- file.path(src, z, paste0("/dotmap_", i, "_", j, ".png"))
        
        if (file.exists(filename)) {
          r <- png::readPNG(filename)
          lapply(0:(nx-1), function(x) { #if (!all(r==1)) 
            xrange <- (x * ts + 1):((x+1) *ts)
            tx <- rz$xmin + x + (i-1) * nx
            dir2 <- file.path(dir, z, tx)
            lapply(0:(ny-1), function(y) {
              yrange <- (y * ts + 1) :((y+1)*ts)
              ty <- rz$ymin + y - 1  + (j-1) * ny
              r2 <- r[yrange, xrange, ]
              
              if ((transparent && !all(r2[,,4]==0)) || (!transparent && !all(r2==1))) {
                png::writePNG(r2, file.path(dir2, paste(ty, "png", sep=".")))
              } else NULL
            })
          })
        }
      }
      if (!is.null(logfile)) closeLog(f)
    }
    # remove empty directories
    lapply(rz$xmin:rz$xmax, function(x) {
      p <- file.path(dir, z, x)
      if (length(list.files(p))==0) unlink(p, recursive = TRUE, force = TRUE)
    })
  }
  
  if (!is.null(zcomb)) for (z in zcomb) {
    message("Process zoom level ", z)
    
    dir.create(file.path(dir, z), recursive = TRUE, showWarnings = FALSE)
    rz <- dm$ri[[paste0("z", z)]]
    
    zdiff <- dm$z_arr - z 
    
    # number of tiles to combine
    nx <- ri_arr$nx / rz$nx
    ny <- ri_arr$ny / rz$ny
    
    rx <- ts / nx
    ry <- ts / ny
    
    for (i in 1:rz$nx) {
      tx <- rz$xmin + (i-1)
      dir2 <- file.path(dir, z, tx)
      dir.create(dir2, recursive = TRUE, showWarnings = FALSE)
      
      for (j in 1:rz$ny) {
        message(z, " ", i, " ", j)
        
        ii <- (i * nx - (nx-1)):(i * nx)
        jj <- (j * ny - (ny-1)):(j * ny)
        
        filenames <- unlist(lapply(ii, function(iii) {
          lapply(jj, function(jjj) {
            file.path(src, z, paste0("/dotmap_", iii, "_", jjj, ".png"))
          })
        }))
        
        
        if (any(file.exists(filenames))) {
          rs <- lapply(filenames, function(f) {
            if (file.exists(f)) {
              png::readPNG(f)
            } else if (transparent) {
              a <- array(1, dim=c(rx, ry, 4))
              a[,,4] <- 0
              a
            } else {
              array(1, dim=c(rx, ry,3))
            }
          })
          
          r2 <- if (transparent) {
            a <- array(1, dim=c(ts,ts, 4))
            a[,,4] <- 0
            a
          } else {
            array(1, dim=c(ts,ts, 3))
          }

          tel <- 0
          for (jjj in 1:ny) {
            for (iii in 1:nx) {
              tel <- tel + 1
              r2[(iii*rx - (rx-1)):(iii*rx),
                 (jjj*ry - (ry-1)):(jjj*ry), ] <- rs[[tel]]
            }
          }
          
          ty <- rz$ymin + (j-2)
          png::writePNG(r2, file.path(dir2, paste(ty, "png", sep=".")))
        }
      }
    }
  }
  
  
  
  
  
  
  
  if (!is.null(zxtra)) for (z in zxtra) {
    message("Process zoom level ", z)
    
    dir.create(file.path(dir, z), recursive = TRUE, showWarnings = FALSE)
    rz <- dm$ri[[paste0("z", z)]]
    
    
    # if (z < dm$z_arr) {
    #   
    # }
    
    
    nx <- rz$nx / ri_arr$nx
    ny <- rz$ny / ri_arr$ny

    mx <- rz$nx / ri_res$nx
    my <- rz$ny / ri_res$ny
    
    par_x <- as.integer(list.files(file.path(dir, z-1)))
    xs <- sort(c(par_x*2, par_x*2+1L))
    
    ys <- lapply(xs, function(x) {
      nm <- list.files(file.path(dir, z-1, floor(x/2)))
      nms <- as.integer(substr(nm, 1, nchar(nm)-4))
      sort(c(nms*2, nms*2+1L))
    })
    names(ys) <- xs
    
    lapply(xs, function(x) dir.create(file.path(dir, z, x), recursive = TRUE, showWarnings = FALSE))
    
    rx <- ts/mx
    ry <- ts/my
    
    #size <- 2^(z-dm$z_res)
    # if (mx==2) {
    #   cat("circle2\n")
    #   circ <- matrix(1, ncol=2,nrow=2)
    # } else if (mx==4) {
    #   cat("circle4\n")
    #   circ <- circle_coverage(n=mx, radius=1.25)
    # } else {
    #   cat("circle8\n")
    #   circ <- circle_coverage(n=mx, radius=.75)
    # }

    
    patt <- get_pattern(mx, ts)
    
    
    if (!transparent) patt <- array(rep(patt,3), dim=c(ts, ts, 3))

    
    
    #browser()
    #foreach(i=1:ri_arr$nx) %dopar% { 
    for (i in 1:ri_arr$nx) {
      #devtools::load_all(pkg)
      if (!is.null(logfile)) {
        #logfile <- paste0(substr(logfile, 1, nchar(logfile)-4), Sys.getpid(), ".txt")
        f <- openLog(logfile)
      }
      for (j in 1:ri_arr$ny) {
        message(z, " ", i, " ", j)
        filename <- file.path(src, dm$z_res, paste0("/dotmap_", i, "_", j, ".png"))
        
        if (file.exists(filename)) {
          r <- png::readPNG(filename)

          # if (!all(r==1)) lapply(0:(nx-1), function(x) {
          #   xrange <- (x * rx + 1):((x+1) * rx)
          #   tx <- rz$xmin + x + (i-1) * nx
          #   dir2 <- file.path(dir, z, tx)
          #   lapply(0:(ny-1), function(y) {
          #     yrange <- (y * ry + 1) :((y+1)*ry)
          #     ty <- rz$ymin + y - 1  + (j-1) * ny
          #     r2 <- r[rep(yrange, each=my), rep(xrange, each=mx), ]
          #   })          
          # })
          
          xsi <- intersect(xs, (rz$xmin + 0 + (i-1) * nx):(rz$xmin + (nx-1) + (i-1) * nx))
          
          #browser()
          
          lapply(xsi, function(tx) { #if (!all(r==1)) 
            x <- -rz$xmin + tx - (i-1) * nx
            xrange <- (x * rx + 1):((x+1) * rx)
            #tx <- rz$xmin + x + (i-1) * nx
            dir2 <- file.path(dir, z, tx)
            
            ysi <- intersect(ys[[as.character(tx)]], (rz$ymin + 0 - 1  + (j-1) * ny):(rz$ymin + (ny - 1) - 1  + (j-1) * ny))

            lapply(ysi, function(ty) {
              y <- ty - rz$ymin +1 - (j-1) * ny
              #ty <- rz$ymin + y - 1  + (j-1) * ny
              yrange <- (y * ry + 1) :((y+1)*ry)
              r2 <- r[rep(yrange, each=my), rep(xrange, each=mx), ]
              if ((transparent && !all(r2[,,4]==0)) || (!transparent && !all(r2==1))) {
                if (transparent) {
                  r3 <- r2
                  r3[,,4] <- r3[,,4] * patt
                } else {
                  r3 <- r2 * patt + (1-patt)
                }
                
                #r3 <- alias_dots(r2, size=)
                png::writePNG(r3, file.path(dir2, paste(ty, "png", sep=".")))
              } else NULL
            })
          })
          
        }
      }
      if (!is.null(logfile)) closeLog(f)
    }
    # remove empty directories
      lapply(xs, function(x) {
        p <- file.path(dir, z, x)
        if (length(list.files(p))==0) unlink(p, recursive = TRUE, force = TRUE)
      })
  }
  
}

# 
# ir=1
# ic=1
# nr=2
# nc=2
# Nr=40
# Nc=40
# Nz=3

# matrix of Nr by Nc, pattern nr by nc, get indices of [r,c] in a vector
get_matrix_indices <- function(i, j, nr, nc, Nr, Nc, Nz) {
  mc <- Nc/nc
  mr <- Nr/nr
  
  stopifnot(length(i)==length(j))
  
  as.vector(mapply(function(ic,ir) {
    cols <- seq(ic, Nc, by=nc)
    rows <- seq(ir, Nr, by=nr)
    
    ids <- rep(rows, times=length(cols)) + (rep(cols, each=length(rows))-1) * Nr
    rep(ids, Nz) + Nr*Nc * rep(0:(Nz-1), each=length(ids))
  }, i,j))
}

brighter <- function(x, weight) {
  weight + (1-weight) * x
}

alias_dots <- function(m, size) {
  if (size==2) {
    ids1 <- get_matrix_indices(c(1,2),
                               c(2,1),
                               2,2,nrow(m), ncol(m), 3)
    ids2 <- get_matrix_indices(2,2,2,2,nrow(m), ncol(m), 3)
    
    m[ids1] <- brighter(m[ids1], .37)
    m[ids2] <- 1 #brighter(m[ids2], .5)
    
    
    # m[seq(2,nrow(m), by=2), ,] <- 1
    # m[, seq(2,ncol(m), by=2),] <- 1
  } else if (size==4) {
    ids1 <- get_matrix_indices(c(1,1,2,2,3,3,4,4), 
                               c(2,3,1,4,1,4,2,3),
                               4,4,nrow(m), ncol(m), 3)
    ids2 <- get_matrix_indices(c(1,1,4,4), 
                               c(1,4,1,4),
                               4,4,nrow(m), ncol(m), 3)

    m[ids1] <- brighter(m[ids1], .64)
    m[ids2] <- 1 #brighter(m[ids2], 1)
    
    # m[c(seq(1,nrow(m), by=4),
    #     seq(4,nrow(m), by=4)),
    #   c(seq(1,ncol(m), by=4),
    #     seq(4,ncol(m), by=4)),] <- 1
  }
  m
}

get_pattern <- function(mx, ts, fact=1) {
  if (mx==ts/32) {
    circ <- circle_coverage(n=mx, radius=.9) #.75
  } else if (mx==ts/64) {
    circ <- circle_coverage(n=mx, radius=1.1)
  } else {
    circ <- circle_coverage(n=mx, radius=2)
  }
  stack_patterns(m=circ, nr=ts * fact, nc=ts * fact)
}
