create_icons <- function(dm, px, dx) {
  px <- 16
  dx <- 4
  fact <- px/dx
  
  vars <- dm$vars
  
  k <- length(vars)
  m <- dm$m
  settings <- dm$settings
  
  get_pattern(2, 4, fact=4)
  
  radius <- ifelse(fact == 8, .9, ifelse(fact == 4, 1.1, 2))
  
  idir <- file.path(dm$dir_website, "index_files/icons")
  if (dir.exists(idir)) unlink(idir, recursive = TRUE, force = TRUE)
  if (!dir.exists(idir)) dir.create(idir)
  
  if (fact != 1) {
    #a3 <- a3[rep(1:tsize, each=osize/tsize), rep(1:tsize, each=osize/tsize), ]
    
    #mx <- dm$ri[[paste0("z", zoom)]]$nx / zr$nx
    circ <- circle_coverage(n=fact, radius=radius)
    
    patt <- stack_patterns(m=circ, nr=px, nc=px)
    if (!dm$transparent) patt <- array(rep(patt,3), dim=c(px, px, 3))
  } else {
    patt <- NULL
  } 
  
  
  cols <- lapply(1:k, function(i) {
    lapply(1:m[i], function(j) {
      a <- matrix(0, nrow = dx^2, ncol = m[i])
      a[, j] <- 1L
      a
      sett <- settings[[i]]
      sett$sub.pops <- NULL
      col <- do.call(get_HCL_colors, c(list(m=a, zf=0, transparent=dm$transparent, output = "rgb"), sett))
      a2 <- array(col, dim=c(dx, dx, ncol(col)))
      a3 <- a2[rep(1:dx, each = fact), rep(1:dx, each = fact), ]
      
      if (!is.null(patt)) {
        if (dm$transparent) {
          a3[,,4] <- a3[,,4] * patt
        } else {
          a3 <- a3 * patt + (1-patt)
        }
      }
      a3
      
      writePNG(a3, target=paste0(idir, "/icon_", vars[i], "_", j, ".png"))
      NULL
    }) 
  })
}







create_icons3 <- function(dm, px = 16, res = NA) {
  require(abind)
  
  
  idir <- file.path(dm$dir_website, "index_files/icons")
  if (dir.exists(idir)) unlink(idir, recursive = TRUE, force = TRUE)
  if (!dir.exists(idir)) dir.create(idir)
  
  
  
  zooms2 <- max(dm$z_res)  #(dm$z_res):dm$z_max
  
  zr <- max(dm$z_res)
  #zm <- dm$z_min
  m <- dm$m
  
  set.seed(20162312)
  
  n <- px^2

  mids <- matrix(1:n, ncol=sqrt(n))
  
  
  # L.delta <- setup$L.delta
  # H1 <- setup$L.delta
  # L.lim <- setup$L.lim
  # L.w <- setup$L.w
  
  vars <- dm$vars
  k <- length(vars)
  
  #########################################################################################
  # counts data for zres, zres+1 and zres+2, zres+3 (corresponding to zooms2)
  #########################################################################################
  
  ds <- 1
  
  counts <- lapply(1:k, function(i) {
    zf <- 2^(z-zr)
    n2 <- (px/zf)^2
    
    a <- matrix(0L, nrow=n2, ncol=m[i])
    
    lapply(1:m[i], function(j) {
      a[,j] <- 1
      a
    })    
  })
  
  
  # counts2 <- lapply(zooms2, function(z) {
  #   lapply(ds, function(d) {
  #     zf <- 2^(z-zr)
  #     n2 <- (px/zf)^2
  #     
  #     a <- matrix(0L, nrow=n2, ncol=m)
  #     
  #     lapply(1:m, function(j) {
  #         a[,j] <- d
  #         a
  #     })
  #   })
  # })
  # 
  # counts2 <- do.call(c, counts2)
  # 
  # zooms2ext <- rep(zooms2, each=length(ds))
  # dsext <- rep(ds, times=length(zooms2))
  # 
  # names(counts2) <- paste0("z", zooms2ext, "_d", dsext)
  


  #########################################################################################
  ## create meta info
  #########################################################################################

  

  #metas <- lapply(zooms2ext, function(zoom) {
    zfact <- 2^(max(1, (zooms2-dm$z_res+1))-1)
    
    osize <- px # size of sample tiles
    ts <- dm$tile_size # size of tile server tiles
    tsize <- osize / zfact # size of output size (to be enlarged for z>z_res)
    
    if (tsize != osize) {
      #a3 <- a3[rep(1:tsize, each=osize/tsize), rep(1:tsize, each=osize/tsize), ]
      
      #mx <- dm$ri[[paste0("z", zoom)]]$nx / zr$nx
      
      if (zfact==ts/32) {
        circ <- circle_coverage(n=zfact, radius=.9) #.75
      } else if (zfact==ts/64) {
        circ <- circle_coverage(n=zfact, radius=1.1)
      } else {
        circ <- circle_coverage(n=zfact, radius=2)
      }
      patt <- stack_patterns(m=circ, nr=osize, nc=osize)
      if (!dm$transparent) patt <- array(rep(patt,3), dim=c(osize, osize, 3))
    } else {
      patt <- NULL
    } 
    meta <- list(z=zooms2,
         zres = zr,
         transparent = dm$transparent,
         tsize = tsize,
         osize = osize,
         zfact = zfact, 
         ts = ts,
         patt = patt)
  #})
  
  
  #########################################################################################
  ## create icons for digital zoom legend
  #########################################################################################
  
  mapply(function(count, var, mi, setup) {
    mapply(function(j) {
      #print(names)
      #if (names == "z11_d2") browser()
      z <- min(meta$z, meta$zres)
      setup$sub.pops <- NULL
      cols <- do.call(get_HCL_colors, c(list(m=count[[j]], zf=meta$zres-z, transparent=meta$transparent, output = "rgb"), setup))
      a <- array(cols, dim=c(meta$tsize, meta$tsize, ncol(cols)))
      if (meta$tsize != meta$osize) {
        a <- a[rep(1:meta$tsize, each=meta$osize/meta$tsize), rep(1:meta$tsize, each=meta$osize/meta$tsize), ]
        
        #mx <- dm$ri[[paste0("z", zoom)]]$nx / zr$nx
        
        if (!is.null(meta$patt)) {
          if (meta$transparent) {
            a[,,4] <- a[,,4] * (meta$patt)
          } else {
            a <- a * meta$patt + (1-meta$patt)
          }
        }
      }
      
      writePNG(a, target=paste0(idir, "/icon_", var, "_", j, ".png"))
    }, 1:mi, SIMPLIFY = FALSE)  
  }, counts, vars, m, dm$settings)
      
    
    
  mapply(function(counts, meta, names) {
    mapply(function(j) {
      #print(names)
      #if (names == "z11_d2") browser()
      z <- min(meta$z, meta$zres)
      cols <- do.call(get_HCL_colors, c(list(m=counts[[j]], zf=meta$zres-z, transparent=meta$transparent, output = "rgb"), setup))
      a <- array(cols, dim=c(meta$tsize, meta$tsize, ncol(cols)))
      if (meta$tsize != meta$osize) {
        a <- a[rep(1:meta$tsize, each=meta$osize/meta$tsize), rep(1:meta$tsize, each=meta$osize/meta$tsize), ]
        
        #mx <- dm$ri[[paste0("z", zoom)]]$nx / zr$nx
        
        if (!is.null(meta$patt)) {
          if (meta$transparent) {
            a[,,4] <- a[,,4] * meta$patt
          } else {
            a <- a * meta$patt + (1-meta$patt)
          }
        }
      }
      
      writePNG(a, target=paste0(path, "/icon_", names, "_", j, ".png"))
    }, 1:m, SIMPLIFY = FALSE)
  }, counts2, metas, names(counts2), SIMPLIFY=FALSE)
  

  invisible()
}
