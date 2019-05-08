#' Get HCL colors based on triples.
#' 
#' Derive colors from the HCL color space based on triples of values. The H and C are 
#' 
#' @param m matrix of counts, rows are the cases, columns are the groups. Number of columns should be at most three.
#' @param H1 hue value of first subpopulation
#' @param L.lim limits of the luminance, corresponds to pop.lim
#' @param L.delta L.delta
#' @param L.w L.w
#' @param zf zf
#' @param C.max maximum chroma
#' @param transparent should the background be transparent? If \code{FALSE}, it will be white.
#' @param H.method H.method
#' @param H.div H.div
#' @param L.method L.method
#' @param C.method C.method
#' @param palette palette
#' @param output a subset of: \code{"colors"}, which outputs the color codes, \code{"hcl"}, which outputs the H, C and L parameters, and \code{"rgb"}, which outputs the red, green and blue values.
#' @return a \code{vector} if \code{output == "colors"}, \code{matrix} if \code{output == "hcl"} or \code{output == "rgb"}, and a \code{data.frame} otherwise.
get_HCL_colors <- function(m, H1 = 0, L.lim=c(80,20), L.delta=.65, L.w=10, zf=0, C.max=100, transparent=FALSE, H.method=c("cat", "div", "seq"), H.div=c(240, 20), L.method=c("v1", "v2"), C.method=c("triangle", "entropy"), palette=NA, output=c("colors", "hcl", "rgb")) {
  C.method <- match.arg(C.method)
  L.method <- match.arg(L.method)
  H.method <- match.arg(H.method)
  
  nrowx <- nrow(m)
  ncolx <- ncol(m)
  
  if (ncolx > 3 && (H.method == "cat" && is.na(palette[1]))) stop("For the categorical coloring method, the number of columns should be at most three, or a palette should be defined")
  
  if (!is.matrix(m)) {
    if (is.data.frame(m)) {
      m <- as.matrix(m)
    } else stop("x should be a matrix")
  }
  
  
  sel <- which(rowSums(m)>0)
  m <- m[sel,,drop=FALSE]
  
  # normalize x
  rs <- rowSums(m)
  n <- m / rs # matrix of probabilities
  n[is.nan(n)] <- NA
  
  if (!is.na(palette[1])) {
    if (length(palette) != ncolx) stop("Number of colors inconsistent with number of categories")
    maxn <- apply(m, MARGIN = 1, function(mi) {
      ids <- which(mi==max(mi))
      if (length(ids)==1) ids else sample(ids, 1)
    })
    cols <- palette[maxn]
  } else {
    if (H.method == "cat") { #difference???
      if (ncolx==3) {
        # calculate cartesian coordinates
        y <- (n[,3] * sqrt(3) / 2) - (sqrt(3) / 6)
        x <- (n[,1] + n[,3] * .5) - .5
        
        H <- atan2(x,y)/pi*180 - 120 + H1
        H[which(H < 0)] <- H[which(H < 0)] + 360
        H[is.na(H)] <- 0
        C <- sqrt(x^2 + y^2) * (3 / sqrt(3)) * C.max
        C[is.na(C)] <- 0
      } else if (ncolx==2) {
        H <- ifelse(n[,1] >= n[,2], H1, H1+180)
        #C <- (2 - (pmax(n[,1], n[,2]) * 2)) * C.max
        mxs <- pmax(n[,1], n[,2])
        mns <- pmin(n[,1], n[,2])
        
        C <- ((mxs - mns) / mxs) * C.max
        
      } else {
        H <- H1
        C <- C.max
      }
    } else {
      if (H.method=="div") {
        n2 <- rowSums(n * matrix(1:ncol(n), ncol=ncol(n), nrow=nrow(n), byrow = TRUE))
        midcol <- (ncolx +1)/2
        # Hids <- apply(n, MARGIN = 1, function(v) {
        #   mids <- which(v==max(v))
        #   if (length(mids)==1) mids else sample(mids, 1)
        # })
        H <- H.div[ifelse(n2 < midcol, 1, 2)]
        C <- abs(n2 - midcol) / (midcol-1) * C.max
      } else {
        stop("H.method 'seq' other not implemented")
      }
      C.method <- "div"
    }
    
    
    
    if (C.method=="entropy") {
      C <- C.max * (1-(apply(m, MARGIN = 1, entropy::entropy) / log(ncolx)))
    }
    
    
    if (L.method=="v1") {
      #pop.lim <- c(1, L.w*(4)^(L.delta*zf))
      pop.lim <- c(0, L.w*(4)^(L.delta*zf))
      
      Lext <- L.lim[2] - L.lim[1]
      
      a <- pmin(rs, pop.lim[2])
      L <- L.lim[1] + (a-pop.lim[1]) * (Lext / (pop.lim[2]-pop.lim[1]))
    } else {
      L.dots <- 70
      zf.max <- 7
      a <- pmin(rs, 4^zf)
      L <- L.dots - zf * ((L.dots-L.lim[2])/zf.max) + (1-(a/(4^zf))) * zf * ((L.lim[1] - L.lim[2]) / zf.max)
    }
  }
  
  
  #browser()
  
  ## manage output
  if (any(c("colors", "rgb") %in% output)) {
    if ((is.na(palette[1]))) {
      if (transparent) {
        cols <-  hcl(h=H, c=C, l=L, alpha=1)
      } else {
        cols <-  hcl(h=H, c=C, l=L)
      }
    }
    if ("rgb" %in% output) {
      if (transparent) {
        yr <- matrix(c(rep(1,nrowx * 3), rep(0, nrowx)), nrow = nrowx, ncol = 4)
        yr[sel, ] <- t(col2rgb(cols, alpha=TRUE) / 255)
        colnames(yr) <- c("r", "g", "b", "a")
      } else {
        yr <- matrix(1, nrow = nrowx, ncol = 3)
        yr[sel, ] <- t(col2rgb(cols) / 255)
        colnames(yr) <- c("r", "g", "b")
      }
    } else yr <- NULL
    if ("colors" %in% output) {
      yc <- rep(ifelse(transparent, "#FFFFFF00", "#FFFFFF"), nrowx)
      yc[sel] <- cols
    } else yc <- NULL
  } else {
    yr <- NULL
    yc <- NULL
  }
  
  if ("hcl" %in% output && (is.na(palette[1]))) {
    yh <- matrix(NA, nrow = nrowx, ncol = 3)
    yh[sel, ] <- c(H, C, L)
    colnames(yh) <- c("h", "c", "l")
  } else yh <- NULL
  
  if (identical(output, "colors")) {
    yc
  } else {
    y <- do.call(cbind, list(yh, yr))
    if ("colors" %in% output) {
      y <- as.data.frame(y)
      y$color <- yc
    }
    y
  }
}


get_cartesian <- function(H, C, L) {
  data.frame(x=sin(H*pi/180) * (C/100),
    y=cos(H*pi/180) * (C/100),
    z=L/100)
}

get_HCL <- function(x, y, z) {
  H <- atan2(x,y)/pi*180
  H[which(H < 0)] <- H[which(H < 0)] + 360
  data.frame(H=H,
    C=sqrt(x^2 + y^2)*100,
    L=z*100)
}


# 
# H1 <- 0
# k <- 3
# 
# H <- seq(0, 360, length.out=k+1)[-(k+1)]
# 
# x <- cos(H/180*pi)
# y <- sin(H/180*pi)
# 
# df <- df[,1:3]
# 
# x_new <- weighted.mean(x, w=c(.3333, 0, .6667))
# y_new <- weighted.mean(y, w=c(.3333, 0, .6667))
# 
# 
# C_new <- sqrt(x_new^2+y_new^2) * 100
# H_new <- atan2(y_new, x_new) * 180 / pi

