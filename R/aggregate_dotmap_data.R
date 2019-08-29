matrix_to_row_col <- function(m) {
  ids <- which(m!=0)
  
  cs <- ((ids-1) %/% nrow(m)) + 1
  rs <- ids - (cs-1)*nrow(m)
  as.matrix(data.frame(row=rs, col=cs))
}

#' Aggregate dotmap data
#' 
#' Aggregate dotmap data
#' 
#' @param dm dm
#' @import KernSmooth
#' @import abind
#' @export
aggregate_dotmap_data <- function(dm) {
  set.seed(12345)
  
  zres <- dm$z_res
  nvars <- length(dm$m)

  if (length(zres) == 1) {
    warning("Nothing to aggregate")
    return(invisible(NULL))
  }
  
  
  for (k in 1:nvars) {
    pop_table_name <- dm$vars[k]
    for (i in length(zres):2L) {
      zfrom <- zres[i]
      zto <- zres[i-1L]
      s <- dm$scale[i-1L]
      
      dir1 <- file.path(dm$dir_dotmap_data, paste0("res", zfrom), pop_table_name, zfrom)
      dir2 <- file.path(dm$dir_dotmap_data, paste0("res", zto), pop_table_name, zto)
      dir.create(dir2, recursive = TRUE, showWarnings = FALSE)

      f <- 2^(zfrom-zto)
      ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
      
      aggregate_dotmap_data_i(dir1=dir1, dir2=dir2, ri_arr=ri_arr, f=f, s=s)
    }
  }
  

}


# f is the scaling reduction per dimension (so 2 for one zoom level)
# s is the scaling factor for the number of dots (normally 2^2 for one zoom level)
aggregate_dotmap_data_i <- function(dir1, dir2, ri_arr, f, s) {
  

  
  seti <- 1L:ri_arr$nx
  setj <- 1L:ri_arr$ny
  
  #s <- f # not sure whether s!=f can happen
  
  
  
  for (i in seti) {
    for (j in setj) {
      cat("i ", i, " j ", j, "\n")
      
      fle <- paste0(dir1, "/pop_", i, "_", j, ".rdata")
      
      if (!file.exists(fle)) next
      load(fle)
      
      n <- sqrt(nrow(a))
      m <- ncol(a)
      k <- n / f
      
      a <- array(a, dim = c(n, n, m))
      
      #if (i == 1 && j == 2) browser()
      
      asums <- apply(a, 3, sum)
      ls <- round(asums / s)
      
      l <- sum(ls)
      
      if (l==0) {
        a <- matrix(0L, nrow=k^2, ncol=m)
        save(a, file = file.path(dir2, paste0("pop_", i, "_", j, ".rdata"))) 
        next
      }
      # x <- a[,, 1]
      # 
      # x2 <- bkde2D(matrix_to_row_col(x), bandwidth = c(1.5,1.5), gridsize=c(k, k), range.x=list(c(1, n), c(1, n)))$fhat
      # x2 <- x2 / sum(x2) * sum(x)
      # x2[x2<1] <- 0
      # 
      # library(plotly)
      # 
      # plot_ly(z=a[512:(512+128), 512:(512+128), 2], type="heatmap")
      # plot_ly(z=b[128:(128+32), 128:(128+32), 2], type="heatmap")
      # sum(a>0)
      # sum(b>0)

      # 2d kde
      #if (i==1 && j==7) browser()
      #bro

      blist <- lapply(1:m, function(ii) {
        if (asums[[ii]] == 0) return(matrix(0, ncol = k, nrow = k))
        
        # matrix with a buffer; otherwise the edges become to sparser
        abuff <- a[c(1:f, 1L:n, (n-(f-1)):n), c(1:f, 1L:n, (n-(f-1)):n), ii]
        
        #bi <- bkde2D(matrix_to_row_col(a[,,ii]), bandwidth = c(1.5,1.5), gridsize=c(k, k))$fhat
        #bi <- bkde2D(matrix_to_row_col(a[,,ii]), bandwidth = c(1.5,1.5), gridsize=c(k, k), range.x=list(c(1, n), c(1, n)))$fhat
        
        bi <- bkde2D(matrix_to_row_col(abuff), bandwidth = c(1.5,1.5), gridsize=c(k+2, k+2), range.x=list(c(1, n + (2*f)), c(1, n + (2*f))))$fhat
        bi <- bi[2:(k+1), 2:(k+1)]
        bi <- bi / sum(bi) * asums[ii]
        bi[bi < 1] <- 0
        bi
      })
      b <- do.call(abind, c(blist, list(along=3)))
      btot <- Reduce('+', blist)
      
      probs <- as.vector(btot)

      ids <- which(btot>0)


      
      if (length(ids) == 0) {
        a <- matrix(0L, nrow=k^2, ncol=m)
        save(a, file = file.path(dir2, paste0("pop_", i, "_", j, ".rdata"))) 
        next
      }
      
      
      a <- matrix(0L, nrow=k^2, ncol=m)

      b2 <- matrix(b, ncol=m)
    
      pw <- rep(1, m)      
      for (it in 1:1) { #10
        x <- sapply(ids, function(id) {
          prb <- b2[id, ] * pw
          if (all(prb==0)) return(NA)
          sample(1:m, size = 1, prob = prb)
        })
        pw <- pw / (tabulate(x, nbins = m) / ls)
        pw[is.infinite(pw) | is.nan(pw)] <- 1e6
      }

      for (ii in 1:m) {
        a[ids[which(x==ii)], ii] <- 1L
      }

      save(a, file = file.path(dir2, paste0("pop_", i, "_", j, ".rdata")))
    }
  }
  invisible(NULL)
  
}

