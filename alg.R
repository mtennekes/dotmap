x <- readRDS("../aggregating_dots/data/commuting_dist.rds")


M <- x



library(tidyverse)
library(igraph)

edgelist <- data.frame(
  from = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 8),
  to = c(2, 3, 4, 5, 6, 4, 5, 6, 7, 8, 7, 8, 7, 8, 9, 9),
  capacity = c(20, 30, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99),
  cost = c(0, 0, 1, 2, 3, 4, 3, 2, 3, 2, 3, 4, 5, 6, 0, 0))

g <- graph_from_edgelist(as.matrix(edgelist[,c('from','to')]))

E(g)$capacity <- edgelist$capacity
E(g)$cost <- edgelist$cost

plot(g, edge.label = E(g)$capacity)
plot(g, edge.label = E(g)$cost)

# coordinates from 0 to 1
get_coor <- function(i, j, n) {
  list(x = (j - .5) / n,
       y = (i - .5) / n)
}


alg_wouter <- function(M, k = 2) {
  mode(M) <- "integer"
  
  n <- nrow(M)

  # start at one  
  start_at_zero <- (min(M) == 0L)
  if (start_at_zero) M <- M + 1L
  
  nc <- max(M)
  
  cbM <- tabulate(M, nbins = nc)
  
  
  s <- n / k
  
  A <- matrix(0L, nrow = s, ncol = s)
  
  while(min(b) == 0L) {
    cbA <- tabulate(A, nbins = nc) 
    
    # find class for which class balance is worst
    z <- which.min(sapply(1L:nc, function(i) {
      penalty <- ifelse(i == 1L, .1, 0)
      ((cbA[i] + 1) / cbM[i]) + penalty
    }))
    
    
    mv <- as.vector(M)
    
    ii <- rep(1:n, times = n)
    jj <- rep(1:n, each = n)
    
    colist <- get_coor (ii, jj, n)
    
    
    Mdf <- data.frame(i = ii, j = jj, x = colist$x, y = colist$y, cid = mv, id = 1L:as.integer(n^2))
    
    
    av <- as.vector(A)
    aii <- rep(1:s, times  = s)
    ajj <- rep(1:s, each  = s)
    acolist <- get_coor (aii, ajj, s)
    
    Adf <- data.frame(i = aii, j = ajj, x = acolist$x, y = acolist$y, cid = av, id = (as.integer(n^2)+1):(as.integer(n^2) + s^2))
    
    
    iddf <- expand.grid(Mdf$id, Adf$id)
    
    
    
    
    for (cl in 1:nc) {
      Mdfsel <- Mdf %>% filter(cid == cl)
      Adfsel <- Adf %>% filter(cid == cl)
      
      if (nrow(Adfsel) > 0) {
        Mdfsel2 <- do.call(rbind, lapply(1:nrow(Adfsel), FUN = function(f) Mdfsel))
      }
      
      
    }
    
  }
  
  
}