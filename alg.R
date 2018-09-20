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

get_coor <- function(i, j, n) {
  
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
    
    z <- which.min(sapply(1L:nc, function(i) {
      penalty <- ifelse(i == 1L, .1, 0)
      ((cbA[i] + 1) / cbM[i]) + penalty
    }))
    
    
    mv <- as.vector(M)
    
    
  }
  
  
}