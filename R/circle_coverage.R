circle_coverage <- function(n=4, radius=1, N=1000, shiftx=0, shifty=0) {
  k <- N^2 / (n^2)

  g <- expand.grid(seq(0,1,length.out=N),
                   seq(0,1,length.out=N), KEEP.OUT.ATTRS = FALSE) %>%
    rename(x=Var1,y=Var2) %>% 
    mutate(cover = sqrt(((x - .5 - shiftx)  * 2)^2 + ((y - .5 - shifty)  * 2)^2) <= radius) %>% 
    mutate(xbin=ntile(x,n),
           ybin=ntile(y,n)) %>% 
    group_by(xbin, ybin) %>% 
    summarise(n=sum(cover)/length(cover)) %>% 
    spread(xbin, n)
  g
  
  as.matrix(g[,-1])
}

stack_patterns <- function(m, nr, nc) {
  kc <- ceiling(nc / ncol(m))
  kr <- ceiling(nr / nrow(m))
  
  ml <- lapply(1:kc, function(i) m)
  
  m2 <- do.call(cbind, ml)[,1:nc]
  
  ml2 <- lapply(1:kr, function(i) m2)
  m3 <- do.call(rbind, ml2)[1:nr,]
  m3
}

