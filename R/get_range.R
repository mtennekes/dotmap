get_range <- function(i, n) {
  iname <- deparse(substitute(i))
  s <- 1:n
  
  if (is.null(i)) s else {
    if (!all(i %in% s)) stop(iname, " should be in range 1 to ", n)
    i    
  }
}

get_dims <- function(dm) {
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  c(ri_arr$nx, ri_arr$ny)
}
