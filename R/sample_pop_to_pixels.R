#' Sample population to pixels at the highest zoom level
#' 
#' @param dm dotmap_info object
#' @param i tile row. If \code{NULL} (default) all rows are processed
#' @param j tile column. If \code{NULL} (default) all columns are processed
sample_pop_to_pixels <- function(dm, i=NULL, j=NULL, logfile=NULL, bound=TRUE, pkg="pkg") {
  rmeta <- readRDS(file.path(dm$dir_tiles_areas, "rmeta_pixels.rds"))
  
  nvars <- length(dm$m)
  nagg <- length(dm$z_res)
  
 
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  
  ri_res <- dm$ri[[paste0("z", dm$z_res)]]
  
  res <- ri_res$py/ri_arr$ny
  
  rnd <- random_seq(res)
  
  for (a in 1:nagg) {
    for (k in 1:nvars) {
      dmk <- dm
      dmk$m <- dmk$m[k]
      
      dmk$z_from <- dmk$z_from[a]
      dmk$z_to <- dmk$z_to[a]
      dmk$z_res <- dmk$z_res[a]
      dmk$resname <- paste0("res", dmk$z_res)
      
      dmk$pop_table <- dmk$pop_tables[[k]]
      dmk$pop_table_name <- names(dmk$pop_tables)[k]
      dmk$pop_tables <- NULL
      sample_one_pop_to_pixels(dmk, i=i, j=j, logfile=logfile, bound=bound, pkg=pkg, rnd=rnd)
    }
  }
}
  
random_seq <- function(res) {
  ## generate quasi-random sequence 
  set.seed(123456789)
  hm <- randtoolbox::halton(res^2, dim=2)
  hm <- floor(hm * res) + 1
  rnd <- as.integer(hm[,1] + (hm[,2]-1) * res)
  
  rnd <- rnd[!duplicated(rnd)]
  rnd_missings <- sample(setdiff(1:res^2, rnd))
  c(rnd, rnd_missings)
}

sample_one_pop_to_pixels <- function(dm, i, j, logfile, bound, pkg, rnd) {
  
  
  #load(file.path(dm$file_pop))
  
  pop <- dm$pop_table
  
  rmeta <- readRDS(file.path(dm$dir_tiles_areas, "rmeta_pixels.rds"))

  dir <- file.path(dm$dir_dotmap_data, dm$resname, dm$pop_table_name, dm$z_res)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  ri_arr <- dm$ri[[paste0("z", dm$z_arr)]]
  
  ri_res <- dm$ri[[paste0("z", dm$z_res)]]
  
  res <- ri_res$py/ri_arr$ny
  
  message("sample population to pixels")
  #rmeta$pop
  
  ais <- list(
    list(rmeta = data.frame(pixels=rmeta$pixels1, pop=rmeta$pop1),
         pops = pop * (rmeta$pop1 / rmeta$pop),
         rcol = "region1"),
    list(rmeta = data.frame(pixels=rmeta$pixels2, pop=rmeta$pop2),
         pops = pop * (rmeta$pop2 / rmeta$pop),
         rcol = "region2")
  )
  

  seti <- get_range(i, ri_arr$nx)
  setj <- get_range(j, ri_arr$ny)
    
  
  if (!is.null(logfile)) if (!file.exists(logfile)) writeLines(c(""), logfile)

  pop_per_pix <- foreach(i=seti, .combine='+') %do% { 
    devtools::load_all(pkg)
    library(data.table)
    if (!is.null(logfile)) {
      f <- openLog(logfile)
    }
    pop_per_pix <- rep(0L, 1001)  
    
    counts <- as.list(rep(0L, dm$m))
    names(counts) <- paste0("c", 1:dm$m)
    
    dt <- do.call(data.table::data.table, c(list(id=1L:as.integer(res^2)), counts))
    data.table::setkey(dt, id)
    
    dtref <- data.table::data.table(id=1L:as.integer(res^2), order=order(rnd))
    
    for (j in setj) {
      set.seed(123456789)
      message(i, " ", j, " (worker ", Sys.getpid(), ")")
      
      # reset counts
      dt[, (names(counts)):=counts]
      
      
      fnames <- c(file.path(dm$dir_tiles_areas, paste0("area_region1_", i, "_", j, ".rdata")),
                  file.path(dm$dir_tiles_areas, paste0("area_region2_", i, "_", j, ".rdata")))
      
      fexists <- file.exists(fnames)
      
      if (any(fexists)) data.table::setkey(dtref, id)
      
      if (fexists[1]) {
        load(fnames[1])
        dtref[, region1:=ids1]
        rm(ids1)
      }
      
      if (fexists[2]) {
        load(fnames[2])
        dtref[, region2:=ids2]
        rm(ids2)
      }
      gc()
      
      mapply(function(ai, fe) {
        if (!fe) return(NULL)
        
        rcol <- ai$rcol
        data.table::setkeyv(dtref, rcol)
        
        pix <- tabulate(dtref[[rcol]], nbins=dm$n)
        prp <- pix / ai$rmeta$pixels
        prp[is.nan(prp)] <- 0
        pop_ij <- data.matrix(round(ai$pops * prp))
        storage.mode(pop_ij) <- "integer"
        
        #a <- array(0L, dim = c(res^2, dm$m))
        
        regsel <- which(pix != 0)
        #counts <- as.list(rep(0L, dm$m))
        #names(counts) <- paste0("c", 1:dm$m)

        lapply(regsel, function(k) {
          #print(k)
          # select buurt
          if (pix[k]!=0) {
            
            dtrefs <- dtref[data.table(k)]

            # get pop totals and cumulative totals
            pp <- unlist(pop_ij[k,])
            
            ns <- nrow(dtrefs) # number of pixels
            np <- sum(pp) # population total
            
            # population is first equally spread among nl base layers
            # sel1_lst is list per category of selected pixels for nl base layers
            if (np > ns) {
              cat("too few pixels for tile ", i, j, " region ", k, ": ", " ns ", ns, " np ", np)
              if (!bound) {
                cat(". Oversampling\n")
                
                nl <- np %/% ns
                nsl <- nl * ns
                pp_l <- round(pp * (nsl/np))
                pp_l[which.max(pp)] <- nsl - sum(pp_l[-which.max(pp)])
                
                pp_l_cs <- c(1, cumsum(pp_l) + 1)
                selm <- sample(rep(dtrefs$id, nl))
                lapply(1:dm$m, function(L) {
                  tb <- tabulate(selm[pp_l_cs[L]:(pp_l_cs[L+1]-1)], nbins=res^2)
                  tbp <- which(tb>0)
                  coln <- paste0("c", L)
                  dt[tbp, (coln):=tb[tbp]]
                  NULL
                })
                pp <- pp - pp_l
              } else {
                xtra <- np-ns
                cat(". Dropping ", xtra, " population units\n")
                droppings <- sample(1:dm$m, size = xtra, replace = TRUE, prob = pp)
                pp <- pp - tabulate(droppings, nbins=dm$m)
              }
            }
            
            dtrefs[, order2:=order(order)]
            
            #rnd_sel <- rnd[rep_len(which(rnd %fin% dts$id), sum(pp))]

            ppcs <- c(1, cumsum(pp) + 1)
            for (L in 1:dm$m) {
              if (pp[L] > 0L) {
                rids <- dtrefs[order2>=ppcs[L] & order2<=(ppcs[L+1]-1)]$id
                coln <- paste0("c", L)
                if (any(dt[rids, ][[coln]]>0)) {
                  cat("!clipping tile", i, j, " region ", k, " L ", L)
                }
                dt[rids, (coln):=get(coln) + 1L]
              }
            }
          }
        })
        NULL
      }, ais, fexists)
      
      if (any(fexists)) {
        a <- as.matrix(dt[, paste0("c", 1:dm$m), with=FALSE])
        pop_per_pix_ij <- tabulate(rowSums(a)+1, nbins = 1001)
        
        if (any(pop_per_pix_ij[-c(1:2)]!=0)) cat("! clipping at tile ", i, j, "\n")
        
        pop_per_pix <- pop_per_pix + pop_per_pix_ij
        save(a, file = file.path(dir, paste0("pop_", i, "_", j, ".rdata")))
        rm(a)
      }
    } # close j
    if (!is.null(logfile)) closeLog(f)
    pop_per_pix
  } # close i
  save(pop_per_pix, file=file.path(dir, "pop_per_pixel_table.rdata"))
}


`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}
