library(tmap)

data("NLD_muni")

data("NLD_pop")
data("NLD_age")
data("NLD_origin")
data("NLD_gender")

data("NLD_area1")
data("NLD_area2")

settings_age <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, palette = rainbow(5), H.method="cat", C.max = 100, C.method = "triangle")
settings_gender <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, palette = c("blue", "pink"), H.method="cat", C.max = 100, C.method = "triangle")
settings_origin <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, H.method="cat", C.max = 100, C.method = "triangle")



NLD_demo <- dotmap_project(dir="test/NLD_demo",
                           area1 = NLD_area1, 
                           area2 = NLD_area2,
                           region = NLD_muni,
                           pop_totals = NLD_pop,
                           pop_tables = list(age = NLD_age,
                                             gender = NLD_gender,
                                             origin = NLD_origin),
                           dens_ub = NULL,
                           dens_lb = NULL,
                           bbx=NA,
                           z = list(c(7, 7, 9),
                                    c(9, 10, 11)),
                           z_arr=9,
                           tile_size=256,
                           transparent=TRUE,
                           settings=list(age=settings_age, gender=settings_gender, origin=settings_origin))

library(doParallel)
library(parallel)
library(foreach)

cl <- makeCluster(3)
registerDoParallel(cl)

process_dotmap <- function() {
  create_area_maps_sf(NLD_demo, pkg = ".")
  create_area_maps_sf_sec(NLD_demo, pkg = ".")
  subtract_area_maps(NLD_demo, pkg = ".")
  create_region_maps(NLD_demo, pkg = ".")
  determine_region_per_area_pixel(NLD_demo, pkg = ".")
  sample_pop_to_pixels(NLD_demo, pkg = ".")

  aggregate_dotmap_data(NLD_demo, pkg = ".", s=4)

  aggregate_lower_zooms(NLD_demo, pkg = ".")

  plot_dotmap(NLD_demo, pkg = ".")
  make_tile_server(NLD_demo, pkg = ".")
}

process_dotmap()

dotmap(NLD_demo)

x <- load("test/NLD_demo/dotmap_data/res10/gender/10/pop_per_pixel_table.rdata")




stopCluster(cl)