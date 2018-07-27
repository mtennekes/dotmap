library(sf)
library(tmap)

bsmap <- "/media/mtes/LaCie/MTES/dotmap/"
#bsmap <- "/media/tijn/LaCie/MTES/dotmap/"

load(file.path(bsmap, "dotmap_data_adam2/source/region.rdata"))
region <- st_geometry(region)

load(file.path(bsmap, "dotmap_data_adam2/source/area1.rdata"))
load(file.path(bsmap, "dotmap_data_adam2/source/area2.rdata"))
area1 <- st_geometry(area1)
area2 <- st_geometry(area2)

load(file.path(bsmap, "dotmap_data_adam2/source/pop_data_hh_type.rdata"))
pop_hh <- pop

load(file.path(bsmap, "dotmap_data_adam2/source/pop_data_income.rdata"))
pop_inc <- pop

pop_tables <- list(hh = pop_hh,
                   inc = pop_inc)


load(file.path(bsmap, "dotmap_data_adam2/source/region_meta.rdata"))
pop_totals <- rmeta


data("NLD_muni")


dotmap_data_adam2 <- dotmap_data(dir="test/adam",
                                 area1 = area1, 
                                 area2 = area2,
                                 region = region,
                                 pop_totals = pop_totals,
                                 pop_tables = pop_tables,
                                 dens_ub = c(28000, 20000, 15000, 10000, 10000),
                                 dens_lb = c(0, 0, 0, 0, 0),
                                 bbx=NA,
                                 z = list(c(14, 7, 16)), #17
                                 z_arr=10,
                                 tile_size=256,
                                 transparent=TRUE)

saveRDS(dotmap_data_adam2, "test/adam/dotmap_data.rds")

dotmap_data_adam2 <- readRDS("test/adam/dotmap_data.rds")


library(doParallel)
library(parallel)
library(foreach)

nclusters <- detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)

create_area_maps_sf(dotmap_data_adam2, pkg = ".")
create_area_maps_sf_sec(dotmap_data_adam2, pkg = ".")

subtract_area_maps(dotmap_data_adam2, pkg = ".")

create_region_maps(dotmap_data_adam2, pkg = ".")

determine_region_per_area_pixel(dotmap_data_adam2, pkg = ".")

sample_pop_to_pixels(dotmap_data_adam2, pkg = ".")

aggregate_lower_zooms(dotmap_data_adam2, pkg = ".")



process_dotmap_data(dotmap_data_adam2[[1]], clusters = 2,
                    actions = "determine_region_per_area_pixel",
                    pkg="~/git/dotmap/pkg")



stopCluster(cl)


process_dotmap_data(dotmap_data_adam2, clusters = 3,
                    actions = c("create_area_maps_sf", "create_area_maps_sf_sec"),
                    pkg="~/git/dotmap/pkg")
