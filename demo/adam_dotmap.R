library(sf)
library(tmap)
library(dplyr)

bsmap <- "/media/mtes/LaCie/MTES/dotmap/"
bsmap <- "/media/tijn/LaCie/MTES/dotmap/"

load(file.path(bsmap, "dotmap_data_adam2/source/region.rdata"))
region <- st_geometry(region)

load(file.path(bsmap, "dotmap_data_adam2/source/area1.rdata"))
load(file.path(bsmap, "dotmap_data_adam2/source/area2.rdata"))
area1 <- st_geometry(area1)
area2 <- st_geometry(area2)


load(file.path(bsmap, "dotmap_data_adam2/source/region_meta.rdata"))
pop_totals <- rmeta


load(file.path(bsmap, "dotmap_data_adam2/source/pop_data_hh_type.rdata"))
pop_hh <- pop

load(file.path(bsmap, "dotmap_data_adam2/source/pop_data_income.rdata"))
pop_inc <- pop

load(file.path(bsmap, "dotmap_data_adam2/source/pop_data_wood_heater.rdata"))
pop_wood <- pop[,1:2]

pop_four <- pop_totals %>% 
  mutate(p1 = runif(n()),
         p2 = runif(n(), max = 1 - p1),
         p3 = runif(n(), max = 1 - p1 - p2)) %>% 
  transmute(cat1 = floor(pop * p1),
            cat2 = floor(pop * p2),
            cat3 = floor(pop * p3),
            cat4 = pop - cat1 - cat2 - cat3)
         

pop_tables <- list(hh = pop_hh,
                   inc = pop_inc,
                   wood = pop_wood,
                   four = pop_four)




data("NLD_muni")

settings1 <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, H.method="cat", C.max = 100, C.method = "triangle")
settings2 <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, C.max = 100, C.method = "triangle", palette=c("#FF0000", "#0000FF"))
settings3 <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, C.max = 100, C.method = "triangle", palette=c("#00B3FF", "#55BF00", "#FF779B"))
settings4 <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, C.max = 100, C.method = "triangle", palette=tmaptools::get_brewer_pal("Dark2", n = 4, plot = F))



adam <- dotmap_project(dir="test/adam",
                       area1 = area1, 
                       area2 = area2,
                       region = region,
                       pop_totals = pop_totals,
                       pop_tables = pop_tables,
                       dens_ub = c(28000, 20000, 15000, 10000, 10000),
                       dens_lb = c(0, 0, 0, 0, 0),
                       bbx=NA,
                       z = list(c(8, 7, 11),
                                c(10, 12, 13), 
                                c(12, 14, 15), 
                                c(14, 16, 16)),
                       z_arr=10,
                       tile_size=256,
                       transparent=TRUE,
                       settings=list(hh = settings1,
                                     inc = settings3,
                                     wood = settings2,
                                     four = settings4))

saveRDS(adam, "test/adam/dotmap_data.rds")

adam <- readRDS("test/adam/dotmap_data.rds")


library(doParallel)
library(parallel)
library(foreach)

nclusters <- detectCores()
cl <- makeCluster(3)
registerDoParallel(cl)

process_dotmap <- function() {
  #create_area_maps_sf(adam, pkg = ".")
  #create_area_maps_sf_sec(adam, pkg = ".")
  #subtract_area_maps(adam, pkg = ".")
  #create_region_maps(adam, pkg = ".")
  #determine_region_per_area_pixel(adam, pkg = ".")
  #sample_pop_to_pixels(adam, pkg = ".")
  
  # aggregate_dotmap_data(adam, pkg = ".", s=4)
  # 
  # aggregate_lower_zooms(adam, pkg = ".")
  # 
  #plot_dotmap(adam, pkg = ".")
  make_tile_server(adam, pkg = ".")
}

process_dotmap()

stopCluster(cl)


# Launch local Apache server
# Install XAMPP or LAMP (Linux)
# sudo /opt/lampp/manager-linux-x64.run
# sudo cp -a * /var/www/html/

ttm()





region <- readRDS(adam$file_shp_region)
zmin <- min(adam$z_from)
zmax <- max(adam$z_to)

save(region, zmin, zmax, file="../tmap/adam.rdata")

tm_shape(region) +
  tm_borders() +
  tm_tiles("http://127.0.0.1/hh/{z}/{x}/{y}.png", group = "Household") +
  tm_tiles("http://127.0.0.1/inc/{z}/{x}/{y}.png", group = "Income") +
  tm_view(set.zoom.limits = c(zmin, zmax))

tm_shape(region) +
  tm_borders() +
  tm_tiles("http://127.0.0.1/hh/{z}/{x}/{y}.png", group = "Household") +
  tm_tiles("http://127.0.0.1/inc/{z}/{x}/{y}.png", group = "Income") +
  tm_tiles("http://127.0.0.1/wood/{z}/{x}/{y}.png", group = "Wood") +
  tm_tiles("http://127.0.0.1/four/{z}/{x}/{y}.png", group = "four") +
  tm_view(set.zoom.limits = c(zmin, zmax))



### test get_HCL

m2 <- matrix(c(400, 200, 100, 400, 400, 400, 100, 300, 200), ncol=3)

get_HCL_colors(m2)
