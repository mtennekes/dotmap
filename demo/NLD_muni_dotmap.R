data("NLD_muni", package = "tmap")
data("NLD_age")
data("NLD_origin")
data("NLD_gender")

data("NLD_area1")
data("NLD_area2")

settings_age <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, H.method="div", C.max = 100, C.method = "triangle")
settings_gender <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, palette = c("blue", "pink"), H.method="cat", C.max = 100, C.method = "triangle")
settings_origin <- list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, H.method="cat", C.max = 100, C.method = "triangle")

NLD_demo <- dotmap_project(dir="test/NLD_demo",
                           area1 = NLD_area1, 
                           area2 = NLD_area2,
                           vars = c("age", "gender", "origin"),
                           var_titles = c("Age", "Gender", "Origin"),
                           region = NLD_muni,
                           title = "Dutch Municipality Dotmap",
                           region_title = "Municipality borders",
                           labels_title = "Labels",
                           dots_text = c("1 dot = 16000 persons", "1 dot = 1000 persons"),
                           dotmap_attr = "&copy; <a href='https://www.cbs.nl'>CBS</a>",
                           pop_tables = list(age = NLD_age,
                                             gender = NLD_gender,
                                             origin = NLD_origin),
                           bbx=NA,
                           z = list(c(7, 8, 9),
                                    c(9, 10, 11)),
                           tile_size=256,
                           transparent=TRUE,
                           settings=list(age=settings_age, gender=settings_gender, origin=settings_origin))

library(doParallel)
library(parallel)
library(foreach)

cl <- makeCluster(3)
registerDoParallel(cl)

process_dotmap <- function() {
  create_area_maps(NLD_demo)
  create_area_maps(NLD_demo, primary = FALSE)
  subtract_area_maps(NLD_demo)
  create_region_maps(NLD_demo)
  determine_region_per_area_pixel(NLD_demo)
  dist_pop_to_pixels(NLD_demo)
  aggregate_dotmap_data(NLD_demo)
  aggregate_lower_zooms(NLD_demo)
  plot_dotmap(NLD_demo)
  make_tile_server(NLD_demo)
}

process_dotmap()

## Make sure XAMPP is installed and running

# Windows:
# - Install Xampp
# - Copy files from [project dir]/htmlserver to the localhost (by default c:\xampp\htdocs)
# - Run Xampp
# - Start Apache server

# Ubuntu:
# - Installation, see e.g. http://www.codebind.com/linux-tutorials/install-xampp-ubuntu-18-04/
# - Open /opt/lampp/etc/httpd.conf and find the DocumentRoot. Change this folder if you like.
# - Copy files from [project dir]/htmlserver to the DocumentRoot
# - Run sudo /opt/lampp/manager-linux-x64.run
# - Start Apache Web Server



show_dotmap(NLD_demo, localhost = "http://127.0.0.1/NLD")

create_dotmap_website(NLD_demo, localhost = "http://127.0.0.1/NLD")



stopCluster(cl)


