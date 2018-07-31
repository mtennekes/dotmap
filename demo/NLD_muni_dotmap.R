library(tmap)

data("NLD_muni")

data("NLD_pop")
data("NLD_age")
data("NLD_origin")
data("NLD_gender")

data("NLD_area1")
data("NLD_area2")


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
                                    z = list(c(10, 7, 12)), #17
                                    z_arr=7,
                                    tile_size=256,
                                    transparent=TRUE,
                                    settings=list(L.delta = 0.7, L.w = 4, L.lim = c(85, 20), H1 = 0, H.method="circle", C.max = 100, C.method = "triangle"))

# dotmap_data("test", 
#             area1 = NLD_area1, 
#             area2 = NLD_area2,
#             region = NLD_muni,
#             pop_totals = NLD_pop,
#             pop_tables = list(age = NLD_age, 
#                               gender = NLD_gender, 
#                               origin = NLD_origin),
#             dens_ub=NULL,
#             dens_lb=NULL,
#             bbx=NA,
#             z=c(7, 10, 14),
#             z_arr=7,
#             tile_size=256,
#             transparent=TRUE)