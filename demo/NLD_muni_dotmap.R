library(tmap)

data("NLD_muni")

data("NLD_pop")
data("NLD_age")
data("NLD_origin")
data("NLD_gender")

data("NLD_area1")
data("NLD_area2")

dotmap_data("test", 
            area1 = NLD_area1, 
            area2 = NLD_area2,
            region = NLD_muni,
            pop_totals = NLD_pop,
            pop_tables = list(age = NLD_age, 
                              gender = NLD_gender, 
                              origin = NLD_origin),
            dens_ub=NULL,
            dens_lb=NULL,
            bbx=NA,
            z=c(7, 10, 14),
            z_arr=7,
            tile_size=256,
            transparent=TRUE)