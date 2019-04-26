library(tmap)
library(tmaptools)
library(sf)
library(units)
library(dplyr)

##########################################################################
##
## Area shapes
##
## Based on BBG2012 shapes
##
## NLD_area1 is BBG category 20 (residential areas)
## NLD_area2 is BBG main category "built-up area" minus NLD_area1
##
#########################################################################


# file <- tempfile(fileext = "zip")
# dir <- tempdir()
# 
# download.file("http://geodata.nationaalgeoregister.nl/bestandbodemgebruik2012/extract/bestandbodemgebruik2012.zip",
#               file) # NOTE: 882,9 MB
# 
# unzip(file, exdir = dir)
dir <- "build"

x <- read_shape(file.path(dir, "shapes/BBG2012hoofdgroep.shp"))

x2 <- x %>% filter(Hoofdgroep %in% c("Bebouwd", "Bedrijfsterrein"))
x3 <- simplify_shape(x2, fact = .1) 
x4 <- st_buffer(x3, 200)
x5 <- st_union(x4)
x5b <- st_sf(x5)
x5b$x <- 1
x6 <- simplify_shape(x5b, fact = .1) 

rm(x, x2, x3, x4, x5, x5b); gc()
save(x6, file = "build/x6.rda", compress = "xz")
#load("build/x6.rda")

y <- read_shape(file.path(dir, "shapes/BBG2012.shp"))
y2 <- y %>% filter(BG2012A == 20)
rm(x, y); gc()
y3 <- simplify_shape(y2, fact = .1) 
y4 <- st_buffer(y3, 200)
y5 <- st_union(y4)
y5b <- st_sf(y5)
y5b$y <- 1
y6 <- simplify_shape(y5b, fact = .1) 
rm(y2, y3, y4, y5, y5b); gc()
save(y6, file = "build/y6.rda", compress = "xz")
#load("build/y6.rda")


  
z6 <- st_difference(x6, y6)
z7 <- st_difference(x6, z6)
z7b <- st_cast(z7, "POLYGON")
z6b <- st_cast(z6, "POLYGON")
z6c <- z6b[st_area(z6b)>set_units(100000, m^2),]



NLD_area1 <- st_geometry(z7b)
NLD_area2 <- st_geometry(z6c)

qtm(NLD_area1, fill = "red") + qtm(NLD_area2, fill = "blue")

dir.create("data")
save(NLD_area1, file = "data/NLD_area1.rda", compress = "xz")
save(NLD_area2, file = "data/NLD_area2.rda", compress = "xz")


##########################################################################
##
## Region shapes
##
## Using tmap NLD_muni
##
#########################################################################

data(NLD_muni)

NLD_pop <- NLD_muni %>% 
  st_set_geometry(NULL) %>% 
  transmute(pop = round(population / 1000),
            class = 1L)

NLD_origin <- NLD_muni %>% 
  st_set_geometry(NULL) %>% 
  mutate(origin_west = round(origin_west / 100 * (population / 1000)),
         origin_non_west = round(origin_non_west / 100 * (population / 1000))) %>% 
  transmute(origin_native = round(population/1000) - origin_west - origin_non_west,
            origin_west = origin_west,
            origin_non_west = origin_non_west)
all(rowSums(NLD_origin) == NLD_pop$pop)

NLD_gender <- NLD_muni %>% 
  st_set_geometry(NULL) %>% 
  transmute(men = round(pop_men / 1000),
            women = round(population/1000) - men)
all(rowSums(NLD_gender) == NLD_pop$pop)

NLD_age <- NLD_muni %>% 
  st_set_geometry(NULL) %>% 
  mutate(age_0_14 = round(pop_0_14 / 100 * population/1000),
         age_15_24 = round(pop_15_24 / 100 * population/1000),
         age_25_44 = round(pop_25_44 / 100 * population/1000),
         age_65plus = round(pop_65plus / 100 * population/1000)) %>% 
  transmute(age_0_14 = age_0_14,
            age_15_24 = age_15_24,
            age_25_44 = age_25_44,
            age_45_64 = round(population/1000) - age_0_14 - age_15_24 - age_25_44 - age_65plus,
            age_65plus = age_65plus)
all(rowSums(NLD_age) == NLD_pop$pop)


#save(NLD_pop, file = "data/NLD_pop.rda", compress = "xz")
save(NLD_origin, file = "data/NLD_origin.rda", compress = "xz")
save(NLD_gender, file = "data/NLD_gender.rda", compress = "xz")
save(NLD_age, file = "data/NLD_age.rda", compress = "xz")


