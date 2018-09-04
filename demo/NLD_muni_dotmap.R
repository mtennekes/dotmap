

dotmap(NLD_demo)

x <- load("test/NLD_demo/dotmap_data/res10/gender/10/pop_per_pixel_table.rdata")

x <- load("test/NLD_demo/dotmap_data/res7/age/7/pop_1_7.rdata")

rmeta <- readRDS("test/NLD_demo/tiles_area/rmeta_pixels.rds")


stopCluster(cl)


# sudo /opt/lampp/lampp start
# sudo /opt/lampp/manager-linux-x64.run

# sudo gedit /etc/apache2/ports.conf      # port 80
# sudo gedit /etc/apache2/sites-available/000-default.conf  # port 80
# sudo gedit /opt/lampp/etc/httpd.conf  # DocumentRoot



dotmap(NLD_demo, label.region = "Municipality borders")



dotmap(adam, label.region = "Municipality borders", label.vars = c("Household", "Income", "Wood", "Random 4 categories"))
