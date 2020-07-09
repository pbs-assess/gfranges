library(sp)
library(raster)
library(tidyverse)

setwd(here::here("analysis", "tmb-sensor-explore"))

temp_all_years <- stack()

for(i in 2003:2018){
  temp_one_year <- raster(paste("data/roms_temp/temp_bcroms_y", i, ".tif", sep = ""))
  names(temp_one_year) <- paste0("T", i)
  temp_all_years <- stack(temp_all_years, temp_one_year)
}

# bath <- readRDS(here::here("analysis/VOCC/data/bathymetry-data")) 
# events <- bath$data %>% mutate(X_old=X*1000, Y_old=Y*1000) %>% select(-X, -Y)
# xx <- events %>%
#   sf::st_as_sf(coords = c("lon", "lat"),
#     crs = 4326 , 
#     remove = FALSE)


events <- m_temp_rw$data %>% mutate(X_old=X*1000, Y_old=Y*1000) %>% select(-X, -Y)


xx <- events %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
    crs = 4326 ,
    remove = FALSE)

trawl <- sf::st_transform(xx, crs = 
    CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# this should be your x,y coordinates that you want to extract. They will need to be in the CRS specified on the next line
trawl_coord <- sf::st_coordinates(trawl)
ggplot(as.data.frame(trawl_coord), aes(X,Y)) + geom_point()

# ### alternative method gets same result
# lonlat <- events %>% dplyr::select(lon, lat)
# coordinates(lonlat) <- events %>% dplyr::select(lon, lat)
# proj4string(lonlat) <-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# latlon_UTM <- spTransform(lonlat, CRSobj = CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# trawl_coord2 <- coordinates(latlon_UTM)
# trawl_coord2 <- as.data.frame(trawl_coord2) %>% rename (x = lon, y = lat)
# ggplot(as.data.frame(trawl_coord2), aes(x,y)) + geom_point()

# convert to projection of ROMS tifs
sets_sP <- SpatialPoints(trawl_coord, proj4string = 
    CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

trawl_all <- cbind(trawl_coord, events)
# trawl_all <- cbind(trawl_coord2, events)

extracted_env_lines_temp_all <- raster::extract(x = temp_all_years, y = sets_sP)

all_events_roms <- cbind(trawl_all, extracted_env_lines_temp_all) %>% 
  mutate(X_roms = X, Y_roms = Y, X = X_old/1000, Y = Y_old/1000)

ggplot(all_events_roms, aes(X,Y, colour=T2018)) + geom_point()


events_roms <- all_events_roms %>% mutate(
  roms=case_when(
    year=="2003" ~ T2003,
    year=="2004" ~ T2004,
    year=="2005" ~ T2005,
    year=="2006" ~ T2006,
    year=="2007" ~ T2007,
    year=="2008" ~ T2008,
    year=="2009" ~ T2009,
    year=="2010" ~ T2010,
    year=="2011" ~ T2011,
    year=="2012" ~ T2012,
    year=="2013" ~ T2013,
    year=="2014" ~ T2014,
    year=="2015" ~ T2015,
    year=="2016" ~ T2016,
    year=="2017" ~ T2017,
    year=="2018" ~ T2018
  ))

filter(events_roms, year== 2012)

saveRDS(events_roms, file = "data/roms_temp_by_events2.rds")

######

temp_stack <- stack()
for(i in 2003:2005){
  temp_one_year <- raster(paste("data/roms_temp/temp_bcroms_y", i, ".tif", sep = ""))
  names(temp_one_year) <- paste0("T", i)
  temp_stack <- stack(temp_stack, temp_one_year)
}

# temp_roms <- as.data.frame(raster::rasterToPoints(temp_stack)) #[, c("x", "y")]

proj.to <- "+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

library(rgdal)

# test on partial first
roms_halfkm <- aggregate(temp_stack, fact=5)

# # if works, run for all years
# roms_halfkm <- aggregate(temp_all_years, fact=5)

roms_projected <- projectRaster(roms_halfkm, crs = proj.to)

temp_roms <- as.data.frame(raster::rasterToPoints(roms_projected)) #[, c("x", "y")]

# ggplot(temp_roms, aes(x,y, colour=T2003)) + geom_point()

d <- temp_roms 

d$X <- d$x/1000
d$Y <- d$y/1000

#d <- filter(d, !is.na(X), !is.na(Y))

grid <- readRDS(here::here("prediction-grids/overall-grid.rds"))

grid$X_lower <- grid$X - 1
grid$Y_lower <- grid$Y - 1
range(grid$X)
range(grid$Y)

# plot(grid$X,grid$Y)

d <- filter(d, X < 808) 
d <- filter(d, X > 166) 
d <- filter(d, Y < 6060) 
d <- filter(d, Y > 5346) 

# plot(d$X,d$Y)

d$X <- 2 * round(d$X/2)
d$Y <- 2 * round(d$Y/2)


data <- d %>% group_by(X, Y) %>% summarise_all(mean) 


data <- data %>% select(-x,-y) %>% 
  pivot_longer(T2003:T2018, names_to = "year", values_to = "roms_temp") %>% mutate(
  year = as.numeric(gsub("T", "", year)) 
) 


# d <- st_set_geometry(d, NULL) 

pred <- readRDS((here::here("analysis/VOCC/data/predicted-DO-2020-06-20-more2016.rds")))

both <- left_join(pred, data)
glimpse(both)

saveRDS(both, file = here::here("analysis/VOCC/data/predicted-DO-2020-06-20-more2016-roms.rds"))