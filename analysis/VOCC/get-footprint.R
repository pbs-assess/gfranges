library(tidyverse)
getwd()
setwd(here::here("/analysis/VOCC"))

# Add "trawled" column to event data

# put event data in sf form
bath <- readRDS("data/bathymetry-data") 
events <- bath$data %>% mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms

trawl_footprint <- sf::st_read(dsn="data/trawl-footprint",layer="Trawl_footprint")
trawl_footprint <- sf::st_transform(trawl_footprint, crs = 4326)

xx <- events %>%
  sf::st_as_sf(coords = c("lon", "lat"),
    crs = 4326, remove = FALSE)

int <- sf::st_intersects(trawl_footprint, xx)

events_in_trawled_area <- xx$fishing_event_id[unlist(int)]
xx$trawled <- if_else(xx$fishing_event_id %in% events_in_trawled_area, 1, 0)

# # plot to see if working correctly
# plot(sf::st_geometry(trawl_footprint))
# plot(sf::st_geometry(xx[,"fishing_event_id"]), col = "black", pch = 4, cex = 0.25,
#   add = TRUE, reset = FALSE)
# xx_trawled <- xx %>% filter(trawled == 1)
# plot(sf::st_geometry(xx_trawled[,"fishing_event_id"]), col = "red", pch = 4, cex = 0.25,
#   add = TRUE, reset = FALSE)

saveRDS(xx, file = "data/events-trawled.rds")



###################

# add trawled footprint to prediction grid in raster form

library(rgdal)
library(raster)

shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
  proj.to = NA, map = TRUE) {
  #require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), r, updatevalue = 0, filename = label, format = "GTiff",
    overwrite = T)
  
  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }
  
  names(r) <- label
  return(r)
}

# retrieve trawl shapefile
trawl_footprint <-readOGR(dsn="data/trawl-footprint",layer="Trawl_footprint")
plot(trawl_footprint)

# set projections
proj.from <- proj4string(trawl_footprint)
proj.to <- "+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# put prediction data in raster form
nd_all <- readRDS("data/nd_all_synoptic.rds")
nd <- nd_all %>% filter(year %in% c(2005,2006)) %>%
  mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
nd_raster <- raster::rasterFromXYZ( nd %>% dplyr::select(X, Y, depth), crs = proj.to)

# extract all polygons and convert to a raster where cells
# associated with raster have a value of 1 and everything else has a
# value of 0.
footprint <- shp2raster(shp = trawl_footprint,
  mask.raster = nd_raster, label = "trawled", 
  transform = TRUE, proj.from = proj.from,
  proj.to = proj.to, value = 1)

coords <- as.data.frame(raster::rasterToPoints(footprint))[, c("x", "y")]
trawled <- as.data.frame(raster::rasterToPoints(footprint))[, 3]
glimpse(trawled)
trawl <- cbind(coords, trawled)
trawl$X <- trawl$x/1000
trawl$Y <- trawl$y/1000

saveRDS(trawl, file = "data/trawl-footprint.rds")



#####################################

