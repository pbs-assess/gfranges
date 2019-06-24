library(tidyverse)

library(rgdal)
library(raster)

# retrieve substate layers
substrate <- readRDS("analysis/VOCC/data/substrate-raster.rds") 

# original projection
# proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0"
# projdefs <- "+datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# geoCRS <- paste( proj, projdefs, sep=" " )

# new projection
proj <- "+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

new_extent <- raster::projectExtent(substrate, crs = proj)
raster::res(new_extent) <- 100

# # Project values to new raster
# hres_substrate <-raster::projectRaster(substrate, new_extent) #, method = "ngb")
# saveRDS(hres_substrate, file = "analysis/VOCC/data/highres-substate-raster.rds")


# Add substrate columns to event data

# put event data in sf form
bath <- readRDS("analysis/VOCC/data/bathymetry-data") 
events <- bath$data %>% mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
glimpse(events)

hres_substrate <- readRDS("analysis/VOCC/data/highres-substate-raster.rds") 

# make spatial points dataframe
pts <- cbind(events$X, events$Y)
xx <- SpatialPointsDataFrame(pts, events, proj4string=crs(proj)) 
sp <- SpatialPoints(pts)

# calculate proportion of each substrate within 1 km radius of each fishing event
events_w_substrate <- extract(hres_substrate, xx, df=TRUE, sp = TRUE, buffer=1000, fun = mean)

saveRDS(events_w_substrate, file = "analysis/VOCC/data/events-w-substrate.rds")



###################

# add substrate data to prediction grid


# put prediction data in raster form
nd_all <- readRDS("analysis/VOCC/data/nd_all_synoptic.rds")
nd <- nd_all %>% filter(year %in% c(2005,2006)) %>%
  mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
nd_raster <- raster::rasterFromXYZ( nd %>% dplyr::select(X, Y, depth), crs = proj)


# Crop extent to match projection data
new_extent <- raster::crop(new_extent, nd_raster)

# Adjust the cell size 
raster::res(new_extent) <- 2000

# Project values to new raster
nd_substrate <-raster::projectRaster(substrate, new_extent) #, method = "ngb")
saveRDS(nd_substrate, file = "analysis/VOCC/data/new-substate-raster.rds")


