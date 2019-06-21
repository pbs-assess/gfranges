library(dplyr)

# put prediction data in raster form
nd_all <- readRDS("analysis/VOCC/data/nd_all_synoptic.rds")
nd <- nd_all %>% filter(year %in% c(2005,2006)) %>%
  mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
nd_raster <- raster::rasterFromXYZ( nd %>% dplyr::select(X, Y, depth), crs = proj)


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

# Crop extent to match projection data
new_extent <- raster::crop(new_extent, nd_raster)

# Project values to new raster
hres_substrate <-raster::projectRaster(substrate, new_extent) #, method = "ngb")
saveRDS(hres_substrate, file = paste0("analysis/VOCC/data/highres-substate-raster.rds"))


# Adjust the cell size 
raster::res(new_extent) <- 2000

# Project values to new raster
new_substrate <-raster::projectRaster(substrate, new_extent) #, method = "ngb")
saveRDS(new_substrate, file = "analysis/VOCC/data/new-substate-raster.rds")


