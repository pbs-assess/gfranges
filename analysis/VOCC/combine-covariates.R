library(dplyr)

events <- readRDS("analysis/VOCC/data/events-trawled.rds")
events2 <- events %>% dplyr::select(-catch_weight, -present, -density,-X, -Y, -depth, -akima_depth)
glimpse(events)
substrate <- readRDS("analysis/VOCC/data/events-w-substrate.rds")

substrate2 <- as_tibble(substrate) 
substrate2 <- substrate2 %>% dplyr::select(fishing_event_id, mixed, muddy, sandy, rocky)

events2 <- as_tibble(events2)
events_w_covs <- left_join(events2, substrate2)

saveRDS(events_w_covs, file = "analysis/VOCC/data/event-covariates.rds")




# put prediction data in raster form
nd_all <- readRDS("analysis/VOCC/data/nd_all_synoptic.rds")
nd <- nd_all %>% filter(year %in% c(2005,2006)) %>%
  mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
nd_raster <- raster::rasterFromXYZ( nd %>% dplyr::select(X, Y, depth), crs = proj)



new_substrate <- readRDS("analysis/VOCC/data/new-substate-raster.rds")

# convert to data.frame for prediction
coords <- as.data.frame(raster::rasterToPoints(new_substrate[[1]]))[, c("x", "y")]

vars <- list()
for (i in seq_len(length(names(new_substrate)))) {
  vars[[i]] <- as.data.frame(raster::rasterToPoints(new_substrate[[i]]))[, 3]
}

nd_substrate <- do.call("cbind", vars)
nd_substrate <- as.data.frame(nd_substrate)
names(nd_substrate) <- names(new_substrate)
nd_substrate <- cbind(coords, nd_substrate)
nd_substrate$X <- round(nd_substrate$x/1000)
nd_substrate$Y <- round(nd_substrate$y/1000)

nrow(nd_substrate)


nd_new <- nd_all %>% filter(year %in% c(2005,2006))
nrow(nd_new)
nd_new <- left_join(nd_new, nd_substrate)

trawl <- readRDS("analysis/VOCC/data/trawl-footprint.rds")
trawl <- trawl %>% select(-x, -y)
nd_test <- left_join(nd_new, trawl)

#View(nd_combind)
glimpse(nd_test)

#View(nd_test)


nd_combind <- nd_test %>% select(-x, -y, -substrate_100m_1step)


saveRDS(nd_combind, file = "analysis/VOCC/data/new_covariates.rds")



# THIS SHOULD probably be done by make grids so that only years with samples are created...
# 
# nd_combind$year <- NULL
# original_time <- sort(unique(nd_all$year))
# new_all <- do.call(
#   "rbind",
#   replicate(length(original_time), nd_combind, simplify = FALSE)
# )
# new_all[["year"]] <- rep(original_time, each = nrow(nd_combind))
# 
# glimpse(new_all)
# glimpse(nd_all)
# saveRDS(new_all, file = "analysis/VOCC/data/new_covariates.rds")

