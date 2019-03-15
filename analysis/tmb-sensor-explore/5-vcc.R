# d_trawl <- gfplot::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
# saveRDS(d_trawl, file = "analysis/tmb-sensor-explore/dat-sensor-trawl.rds")
d_trawl <- readRDS("analysis/tmb-sensor-explore/dat-sensor-trawl.rds")

# d_ll <- gfplot::get_sensor_data_ll_ctd(c(22, 36), sensor_min_max = TRUE)
# saveRDS(d_ll, file = "analysis/tmb-sensor-explore/dat-sensor-ll.rds")
# d_ll <- readRDS("analysis/tmb-sensor-explore/dat-sensor-ll.rds")

# surv <- readRDS("../gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets
# saveRDS(surv, file = "analysis/tmb-sensor-explore/pacific-cod.rds")
surv <- readRDS("analysis/tmb-sensor-explore/pacific-cod.rds")

library(tidyverse)
library(sdmTMB)
ssid <- 4
survey_abbrev <- "SYN WCVI"

# ssid <- 1
# survey_abbrev <- "SYN QCS"

# ssid <- 3
# survey_abbrev <- "SYN HS"

ssid <- 16
survey_abbrev <- "SYN WCHG"

d_trawl <- d_trawl %>%
  dplyr::select(-count, -start_time, -end_time, -min, -max) %>%
  dplyr::mutate(attribute = tolower(attribute)) %>%
  dplyr::distinct() %>%
  reshape2::dcast(fishing_event_id + year + ssid + survey_desc ~ attribute,
    value.var = "avg")

surv_fish <- surv %>% dplyr::select(year, fishing_event_id, survey_series_id, longitude, latitude, density_kgpm2) %>%
  dplyr::distinct() %>%
  dplyr::rename(ssid = survey_series_id)

d_trawl <- left_join(surv_fish, d_trawl)
d_trawl2 <- dplyr::rename(d_trawl, X = longitude, Y = latitude)
d_trawl <- as_tibble(gfplot:::ll2utm(d_trawl2, utm_zone = 9))

d_fit <- d_trawl %>% dplyr::filter(ssid == ssid)
d_fit <- d_fit %>% rename(depth = depth_m)
d_fit <- d_fit %>% filter(!is.na(depth), depth > 0)
dat <- gfplot:::scale_survey_predictors(d_fit)

spde <- make_spde(dat$X, dat$Y, n_knots = 200)
plot_spde(spde)
m_temp <- sdmTMB(dat,
  temperature_c ~ 0 + as.factor(year) + poly(depth_scaled, 3),
  time = "year", spde = spde,
  family = gaussian(link = "identity"),
  ar1_fields = TRUE,
  include_spatial = FALSE,
  silent = FALSE)

grid_locs <- gfplot:::make_prediction_grid(filter(dat, year %in% c(2006, 2007)),
  survey = survey_abbrev, cell_width = 2)$grid
grid_locs <- rename(grid_locs, depth = akima_depth)
grid_locs$year <- NULL

# Expand the prediction grid to create a slice for each time:
original_time <- sort(unique(dat$year))
nd <- do.call("rbind",
  replicate(length(original_time), grid_locs, simplify = FALSE))
nd[["year"]] <- rep(original_time, each = nrow(grid_locs))

predictions <- predict(m_temp, newdata = nd)

plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed()
}

plot_map(predictions$data, "est") +
  scale_fill_viridis_c(trans = "sqrt", option = "C") +
  ggtitle("Prediction (fixed effects + all random effects)")
##################################################

library(vocc)
library(dplyr)
library(raster)
library(sp)
library(ggquiver)
library(ggsidekick)

# load in the temp data or anything else
# d = readRDS(file.choose())
d <- predictions$data

# Just to confirm the trends we're seeing are real,
# mod = mgcv::gam(est ~ s(year, k=5) + te(X,Y) + log(depth),
#   data=d)
# plot(mod)

# This parameter controls how the original 2-km projection is aggregated.
# for example, a value of 5 means that the raster would be reprojected to 10km grid
scale_fac = 3

# create a RasterBrick
# raster for each year
rlist = list()
for(i in 1:length(unique(d$year))) {
  rlist[[i]] = rasterFromXYZ(dplyr::filter(d,year==unique(d$year)[i]) %>%
      dplyr::select(X,Y,est))
  rlist[[i]] = raster::aggregate(rlist[[i]], fact=scale_fac)
}
# stack rasters into layers -> rasterbrick
rstack = stack(rlist[[1]], rlist[[2]])
for(i in 3:length(rlist)) {
  rstack = stack(rstack, rlist[[i]])
}
rbrick = brick(rstack)

# Then calculate the trend per pixel:
slopedat <- calcslope(rbrick)
#Then get the mean temperature for a time period and calculate the spatial gradient:
allyears <- rep(1, nlayers(rbrick))
mnsst <- stackApply(rbrick, indices = allyears, fun = mean)
spatx <- spatialgrad(mnsst)
#Now we can calculate the VoCC:
velodf <- calcvelocity(spatx, slopedat)

#Mapping it again is straightforward:
rtrend <- rgrad_lon <- rgrad_lat <- rvocc <- angle <- magn <- raster(rbrick)
rgrad_lat[spatx$icell] <- spatx$NS # latitude shift, NS
rgrad_lon[spatx$icell] <- spatx$WE # longitude shift, WE
rtrend[slopedat$icell] <- slopedat$slope
rvocc[velodf$icell] <- velodf$velocity

# convert to data frames for ggplot
rtrend_df = as.data.frame(rasterToPoints(rtrend)) %>%
  dplyr::rename(trend = layer)
rgradlat_df = as.data.frame(rasterToPoints(rgrad_lat)) %>%
  dplyr::rename(gradNS = layer)
rgradlon_df = as.data.frame(rasterToPoints(rgrad_lon)) %>%
  dplyr::rename(gradWE = layer)
rvocc_df = as.data.frame(rasterToPoints(rvocc)) %>%
  dplyr::rename(velocity = layer)

# create ggquiver plots. need dataframe of lon, lat, delta_lon, delta_lat, trend, velocity
df = dplyr::left_join(rtrend_df, rgradlat_df) %>%
  dplyr::left_join(rgradlon_df) %>%
  dplyr::left_join(rvocc_df)

gtrend = ggplot(df, aes(x,y,fill=trend)) +
  geom_raster() + scale_fill_gradient2(low="blue",high="red") +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Trend")

# spatial gradient plot
quantile_cutoff = 0.05 # for plotting
df = dplyr::mutate(df, u_velo = trend/gradNS,
  v_velo = trend/gradWE,
  ulow = quantile(u_velo,quantile_cutoff),
  uhi = quantile(u_velo,1-quantile_cutoff),
  vlow = quantile(v_velo,quantile_cutoff),
  vhi = quantile(v_velo,1-quantile_cutoff),
  u_velo = ifelse(u_velo < ulow, ulow, u_velo),
  u_velo = ifelse(u_velo > uhi, uhi, u_velo),
  v_velo = ifelse(v_velo < vlow, vlow, v_velo),
  v_velo = ifelse(v_velo > vhi, vhi, v_velo)) %>%
  dplyr::select(-ulow,-uhi,-vlow,-vhi)

# gradient plot with ggquiver
ggrad = ggplot(df) +
  geom_quiver(aes(x, y, u = gradNS, v = gradWE)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Gradient")

# velocity plot
gvocc = ggplot(df) +
  geom_quiver(aes(x, y, u = u_velo, v = v_velo)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Velocity")

gridExtra::grid.arrange(gtrend, ggrad, gvocc, nrow=1)
