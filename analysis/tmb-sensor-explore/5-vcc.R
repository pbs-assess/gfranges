# d_trawl <- gfplot::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
# saveRDS(d_trawl, file = "analysis/tmb-sensor-explore/dat-sensor-trawl.rds")
d_trawl <- readRDS("analysis/tmb-sensor-explore/dat-sensor-trawl.rds")

# d_ll <- gfplot::get_sensor_data_ll_ctd(c(22, 36), sensor_min_max = TRUE)
# saveRDS(d_ll, file = "analysis/tmb-sensor-explore/dat-sensor-ll.rds")
# d_ll <- readRDS("analysis/tmb-sensor-explore/dat-sensor-ll.rds")

# surv <- gfplot::get_survey_sets(222, ssid = c(1, 3, 4, 16), join_sample_ids = TRUE)
# surv <- readRDS("../gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets
# saveRDS(surv, file = "analysis/tmb-sensor-explore/pacific-cod.rds")
surv <- readRDS("analysis/tmb-sensor-explore/pacific-cod.rds")

library(dplyr)
library(ggplot2)
library(sdmTMB)

ssid <- 4
survey_abbrev <- "SYN WCVI"

# ssid <- 1
# survey_abbrev <- "SYN QCS"

# ssid <- 3
# survey_abbrev <- "SYN HS"

# ssid <- 16
# survey_abbrev <- "SYN WCHG"

surv <- dplyr::filter(surv, survey_series_id == ssid)
d_trawl <- d_trawl %>%
  dplyr::select(-count, -start_time, -end_time, -min, -max) %>%
  dplyr::mutate(attribute = tolower(attribute)) %>%
  dplyr::distinct() %>%
  reshape2::dcast(fishing_event_id + year + ssid + survey_desc ~ attribute,
    value.var = "avg"
  )

surv_fish <- surv %>%
  dplyr::select(year, fishing_event_id, survey_series_id, longitude, latitude, density_kgpm2) %>%
  dplyr::distinct() %>%
  dplyr::rename(ssid = survey_series_id)

d_trawl <- left_join(surv_fish, d_trawl, 
  by = c("year", "fishing_event_id", "ssid"))
d_trawl2 <- dplyr::rename(d_trawl, X = longitude, Y = latitude)
d_trawl <- as_tibble(gfplot:::ll2utm(d_trawl2, utm_zone = 9))

d_fit <- d_trawl %>% dplyr::filter(ssid == ssid)
d_fit <- d_fit %>% dplyr::rename(depth = depth_m)
d_fit <- d_fit %>% dplyr::filter(!is.na(depth), depth > 0)
dat <- gfplot:::scale_survey_predictors(d_fit)

spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = 250)
sdmTMB::plot_spde(spde)
m_temp <- sdmTMB::sdmTMB(dat,
  temperature_c ~ 0 + as.factor(year) + poly(depth_scaled, 3),
  # time_varying = ~ 0 + poly(depth_scaled, 3),
  time = "year", spde = spde,
  family = gaussian(link = "identity"),
  ar1_fields = TRUE,
  include_spatial = FALSE,
  silent = FALSE
)

stopifnot(m_temp$model$convergence == 0L)
m_temp$model$message
r <- m_temp$tmb_obj$report()
r$range
2 * plogis(m_temp$model$par[['ar1_phi']]) - 1
if (ncol(r$b_rw_t) > 1) {
  r$b_rw_t
  exp(r$ln_tau_V)
  plot(r$b_rw_t[,1], type = "o")
  plot(r$b_rw_t[,2], type = "o")
}
exp(r$ln_phi)
r$b_j
r$sigma_E

##################
# this grid_locs not building for PE, WHY?
dummy_year <- c(2005, 2006)
grid_locs <- gfplot:::make_prediction_grid(filter(dat, year %in% dummy_year),
  survey = survey_abbrev, cell_width = 2
)$grid
grid_locs <- dplyr::rename(grid_locs, depth = akima_depth)
grid_locs$year <- NULL
#################


# Expand the prediction grid to create a slice for each time:
original_time <- sort(unique(dat$year))
nd <- do.call(
  "rbind",
  replicate(length(original_time), grid_locs, simplify = FALSE)
)
nd[["year"]] <- rep(original_time, each = nrow(grid_locs))

predictions <- predict(m_temp, newdata = nd)

plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed()
}

p <- plot_map(predictions$data, "est") +
  scale_fill_viridis_c(trans = "sqrt", option = "C") +
  ggtitle("Prediction (fixed effects + all random effects)")
print(p)
ggsave(paste0("analysis/tmb-sensor-explore/", survey_abbrev, "-temp.pdf"))

# Velocity of climate change ---------------------------------------------

d <- predictions$data

# Just to confirm the trends we're seeing are real,
# mod = mgcv::gam(est ~ s(year, k=5) + te(X,Y) + log(depth),
#   data=d)
# plot(mod)

# This parameter controls how the original 2-km projection is aggregated.
# for example, a value of 5 means that the raster would be reprojected to 10km grid
scale_fac <- 2

# create a RasterBrick
# raster for each year
rlist <- list()
for (i in 1:length(unique(d$year))) {
  rlist[[i]] <- raster::rasterFromXYZ(dplyr::filter(d, year == unique(d$year)[i]) %>%
    dplyr::select(X, Y, est))
  rlist[[i]] <- raster::aggregate(rlist[[i]], fact = scale_fac)
}
# stack rasters into layers -> rasterbrick
rstack <- raster::stack(rlist[[1]], rlist[[2]])
for (i in 3:length(rlist)) {
  rstack <- raster::stack(rstack, rlist[[i]])
}
rbrick <- raster::brick(rstack)

# Then calculate the trend per pixel:
slopedat <- vocc::calcslope(rbrick)
# Then get the mean temperature for a time period and calculate the spatial gradient:
allyears <- rep(1, raster::nlayers(rbrick))
mnsst <- raster::stackApply(rbrick, indices = allyears, fun = mean)
spatx <- vocc::spatialgrad(mnsst)
# Now we can calculate the VoCC:
velodf <- vocc::calcvelocity(spatx, slopedat)

# Mapping it again is straightforward:
rtrend <- rgrad_lon <- rgrad_lat <- rvocc <- angle <- magn <- raster::raster(rbrick)
rgrad_lat[spatx$icell] <- spatx$NS # latitude shift, NS
rgrad_lon[spatx$icell] <- spatx$WE # longitude shift, WE
rtrend[slopedat$icell] <- -1 * slopedat$slope
rvocc[velodf$icell] <- velodf$velocity

# convert to data frames for ggplot
rtrend_df <- as.data.frame(raster::rasterToPoints(rtrend)) %>%
  dplyr::rename(trend = layer)
rgradlat_df <- as.data.frame(raster::rasterToPoints(rgrad_lat)) %>%
  dplyr::rename(gradNS = layer)
rgradlon_df <- as.data.frame(raster::rasterToPoints(rgrad_lon)) %>%
  dplyr::rename(gradWE = layer)
rvocc_df <- as.data.frame(raster::rasterToPoints(rvocc)) %>%
  dplyr::rename(velocity = layer)

# create ggquiver plots. need dataframe of lon, lat, delta_lon, delta_lat, trend, velocity
df <- dplyr::left_join(rtrend_df, rgradlat_df, by = c("x", "y")) %>%
  dplyr::left_join(rgradlon_df, by = c("x", "y")) %>%
  dplyr::left_join(rvocc_df, by = c("x", "y"))

gtrend <- ggplot(df, aes(x, y, fill = -trend)) +
  geom_raster() + 
  scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Trend") + coord_fixed()

# spatial gradient plot
quantile_cutoff <- 0.05 # for plotting
df <- dplyr::mutate(df,
  u_velo = trend / gradWE,
  v_velo = trend / gradNS,
  ulow = quantile(u_velo, quantile_cutoff),
  uhi = quantile(u_velo, 1 - quantile_cutoff),
  vlow = quantile(v_velo, quantile_cutoff),
  vhi = quantile(v_velo, 1 - quantile_cutoff),
  u_velo = ifelse(u_velo < ulow, ulow, u_velo),
  u_velo = ifelse(u_velo > uhi, uhi, u_velo),
  v_velo = ifelse(v_velo < vlow, vlow, v_velo),
  v_velo = ifelse(v_velo > vhi, vhi, v_velo)
) %>%
  dplyr::select(-ulow, -uhi, -vlow, -vhi)

# gradient plot with ggquiver
ggrad <- ggplot(df) +
  ggquiver::geom_quiver(aes(x, y, u = gradNS, v = gradWE)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Gradient") + coord_fixed()

# velocity plot
gvocc <- ggplot(mutate(df, `Climate trend\n(C/decade)` = -trend)) +
  ggquiver::geom_quiver(aes(x, y, u = u_velo, v = v_velo, 
    colour = `Climate trend\n(C/decade)`), 
    vecsize = 2) +
  scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Velocity") + 
  gfplot::theme_pbs()

coast <- gfplot:::load_coastline(range(surv$longitude) + c(-1, 1),
  range(surv$latitude) + c(-1, 1),
  utm_zone = 9
)

isobath <- gfplot:::load_isobath(range(surv$longitude) + c(-5, 5),
  range(surv$latitude) + c(-5, 5),
  bath = c(100, 200, 300, 500), utm_zone = 9
)

# library(ggnewscale)

gvocc <- gvocc + 
  ggnewscale::new_scale_color() +
  geom_path(
  data = isobath, aes_string(
    x = "X", y = "Y",
    group = "paste(PID, SID)", colour = "PID"
  ),
  inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
) +
  scale_colour_continuous(low = "grey70", high = "grey10") +
  labs(colour = "Depth") +
  coord_fixed(xlim = range(df$x) + c(-2, 2), ylim = range(df$y) + c(-2, 2))

gvocc <- gvocc + geom_polygon(
  data = coast, aes_string(x = "X", y = "Y", group = "PID"),
  fill = "grey87", col = "grey70", lwd = 0.2
)

pdf(paste0("analysis/tmb-sensor-explore/", survey_abbrev, "-vcc.pdf"), width = 8, height = 6)
# gridExtra::grid.arrange(gtrend, gvocc, nrow = 1)
gridExtra::grid.arrange(gvocc, nrow = 1)
dev.off()
