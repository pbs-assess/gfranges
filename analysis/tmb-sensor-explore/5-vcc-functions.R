
# devtools::install_github("seananderson/vocc")
# install.package(ggnewscale)
# install.package(gfplot)

library(dplyr)
library(ggplot2)
library(sdmTMB)

# all_sensor <- gfplot::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
# # saveRDS(all_sensor, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl.rds")

# all_sensor <- readRDS("analysis/tmb-sensor-explore/data/dat-sensor-trawl-processed.rds")
# glimpse(all_sensor)
#
# # replace obviously faulty depth data with NA
# all_sensor$depth_m[all_sensor$depth_m < 10] <- NA
#
# all_depth <- all_sensor %>%
#   # filter missing location data and trial year
#   dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
#
#   # convert lat and lon to UTMs
#   dplyr::mutate(X = longitude, Y = latitude) %>%
#   gfplot:::ll2utm(., utm_zone = 9) %>%
#
#   # interpolate missing depth values
#   dplyr::rename(depth = depth_m) %>%
#   gfplot:::interp_survey_bathymetry()

# # saveRDS(all_depth, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl-all-depth.rds")
all_depth <- readRDS("analysis/tmb-sensor-explore/data/dat-sensor-trawl-all-depth.rds")

# add and scale predictors after filtering for survey(s) of interest
data <- all_depth$data %>%
  dplyr::filter(year > 2003) %>%
  # filter(ssid != 1) %>%
  # filter(ssid != 3) %>%
  # filter(ssid != 4) %>%
  # filter(ssid != 16) %>%
  gfplot:::scale_survey_predictors()

# remove obvious sensor fail, d_trawl %>% filter(ssid==4, temperature_c >12), fishing event id = 481861
data <- data[data$fishing_event_id != 481861, ]
# add any new predictors
# data <- data %>%
#   dplyr::mutate(
#   DOY = lubridate::yday(date),
#   shallow = ifelse(depth > 35, 0, 1)
# )


# create prediction grid for each year
spatiotemporal_grid <- function(data, ssid = NULL, survey_abbrev = NULL, dummy_year) {
  if (ssid) {
    dat <- data[data$ssid == ssid, ]
    grid_locs <- gfplot:::make_prediction_grid(
      filter(dat, year %in% dummy_year),
      survey = survey_abbrev,
      cell_width = 2
    )$grid
  } else {
    # FIXME: Error ... object 'shape_utm' not found
    # grid_locs <- gfplot:::make_prediction_grid(
    #   filter(dat, year %in% dummy_year),
    #   cell_width = 2
    # )$grid
    #
  }
  grid_locs <- dplyr::rename(grid_locs, depth = akima_depth)
  grid_locs$year <- NULL

  # Expand the prediction grid to create a slice for each time:
  original_time <- sort(unique(dat$year))
  nd <- do.call(
    "rbind",
    replicate(length(original_time), grid_locs, simplify = FALSE)
  )
  nd[["year"]] <- rep(original_time, each = nrow(grid_locs))
  nd[["ssid"]] <- ssid
  nd
}

# choose base year(s) to create grid from
dummy_year <- c(2004, 2005)

# create grids for each ssid separately so that only years with data are included
ssid <- 1
survey_abbrev <- "SYN QCS"
nd_1 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)
unique(nd_1$year)

ssid <- 3
survey_abbrev <- "SYN HS"
nd_3 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)

ssid <- 4
survey_abbrev <- "SYN WCVI"
nd_4 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)

ssid <- 16
survey_abbrev <- "SYN WCHG"
nd_16 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year = 2006)

nd <- rbind(nd_1, nd_3, nd_4, nd_16)
nd <- nd %>% dplyr::mutate(shallow = ifelse(depth > 35, 0, 1))



# choose the spatial range to model
dat <- data # %>% filter(ssid == 4)
nd <- nd # %>% filter(ssid == 4)

spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = 500)
sdmTMB::plot_spde(spde)

m_temp <- sdmTMB::sdmTMB(dat,
  temperature_c ~ 0 + as.factor(year),
  time_varying = ~ 0 + depth_scaled + depth_scaled2,
  time = "year", spde = spde,
  family = gaussian(link = "identity"),
  ar1_fields = TRUE, # maybe TRUE is better for all areas combined?
  include_spatial = TRUE,
  silent = FALSE
)

stopifnot(m_temp$model$convergence == 0L)
m_temp
# Warning messages:
#   1: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation

saveRDS(m_temp, file = "analysis/tmb-sensor-explore/data/m_temp_allpost2003.rds")
m_temp <- readRDS("analysis/tmb-sensor-explore/data/m_temp_allpost2003.rds")

predictions <- predict(m_temp, newdata = nd)

plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed()
}

p <- plot_map(predictions , "est") +
  scale_fill_viridis_c(trans = "sqrt", option = "C") +
  ggtitle("Prediction (fixed effects + all random effects)")
print(p)
# ggsave(paste0("analysis/tmb-sensor-explore/", survey_abbrev, "-temp.pdf"))




# Velocity of climate change

# create a RasterBrick from gridded predictions
# 'scale_fac' controls how the original 2-km projection is aggregated.
# for example, a value of 5 means that the raster would be reprojected to 10km grid

make_raster_brick <- function(data,
                              scale_fac = 1,
                              time_step = "year") {
  d <- data[order(data[[time_step]]), ]
  time_vec <- d[[time_step]]

  # raster for each time_step
  rlist <- list()
  for (i in 1:length(unique(d[[time_step]]))) {
    # browser()
    rlist[[i]] <- raster::rasterFromXYZ(d[time_vec == unique(d[[time_step]])[i], ] %>%
      dplyr::select(X, Y, est))
    rlist[[i]] <- raster::aggregate(rlist[[i]], fact = scale_fac)
  }

  # stack rasters into layers -> rasterbrick
  rstack <- raster::stack(rlist[[1]], rlist[[2]])
  for (i in 3:length(rlist)) {
    rstack <- raster::stack(rstack, rlist[[i]])
  }
  rbrick <- raster::brick(rstack)
  rbrick
}


vocc_calc <- function(data,
                      scale_fac = 1,
                      time_step = "year",
                      utm = TRUE) {
  
  # make raster brick
  rbrick <- make_raster_brick(data,
    scale_fac = scale_fac,
    time_step = time_step
  )

  # Then calculate the trend per pixel:
  slopedat <- vocc::calcslope(rbrick)

  ##### Then get the mean temperature for a time period? #####

  # # Old code:
  # # calculates mean temp across all time slices for each grid cell
  # alltime_steps <- rep(1, raster::nlayers(rbrick))
  # mnsst <- raster::stackApply(rbrick, indices = alltime_steps, fun = mean)
  # # library(rgdal)
  # # library(raster)
  # # plot(mnsst)

  # Alternate code:
  # uses spatial gradient in most recent time slice
  mnsst <- rbrick[[raster::nlayers(rbrick)]]
  # browser()
  # Calculate the spatial gradient:
  # must use y_dist = res(rx) if data is in UTMs

  if (utm) {
    spatx <- vocc::spatialgrad(mnsst, y_dist = raster::res(mnsst), y_diff = NA)
  } else {
    spatx <- vocc::spatialgrad(mnsst)
  }

  # Now we can calculate the VoCC:
  velodf <- vocc::calcvelocity(spatx, slopedat)
  #browser()

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
  df
}



# PLOT VECTORS
plot_vocc <- function(df,
                      vec_col = "C_per_decade",
                      col_label = "Local\nclimate trend\n(°C/decade)",
                      coast = NULL,
                      isobath = NULL) {
  
  colour <- df[[vec_col]]

  gvocc <- ggplot(df) +
    ggquiver::geom_quiver(aes(x, y,
      u = u_velo, v = v_velo,
      colour = colour
    ),
    vecsize = 2
    ) +
    scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
    xlab("UTM") + ylab("UTM") +
    labs(colour = col_label) +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs()

  if (!is.null(isobath)) {
    gvocc <- gvocc +
      ggnewscale::new_scale_color() +
      geom_path(
        data = isobath,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)", colour = "PID"
        ),
        inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      ) +
      scale_colour_continuous(low = "grey70", high = "grey10") +
      guides(colour = FALSE)
  } else {
    try({df <- df %>%
      dplyr::mutate(X = x, Y = y) %>%
      gfplot:::utm2ll(., utm_zone = 9)
    
    # creates utm bathymetry lines for area defined in lat lon
    isobath <- gfplot:::load_isobath(
      range(df$X) + c(-5, 5),
      range(df$Y) + c(-5, 5),
      bath = c(100, 200, 300, 500),
      utm_zone = 9
    )
    
    gvocc <- gvocc +
      ggnewscale::new_scale_color() +
      geom_path(
        data = isobath,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)", colour = "PID"
        ),
        inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      ) +
      scale_colour_continuous(low = "grey70", high = "grey10") +
      guides(colour = FALSE)
    gvocc
    }, silent = TRUE )
  }

  if (!is.null(coast)) {
    gvocc <- gvocc +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
  } else {
    try({df <- df %>%
      dplyr::mutate(X = x, Y = y) %>%
      gfplot:::utm2ll(., utm_zone = 9)
    
    # creates coast lines for area defined in lat lon
    coast <- gfplot:::load_coastline(
      range(df$X) + c(-1, 1),
      range(df$Y) + c(-1, 1),
      utm_zone = 9
    )
    gvocc <- gvocc +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
    gvocc}, silent = TRUE)
  }
  gvocc
}


# PLOT OTHER VARIABLES
plot_var <- function(df,
                     variable = "C_per_decade",
                     var_label = "",
                     white_value = 0,
                     isobath = NULL,
                     coast = NULL) {
  
  variable <- df[[variable]]
 
  g <- ggplot(df, aes(x, y, fill = variable)) +
    geom_raster() +
    scale_fill_gradient2(low = scales::muted("blue"), midpoint = white_value, high = scales::muted("red")) +
    xlab("UTM") + ylab("UTM") +
    labs(fill = var_label) +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs()

  if (!is.null(isobath)) {
    g <- g +
      ggnewscale::new_scale_color() +
      geom_path(
        data = isobath,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)", colour = "PID"
        ),
        inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      ) +
      scale_colour_continuous(low = "grey70", high = "grey10") +
      guides(colour = FALSE)
  } else {
    try({df <- df %>%
      dplyr::mutate(X = x, Y = y) %>%
      gfplot:::utm2ll(., utm_zone = 9)
    
    # creates utm bathymetry lines for area defined in lat lon
    isobath <- gfplot:::load_isobath(
      range(df$X) + c(-5, 5),
      range(df$Y) + c(-5, 5),
      bath = c(100, 200, 300, 500),
      utm_zone = 9
    )
    
    g <- g +
      ggnewscale::new_scale_color() +
      geom_path(
        data = isobath,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)", colour = "PID"
        ),
        inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      ) +
      scale_colour_continuous(low = "grey70", high = "grey10") +
      guides(colour = FALSE)
    g
    }, silent = TRUE )
  }

  if (!is.null(coast)) {
    g <- g +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
  } else {
    try({df <- df %>%
      dplyr::mutate(X = x, Y = y) %>%
      gfplot:::utm2ll(., utm_zone = 9)
    
    # creates coast lines for area defined in lat lon
    coast <- gfplot:::load_coastline(
      range(df$X) + c(-1, 1),
      range(df$Y) + c(-1, 1),
      utm_zone = 9
    )
    g <- g +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
    g}, silent = TRUE)
  }
  g
}




# choose the spatial range to build raster on
predicted <- predictions %>% filter(ssid == 1)

# scale_fac = 3 means that the raster is reprojected to 3 X original grid (2 km)
rbrick <- make_raster_brick(predicted, scale_fac = 3)

df <- vocc_calc(predicted, scale_fac = 3)
glimpse(df)

df <- df %>%
  mutate(C_per_decade = -trend) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)


# creates coast lines for area defined in lat lon
coast <- gfplot:::load_coastline(
  range(df$X) + c(-1, 1),
  range(df$Y) + c(-1, 1),
  utm_zone = 9
)

# creates utm bathymetry lines for area defined in lat lon
isobath <- gfplot:::load_isobath(
  range(df$X) + c(-5, 5),
  range(df$Y) + c(-5, 5),
  bath = c(100, 200, 300, 500),
  utm_zone = 9
)

gvocc <- plot_vocc(df, vec_col = "C_per_decade", col_label = "Local\nClimate trend\n(C/decade)", coast = coast, isobath = isobath)
gvocc

# plot temperature estimates in most recent time slice
raster7 <- as.data.frame(raster::rasterToPoints(rbrick[[7]]))
names(raster7)[3] <- "est"
gcurrent <- plot_var(raster7, variable = "est", var_label = "Most recent\ntemperature", white_value = mean(raster7$est))

gridExtra::grid.arrange(gcurrent, gvocc, nrow = 1)

