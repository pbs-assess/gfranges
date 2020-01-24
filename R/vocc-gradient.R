#' Velocity of climate change
#'
#' @param data Gridded data.
#' @param layer Variable to retain. Defaults to "est", for raw sdmTMB predictions.
#' @param scale_fac Controls how the original 2-km projection is aggregated
#'    (e.g. a value of 5 means that the raster would be reprojected to 10km grid).
#' @param time_step Time variable.
#' @param grad_time_steps Define time periods.
#'    If NULL will use spatial gradient in last time period.
#' @param latlon Default is TRUE. False for UTMs or other true distance grid.
#' @param quantile_cutoff Used for plotting to set min and max angles of vectors.
#'    Defaults to 0.05.
#'
#' @export
#'
vocc_gradient_calc <- function(data,
                               layer,
                               scale_fac = 1,
                               time_step = "year",
                               indices = NULL,
                               divisor = 10,
                               latlon = FALSE,
                               log_space = FALSE,
                               quantile_cutoff = 0.05) {

  # # devtools::install_github("seananderson/vocc")
  # # install.package(ggnewscale)
  # # install.package(gfplot)

  # make raster brick
  rbrick <- make_gradient_brick(data, layer,
    scale_fac = scale_fac,
    time_step = time_step
  )
  
  if(log_space) {
  data$est_exp <- exp(data[[layer]])
  
  rbrick_exp <- make_gradient_brick(data, "est_exp",
    scale_fac = scale_fac,
    time_step = time_step
  )
  sdraster <- raster::calc(rbrick_exp, sd)
  cvraster <- raster::calc(rbrick, sd)
  
  } else {
    
  sdraster <- raster::calc(rbrick, sd)
  cvraster <- raster::calc(rbrick, sd)
  
  }
  
  
  # Then calculate the trend per pixel:
  slopedat <- vocc::calcslope(rbrick, divisor = divisor)
  
  # Then get the mean values for a time period
  if (!is.null(indices)) {
    # if (indices = "all") {
    #   # calculates mean temp across all time slices for each grid cell
    #   indices <- rep(1, raster::nlayers(rbrick))
    #  mnraster_brick <- raster::stackApply(rbrick, indices = indices, fun = mean)
    #  mnraster <-mnraster_brick[[raster::nlayers(mnraster_brick)]]
    #   } else {
    mnraster_brick <- raster::stackApply(rbrick, indices = indices, fun = mean)
    # use first time period
    # mnraster <- mnraster_brick[[1]]
    # to use last time period
    gradraster <- mnraster_brick[[raster::nlayers(mnraster_brick)]]
    mnraster <- raster::calc(rbrick, mean)
  } else {
    # uses average spatial gradient
    mnraster <- raster::calc(rbrick, mean)
    gradraster <- mnraster
  }
  # # library(rgdal)
  # # library(raster)
  # # plot(mnraster)

  # Calculate the spatial gradient for chosen time period:
  if (latlon) {
    spatx <- vocc::spatialgrad(gradraster)
  } else {
    # must use y_dist = res(rx) if data is in UTMs or other true distance grid
    spatx <- vocc::spatialgrad(gradraster, y_dist = raster::res(gradraster), y_diff = NA)
  }

  # Now we can calculate the VoCC:
  velodf <- vocc::calcvelocity(spatx, slopedat, y_dist = 1)

  # Mapping it again is straightforward:
  rtrend <- rgrad_lon <- rgrad_lat <- rvocc <- rgrad <- angle <- magn <- raster::raster(rbrick)
  rgrad_lat[spatx$icell] <- spatx$NS # latitude shift, NS
  rgrad_lon[spatx$icell] <- spatx$WE # longitude shift, WE
  rtrend[slopedat$icell] <- slopedat$slope # why was this multiplied by -1?
  rvocc[velodf$icell] <- velodf$velocity
  rgrad[velodf$icell] <- velodf$spatial_gradient

  # convert to data frames for ggplot
  rtrend_df <- as.data.frame(raster::rasterToPoints(rtrend)) %>%
    dplyr::rename(trend = layer)
  rmnvalues_df <- as.data.frame(raster::rasterToPoints(mnraster))
  names(rmnvalues_df)[3] <- "mean"
  rsdvalues_df <- as.data.frame(raster::rasterToPoints(sdraster))
  names(rsdvalues_df)[3] <- "sd"
  rcvvalues_df <- as.data.frame(raster::rasterToPoints(cvraster))
  names(rcvvalues_df)[3] <- "cv"
  rgradlat_df <- as.data.frame(raster::rasterToPoints(rgrad_lat)) %>%
    dplyr::rename(gradNS = layer)
  rgradlon_df <- as.data.frame(raster::rasterToPoints(rgrad_lon)) %>%
    dplyr::rename(gradWE = layer)
  rvocc_df <- as.data.frame(raster::rasterToPoints(rvocc)) %>%
    dplyr::rename(velocity = layer)
  rgrad_df <- as.data.frame(raster::rasterToPoints(rgrad)) %>%
    dplyr::rename(gradient = layer)

  # create ggquiver plots. need dataframe of lon, lat, delta_lon, delta_lat, trend, velocity
  df <- dplyr::left_join(rtrend_df, rmnvalues_df, by = c("x", "y")) %>%
    dplyr::left_join(rsdvalues_df, by = c("x", "y")) %>%
    dplyr::left_join(rcvvalues_df, by = c("x", "y")) %>%
    dplyr::left_join(rgradlat_df, by = c("x", "y")) %>%
    dplyr::left_join(rgradlon_df, by = c("x", "y")) %>%
    dplyr::left_join(rvocc_df, by = c("x", "y")) %>%
    dplyr::left_join(rgrad_df, by = c("x", "y"))
#browser()
  # spatial gradient plot
  df <- dplyr::mutate(df,
    #CV = sd/mean,
    u_velo = trend / gradWE,
    v_velo = trend / gradNS,
    ulow = quantile(u_velo, quantile_cutoff, na.rm = TRUE),
    uhi = quantile(u_velo, 1 - quantile_cutoff, na.rm = TRUE),
    vlow = quantile(v_velo, quantile_cutoff, na.rm = TRUE),
    vhi = quantile(v_velo, 1 - quantile_cutoff, na.rm = TRUE),
    u_velo = ifelse(u_velo < ulow, ulow, u_velo),
    u_velo = ifelse(u_velo > uhi, uhi, u_velo),
    v_velo = ifelse(v_velo < vlow, vlow, v_velo),
    v_velo = ifelse(v_velo > vhi, vhi, v_velo)
  ) %>%
    dplyr::select(-ulow, -uhi, -vlow, -vhi)
  
  if(log_space) { df } else { df <- df %>% select(-cv) }
  
}

#' Create a RasterBrick from gridded predictions
make_gradient_brick <- function(data,
                                layer,
                                scale_fac = 1,
                                time_step = "year") {
  d <- data[order(data[[time_step]]), ]
  time_vec <- d[[time_step]]
  d$var <- d[[layer]]

  # raster for each time_step
  rlist <- list()
  for (i in 1:length(unique(d[[time_step]]))) {
    rlist[[i]] <- raster::rasterFromXYZ(d[time_vec == unique(d[[time_step]])[i], ] %>%
      dplyr::select(X, Y, var))
    rlist[[i]] <- raster::aggregate(rlist[[i]], fact = scale_fac)
  }

  # stack rasters into layers -> rasterbrick
  rstack <- raster::stack(rlist[[1]], rlist[[2]])
  if (length(rlist) > 2) {
    for (i in 3:length(rlist)) {
      rstack <- raster::stack(rstack, rlist[[i]])
    }
  }
  rbrick <- raster::brick(rstack)
  rbrick
}

