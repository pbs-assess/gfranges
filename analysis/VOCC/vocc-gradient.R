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
                               quantile_cutoff = 0.05) {

  # # devtools::install_github("seananderson/vocc")
  # # install.package(ggnewscale)
  # # install.package(gfplot)

  # make raster brick
  rbrick <- make_gradient_brick(data, layer,
    scale_fac = scale_fac,
    time_step = time_step
  )

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
    mnraster <- mnraster_brick[[raster::nlayers(mnraster_brick)]]
  } else {
    # uses average spatial gradient
    mnraster <- raster::calc(rbrick, mean)
  }
  # # library(rgdal)
  # # library(raster)
  # # plot(mnraster)

  # Calculate the spatial gradient for chosen time period:
  if (latlon) {
    spatx <- vocc::spatialgrad(mnraster)
  } else {
    # must use y_dist = res(rx) if data is in UTMs or other true distance grid
    spatx <- vocc::spatialgrad(mnraster, y_dist = raster::res(mnraster), y_diff = NA)
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
    dplyr::left_join(rgradlat_df, by = c("x", "y")) %>%
    dplyr::left_join(rgradlon_df, by = c("x", "y")) %>%
    dplyr::left_join(rvocc_df, by = c("x", "y")) %>%
    dplyr::left_join(rgrad_df, by = c("x", "y"))

  # spatial gradient plot
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


#' Title
#'
#' @param df
#' @param vec_col
#' @param col_label
#' @param vecsize
#' @param lwd
#' @param low_col
#' @param high_col
#' @param mid_col
#' @param coast
#' @param isobath
#' @param raster
#'
#' @export
plot_gradient_vocc <- function(df,
                               vec_col = "C_per_decade",
                               col_label = "Local\nclimate trend\n(°C/decade)",
                               vecsize = 1,
                               lwd = 1,
                               low_col = scales::muted("blue"),
                               high_col = scales::muted("red"),
                               mid_col = "white",
                               raster = NULL,
                               fill_label = raster,
                               coast = NULL,
                               isobath = NULL) {
  colour <- df[[vec_col]]
  fill <- df[[raster]]


  gvocc <- ggplot(df) +
    xlab("UTM") + ylab("UTM") +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs()


  if (!is.null(raster)) {
    gvocc <- gvocc +
      geom_raster(aes(x, y, fill = fill), inherit.aes = FALSE) +
      scale_fill_gradient2(low = low_col, high = high_col, mid = "white") +
      # scale_fill_viridis_c() +
      labs(fill = fill_label)
  }

  if (!is.null(coast)) {
    gvocc <- gvocc +
      # ggnewscale::new_scale_fill() +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
  } else {
    try({
      df <- df %>%
        dplyr::mutate(X = x, Y = y) %>%
        gfplot:::utm2ll(., utm_zone = 9)

      # creates coast lines for area defined in lat lon
      coast <- gfplot:::load_coastline(
        range(df$X) + c(-1, 1),
        range(df$Y) + c(-1, 1),
        utm_zone = 9
      )
      gvocc <- gvocc +
        # ggnewscale::new_scale_fill() +
        geom_polygon(
          data = coast, aes_string(x = "X", y = "Y", group = "PID"),
          fill = "grey87", col = "grey70", lwd = 0.2
        )
      gvocc
    }, silent = TRUE)
  }


  gvocc <- gvocc +
    ggquiver::geom_quiver(aes(x, y,
      u = u_velo, v = v_velo,
      colour = colour
    ),
    vecsize = vecsize,
    lwd = lwd
    ) +
    # ggnewscale::new_scale_color() +
    scale_colour_gradient2(
      trans = fourth_root_power,
      low = low_col, high = high_col, mid = mid_col
    ) +
    labs(colour = col_label)

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
      scale_colour_continuous(low = "grey80", high = "grey10") +
      guides(colour = FALSE)
  } else {
    try({
      df <- df %>%
        dplyr::mutate(X = x, Y = y) %>%
        gfplot:::utm2ll(., utm_zone = 9)

      # creates utm bathymetry lines for area defined in lat lon
      isobath <- gfplot:::load_isobath(
        range(df$X) + c(-5, 5),
        range(df$Y) + c(-5, 5),
        bath = c(100, 200, 300, 400, 500),
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
        scale_colour_continuous(low = "grey80", high = "grey10") +
        guides(colour = FALSE)
      gvocc
    }, silent = TRUE)
  }


  gvocc
}
