#' Make raster brick from dataframe of predicted values through time
#'
#' @param data Dataframe of spatial grid for multiple time slices.
#' @param parameter Name of column with raster values.
#' @param time_var Name of column with time variable.
#' @param scale_fac Controls how the original projection is aggregated.
#'
#' @export
#'
make_raster_brick <- function(data,
                              parameter = "est",
                              scale_fac = 1,
                              time_var = "year") {
  d <- data[order(data[[time_var]]), ]
  time_vec <- d[[time_var]]
  time_steps <- length(unique(d[[time_var]]))

  # raster for each unique value of time_var
  rlist <- list()
  for (i in 1:length(unique(d[[time_var]]))) {
    # browser()
    rlist[[i]] <- raster::rasterFromXYZ(d[time_vec == unique(d[[time_var]])[i], ] %>%
      dplyr::select(X, Y, parameter))
    if (isTRUE(scale_fac > 1)) {
      rlist[[i]] <- raster::aggregate(rlist[[i]], fact = scale_fac)
    }
  }

  # stack rasters into layers -> rasterbrick
  rstack <- raster::stack(rlist[[1]], rlist[[2]])
  if (isTRUE(time_steps > 2)) {
    for (i in 3:length(rlist)) {
      rstack <- raster::stack(rstack, rlist[[i]])
    }
  }
  rbrick <- raster::brick(rstack)
  rbrick
}


#' Create prediction grid for each year
#'
#' @param data Original dataframe of spatiotemporal data.
#' @param ssid Survey series id if using established survey grid.
#' @param survey_abbrev Survey series abbreviation if using established survey grid.
#' @param dummy_year Single year on which to base the survey grid.
#'
#' @export
#'
spatiotemporal_grid <- function(data,
                                ssid = NULL,
                                survey_abbrev = NULL,
                                dummy_year,
                                cell_width = 2) {
  if (ssid) {
    dat <- data[data$ssid == ssid, ]
    grid_locs <- gfplot:::make_prediction_grid(
      filter(dat, year %in% dummy_year),
      survey = survey_abbrev,
      cell_width = cell_width
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
