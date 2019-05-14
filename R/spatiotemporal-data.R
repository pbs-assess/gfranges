#' Make raster brick from dataframe of predicted values through time 
#'
#' @param data 
#' @param scale_fac Controls how the original projection is aggregated.
#' @param time_step 
#'
#' @return
#' @export
#'
#' @examples
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


#' Create prediction grid for each year
#'
#' @param data 
#' @param ssid 
#' @param survey_abbrev 
#' @param dummy_year 
#'
#' @return
#' @export
#'
#' @examples
spatiotemporal_grid <- function(data, ssid = NULL, survey_abbrev = NULL, dummy_year, cell_width = 2) {
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