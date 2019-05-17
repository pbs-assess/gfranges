#' Make VOCC vector data from sdmTMB output predict funciton
#'
#' @param climate_data List of dataframes each containing output from predict.
#' @param ssid Vector of ssid to be included.
#' @param start_year Starting year for VOCC calculation.
#' @param input_cell_size Cell size of predict grid.
#' @param scale_fac Factor by which cells are aggregated for VOCC.
#' @param delta_t_total Mean time between start and end climate values.
#' @param delta_t_step Time between each time step in same units as above.
#' @param time_steps Number of time steps.
#' @param indices Vector of length equal to number of time steps,
#'    where 1 = starting time step(s), 2 = middle time step(s) not included in VOCC calc, 3 = end time step(s).
#' @param thresholds
#'
#' @export
make_vector_data <- function(climate_data,
                             ssid = NULL,
                             start_year = NULL,
                             input_cell_size = 2,
                             scale_fac = 2,
                             time_var = "year",
                             delta_t_total = 10,
                             delta_t_step = 2,
                             time_steps = 7,
                             indices = c(1, 1, 2, 2, 2, 3, 3),
                             thresholds = c(0.75)) {
  if (!is.null(ssid)) climate_data <- climate_data[climate_data$ssid %in% ssid, ]
  if (!is.null(start_year)) climate_data <- climate_data %>% dplyr::filter(year >= start_year)

  # if (!identical(time_steps, length(indices)))
  length_indices <- length(indices)
  if (!isTRUE(time_steps == length_indices)) {
    stop("Must have an indice assigned to each time step,",
      "therefore length('indices') must equal 'time_steps'.",
      call. = FALSE
    )
  }

  # FIXME: need way of determining how many variables(in separate dataframes) are in dataset
  # if (!is.list(climate_data)) {
  # if (length(thresholds) > 1)
  #       stop("If multiple climate variables (and corresponding thresholds),",
  #         "data must be in list form.", call. = FALSE)

  rbrick <- make_raster_brick(climate_data, time_var = time_var, scale_fac = scale_fac)
  mnraster_brick <- raster::stackApply(rbrick, indices = indices, fun = mean)
  start_raster <- mnraster_brick[[1]]
  end_raster <- mnraster_brick[[3]]

  # make sparate named lists containing climate rasters or dataframes
  # data with just one climate variable
  start_data <- list(var_1 = start_raster)
  end_data <- list(var_1 = end_raster)

  # } else {
  #     if (!identical(length(climate_data), length(thresholds)))
  #       stop("Must have a 'thresholds' value for each climate variable", call. = FALSE)
  # rbrick <- list()
  # for (i in seq_len(climate_data)) {
  #
  # rbrick[[i]] <- make_raster_brick(climate_data[[i]], scale_fac = scale_fac)
  # mnraster_brick <- raster::stackApply(rbrick[[i]], indices = indices, fun = mean)
  #
  # start_raster <- mnraster_brick[[1]]
  # end_raster <- mnraster_brick[[3]]
  # var <- list(start_raster = start_raster, end_raster = end_raster)
  # }

  # start_data <- list(var = var$start_raster[[i]])
  # end_data <- list(var = var$end_raster[[i]])
  # }

  out <- dist_based_vocc(
    start_data = start_data,
    end_data = end_data,
    x = "x",
    y = "y",
    variable_names = c("index_1"), # what the layer within each element is called
    thresholds = thresholds,
    cell_size = input_cell_size * scale_fac,
    delta_t = delta_t_total,
    raster = TRUE
  )

  # FIXME: need to change function to deal with different time steps within a brick
  slopedat <- calcslope(rbrick, delta_t_step = delta_t_step) # vocc::calcslope for comparison
  out <- left_join(out, slopedat, by = c("x", "y")) %>% select(-icell)
  out$C_per_decade <- out$slope * 10
  out$km_per_decade <- (out$distance / delta_t_total) * 10
  out
}
