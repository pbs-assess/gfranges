#' Make VOCC vector data from sdmTMB output predict funciton
#'
#' @param data List of dataframes each containing output from predict.
#' @param ssid Vector of ssid to be included.
#' @param start_time Starting time for VOCC calculation. Default is NULL and all data is used.
#' @param end_time End time for VOCC calculation. Default is NULL and all data is used.
#' @param skip_time Time steps to be excluded. Default is NULL and all data is used.
#' @param time_var Name of column containing time data.
#' @param input_cell_size Cell size of predict grid.
#' @param scale_fac Factor by which cells are aggregated for VOCC.
#' @param delta_t_total Mean time between start and end values.
#' @param delta_t_step Time between each time step in same units as above.
#' @param indices Vector of length equal to number of time steps retained for analysis,
#'  where 1 = starting time step(s), 2 = end time step(s).
#' @param variable_names Name(s) of column containing parameter(s).
#' @param round_fact Speed up searches by rounding (1 = integers; 10 = 10ths; 100 = 100ths).
#'  If NULL, will use 10x precision of the plus_minus threshold when `match_logic` = NULL.
#' @param min_thresholds Optional vector of negative thresholds.
#'  Include if sensitivity to the direction of climate change is not symmetrical and `match_logic` = NULL.
#' @param max_thresholds Optional vector of positive thresholds.
#'  Include if sensitivity to the direction of climate change is not symmetrical and `match_logic` = NULL.
#' @param plus_minus Vector of plus/minus threshold(s) to define a symmetical (default = 1 unit) climate match.
#' @param match_logic An optional vector of logical functions applied using 'rounding' of allnclimate values.
#'  If max_thresholds are not provided, the default of 'NULL' applies symmetrical plus/minus thresholds.
#'
#' @importFrom rlang .data
#'
#' @export
make_vector_data <- function(data,
                             ssid = NULL,
                             start_time = NULL,
                             end_time = NULL,
                             skip_time = NULL,
                             input_cell_size = 2,
                             scale_fac = 1,
                             time_var = "year",
                             delta_t_total = 10,
                             delta_t_step = 2,
                             indices = c(1, 2),
                             variable_names = "est",
                             round_fact = NULL, # NULL applies 10th of the plus/minus threshold
                             min_thresholds = NULL, # c(1) and round_fact = 10 to apply 1 unit thresholds
                             max_thresholds = NULL, # c(1,1) to apply symmetrical 1 unit thresholds
                             plus_minus = c(0.5), # default symmetrical thresholds of 1 unit
                             match_logic = NULL) {
  var_number <- length(variable_names)
  # browser()
  # if (!identical(time_steps, length(indices)))
  length_indices <- length(indices)


  if (isTRUE(var_number == 1)) {
    if (!is.null(ssid)) data <- data[data$ssid %in% ssid, ]
    if (!is.null(start_time)) data <- data %>% dplyr::filter(.data[[time_var]] >= start_time)
    if (!is.null(end_time)) data <- data %>% dplyr::filter(.data[[time_var]] <= end_time)
    if (!is.null(skip_time)) data <- data %>% dplyr::filter(!.data[[time_var]] %in% skip_time)

#browser()

    length_time_steps <- length(unique(data[[time_var]]))

    if (!isTRUE(length_time_steps == length_indices)) {
      stop("Must have an indice assigned to each unique time step retained in analysis.",
        call. = FALSE
      )
    }

    parameter <- variable_names
    rbrick <- make_raster_brick(data, parameter = parameter, time_var = time_var, scale_fac = scale_fac)

    if (isTRUE(length_indices > 2)) {
      mnraster_brick <- raster::stackApply(rbrick, indices = indices, fun = mean)
      start_raster <- mnraster_brick[[1]]
      end_raster <- mnraster_brick[[2]]
    } else {
      start_raster <- rbrick[[1]]
      end_raster <- rbrick[[2]]
    }
    # make sparate named lists containing climate rasters or dataframes
    # data with just one climate variable
    start_data <- list(var_1 = start_raster)
    end_data <- list(var_1 = end_raster)


    # FIXME: need to change function to deal with different time steps within a brick
    slopedat <- calcslope(rbrick, delta_t_step = delta_t_step) # vocc::calcslope for comparison
    slopedat$units_per_decade <- slopedat$slope * 10
  } else {

    # check that data list is equal in length to varaible_names vector
    if (!identical(length(variable_names), length((data)))) {
      stop(
        "Need list of dataframes (or rasters) with a dataframe (or raster) for each variable. 
        If multiple variables are found in one dataframe, it can be duplicated as is."
      )
    }

    start_raster <- list()
    end_raster <- list()
    start_data <- list()
    end_data <- list()
    slopedat <- list()

    for (i in seq_len(var_number)) {
      d <- data[[i]]
      parameter <- variable_names[[i]]

      if (!is.null(ssid)) d <- d[d$ssid %in% ssid, ]
      if (!is.null(start_time)) d <- d %>% dplyr::filter(.data[[time_var]] >= start_time) # .data uses rlang
      if (!is.null(end_time)) d <- d %>% dplyr::filter(.data[[time_var]] <= end_time)
      if (!is.null(skip_time)) d <- d %>% dplyr::filter(.data[[time_var]] != skip_time)
      length_time_steps <- length(unique(d[[time_var]]))
      
      if (!isTRUE(length_time_steps == length_indices)) {
        stop("Must have an indice assigned to each unique time step retained in analysis.",
          call. = FALSE
        )
      }
      
      rbrick <- make_raster_brick(d,
        parameter = parameter, time_var = time_var, scale_fac = scale_fac
      )

      slopedat[[i]] <- calcslope(rbrick, delta_t_step = delta_t_step) # vocc::calcslope for comparison
      x <- slopedat[[i]]$x
      y <- slopedat[[i]]$y
      icell <- slopedat[[i]]$icell ####
      slopedat[[i]]$units_per_decade <- slopedat[[i]]$slope * 10

      if (isTRUE(length_indices > 2)) {
        mnraster_brick <- raster::stackApply(rbrick, indices = indices, fun = mean)
        start_raster[[i]] <- mnraster_brick[[1]]
        end_raster[[i]] <- mnraster_brick[[2]]
      } else {
        start_raster[[i]] <- rbrick[[1]]
        end_raster[[i]] <- rbrick[[2]]
      }

      start_data[[i]] <- start_raster[[i]]
      names(start_data[[i]]) <- paste0("var_", i, "")
      end_data[[i]] <- end_raster[[i]]
      names(end_data[[i]]) <- paste0("var_", i, "")
    }

    names(slopedat) <- variable_names
    slopedat <- as.data.frame(slopedat)
    slopedat$x <- x
    slopedat$y <- y
    slopedat$icell <- icell ####
  }
  out <- dist_based_vocc(
    start_data = start_data,
    end_data = end_data,
    x = "x",
    y = "y",
    variable_names = c(rep("index_1", var_number)), # what the layer within each element is called
    round_fact = round_fact,
    min_thresholds = min_thresholds,
    max_thresholds = max_thresholds,
    plus_minus = plus_minus,
    match_logic = match_logic,
    cell_size = input_cell_size * scale_fac,
    delta_t = delta_t_total,
    raster = TRUE
  )

  out <- left_join(out, slopedat, by = c("x", "y"))
  out$km_per_decade <- (out$distance / delta_t_total) * 10
  out$start_time <- min(data[[time_var]])
  out$timespan <- delta_t_total
  out
}
