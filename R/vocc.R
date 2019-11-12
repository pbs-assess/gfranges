#' Velocity of climate change based on distance
#'
#' @param start_data Named list containing starting climate data.
#' @param end_data Named list containing target climate data.
#' @param x Numeric vector of x coordinates.
#' @param y Numeric vector of y coordinates.
#' @param variable_names Vector of column/layer names within each data element.
#' @param round_fact Speed up searches by rounding (1 = integers; 10 = 10ths; 100 = 100ths).
#'  If NULL, will use 10x precision of the plus_minus threshold when `match_logic` = NULL.
#' @param min_thresholds Optional vector of negative thresholds.
#'  Include if sensitivity to climate is not symmetrical and `match_logic` = NULL.
#' @param max_thresholds Optional vector of positive thresholds.
#'  Include if sensitivity to climate is not symmetrical and `match_logic` = NULL.
#' @param plus_minus Vector of plus/minus threshold(s) to define a symmetical (default = 1 unit) climate match.
#' @param match_logic An optional vector of logical functions applied using 'rounding' of allnclimate values.
#'  If max_thresholds are not provided, the default of 'NULL' applies symmetrical plus/minus thresholds.
#' @param cell_size Cell size in raster or distance to nearest data point.
#' @param max_dist Distance reported when no analogue found. Default will
#'  estimate this assuming that both major axes are likely to hold an analogue.
#' @param min_dist Defaults to half cell size. 
#' @param delta_t Time difference between starting and target time periods.
#' @param raster Logical for whether climate data is in raster form.
#' @param kNN Logical for whether to use kNN search for extra speed
#'  (but uses rounding, symmetrical thresholds, and only returns 1 target cell per source).

#'
#' @export
#'
dist_based_vocc <- function(start_data,
                            end_data,
                            x = "x",
                            y = "y",
                            variable_names = c("var.1", "var.2"),
                            round_fact = NULL, # NULL applies 10th of the plus/minus threshold
                            min_thresholds = NULL, # c(1,1) and round_fact = 10 to apply 1 unit thresholds
                            max_thresholds = NULL, # c(1,1) to apply symmetrical 1 unit thresholds
                            plus_minus = c(0.5, 0.5), # default symmetrical thresholds of 1 unit
                            match_logic = NULL, # use c("==","==") to apply fastest rounding based search
                            cell_size = 2, # works best if >/= 1
                            max_dist = NULL,
                            min_dist = cell_size / 2, 
                            delta_t = 10, # time difference between start and end
                            raster = FALSE,
                            kNN = FALSE) {

  # check that start_data list is equal in length to varaible_names vector
  if (!identical(length(variable_names), length((start_data)))) {
    stop(
      "Must have a layer for each varible, ",
      "therefore `start_data` must be of the same length as `varible_names`."
    )
  }

  data <- data_lists_to_dfs(start_data, end_data, x = x, y = y, variable_names, raster)
  data <- data %>% dplyr::mutate(id = 1:nrow(data)) # add cell id

  if (!is.null(match_logic)) {
    if (!identical(length(variable_names), length(match_logic))) {
      stop("Need `match_logic` function for each varible.")
    }
    if (!identical(length(variable_names), length((plus_minus)))) {
      stop("Must have `plus_minus` value for each varible.")
    }
    warning(
      "Matching determined using this method is faster, but may be less precise."
    )
  } else {
    if (is.null(max_thresholds)) {
      min_thresholds <- plus_minus * 2
      max_thresholds <- plus_minus * 2
    } else {
      if (!identical(length(min_thresholds), length(max_thresholds))) {
        stop("Need `max_thresholds` and `min_thresholds` of equal lengths.")
      }
    }
    if (!identical(length(variable_names), length(max_thresholds))) {
      stop("Need threshold values for each varible.")
    }
  }

  # round_fact <- c()
  start <- list()
  end <- list()
  n_variables <- length(variable_names)
  data$s <- c()
  data$e <- c()
  s <- c()
  e <- c()


  for (k in seq_along(variable_names)) {

    # if using match_logic and rounding to speed up process
    if (!is.null(match_logic)) {
      # Apply difference threshold using rounding
      round_fact <- 1 / (plus_minus[k] * 2) # inverse for rounding, double for plus/minus
      # if we add 1/100th of round_factor, we can ensure that values ending in 5 round up...
      # but maybe inconsistent up-down rounding would average out to more realistic results?
      start_round <- data[, (2 + k)] * round_fact #+ round_fact/100
      start[[k]] <- round(start_round) / round_fact # rounded start values
      end_round <- data[, (2 + n_variables + k)] * round_fact #+ round_fact/100
      end[[k]] <- round(end_round) / round_fact # rounded end values
    } else {
      # use "<=", ">=" thresholds instead of rounding
      # set rounding thresholds to reduce the number of unique initial values without introducing rounding error
      if (is.null(round_fact)) round_fact <- 1 / (plus_minus[k] * 2) * 10 # 10x precision of threshold values
      start_round <- data[, (2 + k)] * round_fact
      start[[k]] <- round(start_round) / round_fact # rounded start values
      end_round <- data[, (2 + n_variables + k)] * round_fact * 10
      end[[k]] <- round(end_round) / (round_fact * 10) # rounded end values
    }

    if (k == 1) {
      s <- paste(as.vector(start[[k]]))
      e <- paste(as.vector(end[[k]]))
    } else {
      s <- paste(s, as.vector(start[[k]]))
      e <- paste(e, as.vector(end[[k]]))
    }
  }
  data[, "s"] <- s
  data[, "e"] <- e

  # Generate list of unique values in start
  u <- unique(data$s)[order(unique(data$s))] # list of unique values, or combinations

  if (kNN) {
    dist_tab <- dist_kNN_search(data, s, e, u)
  } else {
    dist_tab <- dist_simple_search(as.data.frame(data),
      variable_names = variable_names,
      s = s, e = e, u = u,
      plus_minus = plus_minus,
      min_thresholds = min_thresholds,
      max_thresholds = max_thresholds,
      match_logic = match_logic
    )
  }

  if (is.null(max_dist)) {
    max_dist <- min((max(data$x) - min(data$x)), (max(data$y) - min(data$y)))
  }

  dist_tab$distance[dist_tab$distance == Inf] <- max_dist # sets no analogue
  dist_tab$distance[dist_tab$distance == 0] <- min_dist # sets zero distance to half cell size

  # calculate speed in units of distance by time in same units as `max_dist` and `delta_t`
  dist_tab$speed <- dist_tab$distance / delta_t
  dplyr::full_join(data, dist_tab, by = c("id", "x", "y"))
}


# internal function that converts data lists into a dataframe used by dist_based_vocc function
data_lists_to_dfs <- function(start_data,
                              end_data,
                              x = "x", y = "y",
                              variable_names,
                              raster = FALSE) {
  start_data_vars <- list()
  end_data_vars <- list()

  # check class
  # class()

  # if data is in raster form
  if (raster) {
    start_xy <- as.data.frame(raster::rasterToPoints(start_data[[1]]))[, c(x, y)]
    end_xy <- as.data.frame(raster::rasterToPoints(end_data[[1]]))[, c(x, y)]

    for (i in seq_along(variable_names)) {
      start_data_vars[[i]] <- as.data.frame(raster::rasterToPoints(start_data[[i]]))[, 3]
      end_data_vars[[i]] <- as.data.frame(raster::rasterToPoints(end_data[[i]]))[, 3]
    }

    start_df <- do.call("cbind", start_data_vars)
    start_df <- as.data.frame(start_df)
    names(start_df) <- names(start_data)
    start_df <- cbind(start_xy, start_df)

    end_df <- do.call("cbind", end_data_vars)
    end_df <- as.data.frame(end_df)
    names(end_df) <- names(end_data)
    end_df <- cbind(end_xy, end_df)
  } else {
    start_xy <- start_data[[1]][, c(x, y)] # data frame of XY coords
    end_xy <- end_data[[1]][, c(x, y)]


    # if data is in list of dataframes from predict functions
    for (i in seq_along(variable_names)) {
      variable_i <- variable_names[i]
      start_data_vars[i] <- start_data[[i]][variable_i]
      end_data_vars[i] <- end_data[[i]][variable_i]
    }
    start_df <- as.data.frame(start_data_vars)
    names(start_df) <- names(start_data)
    start_df <- cbind(start_xy, start_df)

    end_df <- as.data.frame(end_data_vars)
    names(end_df) <- names(start_data)
    end_df <- cbind(start_xy, end_df)
  }

  if (!identical(length(start_df), length((end_df)))) {
    warning(
      "Start and end data are not the same length.",
      "Only coordinate combinations found in both will be retained."
    )
  }
  as.data.frame(dplyr::inner_join(start_df, end_df, by = c(x, y), suffix = c("_s", "_e")))
}

# internal function to find nearest analogue(s) for each location
dist_simple_search <- function(data,
                               variable_names,
                               s, e, u,
                               plus_minus, match_logic,
                               min_thresholds, max_thresholds) {
  sid <- list() # empty list for source IDs
  tid <- list() # empty list for target IDs
  d <- list() # empty list for distances
  n_variables <- length(variable_names)

  # function finding climate matches of u with e
  match <- function(u) {
    u_split <- as.numeric(strsplit(u, " ")[[1]])
    # e_split <- purrr::map_df(strsplit(e, " "), ~data.frame(t(as.numeric(.x))))
    e_split <- purrr::map_df(strsplit(e, " "), function(x) data.frame(t(as.numeric(x))))

    matches_round <- matrix(nrow = nrow(e_split), ncol = ncol(e_split))
    matches_lower <- matrix(nrow = nrow(e_split), ncol = ncol(e_split))
    matches_upper <- matrix(nrow = nrow(e_split), ncol = ncol(e_split))
    matches_both <- matrix(nrow = nrow(e_split), ncol = ncol(e_split))

    for (j in seq_len(length(variable_names))) {
      if (is.null(match_logic)) {
        matches_upper[, j] <- e_split[, j] <= u_split[j] + max_thresholds[j]
        matches_lower[, j] <- e_split[, j] >= u_split[j] - min_thresholds[j]
        matches_both[, j] <- matches_upper[, j] & matches_lower[, j]
        # matches_both[, j] <- vapply(seq_len(nrow(e_split)), function(i)
        #  all(matches_lower[i, j], matches_upper[i, j]), FUN.VALUE = logical(1))
      } else {
        matches_round[, j] <- vapply(seq_len(nrow(e_split)), function(i)
          eval(rlang::parse_expr(paste(e_split[i, j], match_logic[[j]], u_split[j]))),
        FUN.VALUE = logical(1)
        )
      }
    }

    if (!is.null(match_logic)) {
      apply(matches_round, 1, all)
    } else {
      apply(matches_both, 1, all)
    }
  }

  m <- sapply(u, match) # list of climate matches for unique values
  X <- data$x # x coords
  Y <- data$y # y coords
  s <- data$s
  out <- list()

  for (i in seq_along(data$s)) { # loop for all grid cells in both time periods
    mi <- m[, u == s[i]] # recalls list of climate matches for s[i]
    sxy <- data[i, ] # coordinates of i-th unique combination in start

    if (sum(mi, na.rm = TRUE) > 0) { # search unless no-analogue climate
      # empty vector for distances between all analagous points
      d_all <- vector(mode = "numeric", length = length(mi))
      for (k in seq_along(mi)) {
        if (mi[k]) {
          # distances to all matches
          d_all[k] <- sqrt((X[i] - X[k])^2 + (Y[i] - Y[k])^2)
        } else {
          d_all[k] <- NA
        }
      }

      d[[i]] <- min(d_all, na.rm = TRUE) # distance to closest match
      txy <- data[d_all == d[[i]], ] # coordinates of closest match in end
      tid[[i]] <- c()
      tid[[i]] <- as.vector(na.omit(txy$id)) # the ID of the closest match(es)
      out[[i]] <- data.frame(tid = tid[[i]])
      out[[i]]$target_X <- as.vector(na.omit(txy$x))
      mean_target_X <- mean(as.vector(na.omit(txy$x)))
      out[[i]]$target_Y <- as.vector(na.omit(txy$y))
      mean_target_Y <- mean(as.vector(na.omit(txy$y)))
      n_targets <- nrow(out[[i]])
      out[[i]]$id <- rep(sxy$id, n_targets)
      out[[i]]$x <- rep(sxy$x, n_targets)
      out[[i]]$y <- rep(sxy$y, n_targets)
      out[[i]]$distance <- rep(d[[i]], n_targets)

      value_k <- data.frame()
      v <- list()
      for (k in seq_along(variable_names)) {
        value_k[i, k] <-
          round(mean(as.numeric(na.omit(txy[, (2 + n_variables + k)])),
            na.rm = TRUE
          ), digits = 2)

        if (k == 1) {
          v[[i]] <- paste(as.vector(value_k[[i, k]]))
        } else {
          v[[i]] <- paste(v[[i]], as.vector(value_k[[i, k]]))
        }
      }

      target_values <- paste(as.vector(v[[i]]))
      out[[i]]$target_values <- rep(target_values, n_targets)
      out[[i]]$n_targets <- rep(n_targets, n_targets)
      out[[i]]$mean_target_X <- rep(mean_target_X, n_targets)
      out[[i]]$mean_target_Y <- rep(mean_target_Y, n_targets)
    } else { # else statement for no-analogue climates

      d[[i]] <- Inf # flag distances as infinity for no analogues
      tid[[i]] <- NA
      out[[i]] <- data.frame(tid = tid[[i]])
      names(out[[i]])[1] <- "tid"
      out[[i]]$target_X <- NA
      out[[i]]$target_Y <- NA
      out[[i]]$id <- sxy$id
      out[[i]]$x <- sxy$x
      out[[i]]$y <- sxy$y
      out[[i]]$distance <- d[[i]]
      out[[i]]$target_values <- NA
      out[[i]]$n_targets <- 0
      out[[i]]$mean_target_X <- NA
      out[[i]]$mean_target_Y <- NA
    }
  }
  do.call(rbind, out)
}


# internal function using kNN search method for just one nearest analogue
dist_kNN_search <- function(data, s, e, u) {
  sid <- list() # empty list for source IDs
  tid <- list() # empty list for target IDs
  d <- list() # empty list for distances
  idxy <- data %>% dplyr::select(id, x, y) # data frame of IDs and XY coords

  for (i in u) { # loop for each unique PC1/PC2 combination
    sxy <- idxy[data$s == i, , drop = FALSE] # coordinates of i-th unique combination in start
    txy <- idxy[data$e == i, , drop = FALSE] # coordinates of i-th unique combination in end
    sid[[i]] <- sxy$id
    if (nrow(txy) > 0) { # kNN search unless no-analogue climate
      knn <- data.frame(
        yaImpute::ann(as.matrix(txy[, -1]), as.matrix(sxy[, -1]), k = 1)$knnIndexDist
      )

      tid[[i]] <- txy[knn[, 1], "id"] # the IDs of the closest matches
      d[[i]] <- sqrt(knn[, 2]) # their corresponding geographic distances
    } else { # else statement for no-analogue climates

      tid[[i]] <- rep(NA, nrow(sxy)) # flag destinations as missing for no analogues
      d[[i]] <- rep(Inf, nrow(sxy)) # flag distances as infinity for no analogues
    }
  }
  sid <- do.call("c", sid)
  tid <- do.call("c", tid)
  d <- do.call("c", d)
  sxy <- dplyr::full_join(tibble::tibble(id = sid), idxy)[2:3]
  txy <- dplyr::left_join(tibble::tibble(id = tid), idxy)[2:3]
  names(txy) <- c("target_X", "target_Y")

  tibble::as_tibble(cbind(id = sid, sxy, txy, distance = d, n_targets = 1))
}
