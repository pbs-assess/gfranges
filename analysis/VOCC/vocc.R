# install.packages("SDMTools")     # install package to read and write ESRI ASCII grids
# install.packages("yaImpute")     # install package for k-nearest neighbour (kNN) search
# install.packages("raster")
# library(dplyr)

#' Velocity of climate change based on distance
#'
#' @param start_data Named list containing starting climate data.
#' @param end_data Named list containing target climate data.
#' @param x Numeric vector of x coordinates.
#' @param y Numeric vector of y coordinates.
#' @param variable_names Vector of columns/layers names within each data element.
#' @param thresholds Vector of plus/minus threshold(s) to define climate match.
#' @param cell_size Cell size in raster or distance to nearest data point.
#' @param max_dist Distance reported when no analogue found. Default will
#'  estimate this assuming that both major axes are likely to hold an analogue.
#' @param delta_t Time difference between starting and target time periods.
#' @param raster Logical for whether climate data is in raster form.
#'
#' @export
#'

dist_based_vocc <- function(start_data,
                            end_data,
                            x = "x",
                            y = "y",
                            variable_names = c("var.1", "var.2"),
                            thresholds = c(0.13, 0.1), # plus/minus thresholds to define climate match
                            cell_size = 2, # works best if >/= 1
                            max_dist = NULL,
                            delta_t = 50, # time difference between start and end
                            raster = FALSE,
                            kNN = FALSE
  ) {

  # check that start_data list is equal in length to varaible_names vector
  if (!identical(length(variable_names), length((start_data)))) {
    stop(
      "Must have a layer for each varible, ",
      "therefore `start_data` must be of the same length as `varible_names`."
    )
  }

  data <- data_lists_to_dfs(start_data, end_data, x = x, y = y, variable_names, raster)
  data <- data %>% dplyr::mutate(id = 1:nrow(data)) # add cell id
  
  # Apply difference threshold using rounding
  if (!identical(length(variable_names), length((thresholds)))) {
    stop("Must have `thresholds` value for all `varible_names`.")
  }

  round_fact <- c()
  start <- list()
  end <- list()
  n_variables <- length(variable_names)
  data$s <- c()
  data$e <- c()
  for (i in seq_len(nrow(data))) {
  for (k in seq_along(thresholds)) {
    round_fact <- 1 / (thresholds[k] * 2) # inverse for rounding, double for plus/minus
    start[[i]] <- round(data[i, (2 + k)] * round_fact) / round_fact # rounded start values
    end[[i]] <- round(data[i, (2 + n_variables + k)] * round_fact) / round_fact # rounded end values
    if (k == 1) {
      data[i,"s"] <- paste(as.vector(start[[i]]))
      data[i,"e"] <- paste(as.vector(end[[i]]))
    } else {
      data[i,"s"] <- paste(s, as.vector(start[[i]]))
      data[i,"e"] <- paste(e, as.vector(end[[i]]))
    }
  }
  }  

  # Generate list of unique values in start
  u <- unique(data$s)[order(unique(data$s))] # list of unique values, or combinations

  # # Generate empty lists if not functionizing next steps
  # sid <- list() # empty list for source IDs
  # tid <- list() # empty list for target IDs
  # d <- list() # empty list for distances


  # Find nearest analogue for each location
  # # draft code of simpler method, possibly for alternate C++ option
dist_simple_search <- function(data, s, e, u){
sid <- list() # empty list for source IDs
tid <- list() # empty list for target IDs
d <- list() # empty list for distances

  match <- function(u){c(u == data$e)}   # function finding climate matches of u with e
  m     <- sapply(u, match)         # list of climate matches for unique values
  X <- data$x                 # x coords
  Y <- data$y                 # y coords
  s <- data$s
  out <- list()
  for(i in seq_along(data$s)){     # loop for all grid cells in both time periods
    mi <- m[ , u == s[i]]     # recalls list of climate matches for s[i]
    sxy <- data[i,]        # coordinates of i-th unique combination in start
    out[[i]] <- tibble::tibble()
    #browser()
  # FIXME:  need to check why this if else not working...
  if (length(mi) > 0) {     # search unless no-analogue climate
      d_all <- c()            # empty vector for distances between all analagous points
      for (k in seq_along(mi)) {
        if (mi[k]) {
          d_all[k] <- sqrt((X[i]-X[k])^2 + (Y[i]-Y[k])^2) } # distances to all matches
        else {
          d_all[k] <- NA
        }
      }
      #d_all <- do.call("c", d_all)
      d[[i]] <- min(d_all, na.rm = TRUE)    # distance to closest match
      txy <- data[d_all == d[[i]], , drop = TRUE] # coordinates of closest match in end
      tid[[i]] <- c()
      # returns 2 data points equal distance away?
      tid[[i]] <- na.omit(txy$id)      # the ID of the closest match
    } else {                  # else statement for no-analogue climates
      d[[i]] <- Inf           # flag distances as infinity for no analogues
      tid[[i]] <- NA
    }
    out[[i]] <- tibble::tibble(tid[[i]])
    names(out[[i]])[1] <- "tid"
    out[[i]]$target_X <- na.omit(txy$x)
    out[[i]]$target_Y <- na.omit(txy$y)
    n_targets <- nrow(out[[i]])
    out[[i]]$id <- rep(sxy$id, n_targets)
    out[[i]]$x <- rep(sxy$x, n_targets)
    out[[i]]$y <- rep(sxy$y, n_targets)
    out[[i]]$distance <- rep(d[[i]], n_targets)
    out[[i]]$n_targets <- rep(n_targets, n_targets)
  }

  do.call(rbind, out)
}
  
# kNN search method
dist_kNN_search <- function(data, s, e, u){
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


if (kNN) {dist_tab <- dist_kNN_search(data, s, e, u)
} else { 
dist_tab <- dist_simple_search(data, s, e, u)
}

  if (is.null(max_dist)) {
    max_dist <- min((max(data$x) - min(data$x)), (max(data$y) - min(data$y)))
  }
  
  dist_tab$distance[dist_tab$distance == Inf] <- max_dist # sets no analogue
  dist_tab$distance[dist_tab$distance == 0] <- cell_size / 2 # sets zero distance to half cell size
  
  # calculate speed in units of distance by time in same units as `max_dist` and `delta_t`
  dist_tab$speed <- dist_tab$distance / delta_t
  round(dist_tab, digits = 2)
  dplyr::inner_join(data, dist_tab, by = c("id","x","y")) 
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
      var <- variable_names[i]
      start_data_vars[i] <- start_data[[i]][var]
      end_data_vars[i] <- end_data[[i]][var]
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
  dplyr::inner_join(start_df, end_df, by = c(x, y), suffix = c("_s", "_e"))
}




