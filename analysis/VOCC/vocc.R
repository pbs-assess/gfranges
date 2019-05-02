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
                            raster = FALSE) {

  # check that start_data list is equal in length to varaible_names vector
  if (!identical(length(variable_names), length((start_data)))) {
    stop(
      "Must have a layer for each varible, ",
      "therefore `start_data` must be of the same length as `varible_names`."
    )
  }

  data <- data_lists_to_dfs(start_data, end_data, x = x, y = y, variable_names, raster)
  data <- data %>% dplyr::mutate(id = 1:nrow(data)) # add cell id
  idxy <- data %>% dplyr::select(id, x, y) # data frame of IDs and XY coords

  # Apply difference threshold using rounding
  if (!identical(length(variable_names), length((thresholds)))) {
    stop("Must have `thresholds` value for all `varible_names`.")
  }

  round_fact <- c()
  start <- list()
  end <- list()
  n_variables <- length(variable_names)
  for (i in seq_along(thresholds)) {
    round_fact <- 1 / (thresholds[i] * 2) # inverse for rounding, double for plus/minus
    start[[i]] <- round(data[, (2 + i)] * round_fact) / round_fact # rounded start values
    end[[i]] <- round(data[, (2 + n_variables + i)] * round_fact) / round_fact # rounded end values
    if (i == 1) {
      s <- paste(as.vector(start[[i]]))
      e <- paste(as.vector(end[[i]]))
    } else {
      s <- paste(s, as.vector(start[[i]]))
      e <- paste(e, as.vector(end[[i]]))
    }
  }

  # Generate list of unique values in start
  u <- unique(s)[order(unique(s))] # list of unique values, or combinations
  
  # Generate empty lists if not functionizing next steps
  sid <- list() # empty list for source IDs
  tid <- list() # empty list for target IDs
  d <- list() # empty list for distances
  
  
  # Find nearest analogue for each location
  # draft code of simpler method, possibly for alternate C++ option
  # # dist_simple_search <- function(idxy, s, e, u){
  # # sid <- list() # empty list for source IDs
  # # tid <- list() # empty list for target IDs
  # # d <- list() # empty list for distances
  
  # match <- function(u){c(u == e)}       # function finding climate matches of u with e
  # m     <- sapply(u, match)             # list of climate matches for unique values
  # X <- idxy[x]                # x coords
  # Y <- idxy[y]                # y coords
  # for(i in seq_along(s)){     # loop for all grid cells in both time periods
  #   mi   <- m[[u==s[i]]]      # recalls list of climate matches for s[i]
  #   sxy <- idxy[s == i, , drop = FALSE] # coordinates of i-th unique combination in start
  #   sid[[i]] <- sxy$id
  #   if (nrow(mi) > 0) {       # search unless no-analogue climate
  #     d_all <- c()            # empty vector for distances between all analagous points
  #     for (k in seq_along(mi)) {
  #       d_all[k] <- sqrt((X[i]-X[k])^2 + (Y[i]-Y[k])^2)    # distances to all matches
  #     }
  #     #d_all <- do.call("c", d_all) 
  #     d[[i]] <- min(d_all)    # distance to closest match
  #     txy <- idxy[d_all == min(d_all), , drop = FALSE] # coordinates of closest match in end
  #     tid[[i]] <- sxy$id      # the ID of the closest match
  #   } else {                  # else statement for no-analogue climates
  #     d[[i]] <- Inf           # flag distances as infinity for no analogues
  #   } 
  # }
  
  
  # kNN search method 
  # # dist_kNN_search <- function(idxy, s, e, u){
  # # sid <- list() # empty list for source IDs
  # # tid <- list() # empty list for target IDs
  # # d <- list() # empty list for distances
  
  for (i in u) { # loop for each unique PC1/PC2 combination
    sxy <- idxy[s == i, , drop = FALSE] # coordinates of i-th unique combination in start
    txy <- idxy[e == i, , drop = FALSE] # coordinates of i-th unique combination in end
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

  dist_tab <- cbind(id = sid, txy, distance = d)
  
  # }
  
  # dist_kNN_search(idxy, s, e, u)
  # dist_simple_search(idxy, s, e, u)
  
  out <- dplyr::inner_join(data, dist_tab, by = "id")

  if (!max_dist) {
    max_dist <- min((max(data$x) - min(data$x)), (max(data$y) - min(data$y)))
  }

  out$distance[out$distance == Inf] <- max_dist # sets no analogue
  out$distance[out$distance == 0] <- cell_size / 2 # sets zero distance to half cell size

  # calculate speed in units of distance by time in same units as `max_dist` and `delta_t`
  out$speed <- out$distance / delta_t
  round(out, digits = 3)
}




# internal function that converts data lists into a dataframe used by dist_based_vocc function
data_lists_to_dfs <- function(start_data,
            end_data,
            x = "x", y = "y",
            variable_names,
            raster = FALSE
            ){
  start_xy <- start_data[[1]][, c(x, y)] # data frame of XY coords
  end_xy <- end_data[[1]][, c(x, y)]
  
  start_data_vars <- list()
  end_data_vars <- list()
  
  # if data is in raster form
  if (raster) {
    for (i in seq_along(variable_names)) {
      start_data_vars[i] <- as.data.frame(raster::rasterToPoints(start_data[[i]]))[, 3]
      end_data_vars[i] <- as.data.frame(raster::rasterToPoints(end_data[[i]]))[, 3]
    }
    start_df <- as.data.frame(start_data_vars)
    names(start_df) <- names(start_data)
    start_df <- cbind(start_xy, start_df)
    
    end_df <- as.data.frame(end_data_vars)
    names(end_df) <- names(end_data)
    end_df <- cbind(end_xy, end_df)
  } else {
    
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




# EXAMPLE DATA FROM Hamann et al. 2015

# Simple example data
# start <- asc2dataframe("eg_data/MAT6190.asc")
# finish  <- asc2dataframe("eg_data/MAT2020s.asc")

# Two variable example data
var1_s <- SDMTools::asc2dataframe("eg_data/PC1-6190.asc") 
var2_s <- SDMTools::asc2dataframe("eg_data/PC2-6190.asc")
var1_e <- SDMTools::asc2dataframe("eg_data/PC1-2020s.asc")
var2_e <- SDMTools::asc2dataframe("eg_data/PC2-2020s.asc")

# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data1 <- list(var1 = var1_s)
end_data1 <- list(var1 = var1_e)

# data with two climate variables
start_data2 <- list(var1 = var1_s, var2 = var2_s)
end_data2 <- list(var1 = var1_e, var2 = var2_e)


out1 <- dist_based_vocc(
  start_data = start_data1,
  end_data = end_data1,
  x = "x",
  y = "y",
  variable_names = c("var.1"), 
  thresholds = c(0.13), 
  cell_size = 1, 
  max_dist = 10000,
  delta_t = 50,
  raster = FALSE
)

out2 <- dist_based_vocc(
  start_data = start_data2,
  end_data = end_data2,
  x = "x",
  y = "y",
  variable_names = c("var.1", "var.1"), 
  thresholds = c(0.13, 0.13), 
  cell_size = 1, 
  max_dist = 10000,
  delta_t = 50,
  raster = FALSE
)

out2
hist(log10(out2$distance))
hist(log10(out2$speed))
