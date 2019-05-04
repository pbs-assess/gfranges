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

  match <- function(u){c(u == e)}   # function finding climate matches of u with e
  m     <- sapply(u, match)             # list of climate matches for unique values
  X <- idxy$x                # x coords
  Y <- idxy$y                # y coords
  
  for(i in seq_along(s)){     # loop for all grid cells in both time periods
    mi <- m[ , u==s[i]]       # recalls list of climate matches for s[i]
    sxy <- idxy[s[i],]        # coordinates of i-th unique combination in start
    sid[[i]] <- sxy$id
    if (length(mi) > 0) {       # search unless no-analogue climate
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
      txy <- idxy[d_all == d[[i]], , drop = FALSE] # coordinates of closest match in end
                                       # returns 2 data points equal distance away?
      tid[[i]] <- na.omit(txy$id)      # the ID of the closest match
    } else {                  # else statement for no-analogue climates
      d[[i]] <- Inf           # flag distances as infinity for no analogues
      tid[[i]] <- NA
    }
    #FIXME: need way to identify which tid belong to sid (when there are multiple tid)
  }


  # kNN search method
  # # dist_kNN_search <- function(idxy, s, e, u){
  # # sid <- list() # empty list for source IDs
  # # tid <- list() # empty list for target IDs
  # # d <- list() # empty list for distances
# 
#   for (i in u) { # loop for each unique PC1/PC2 combination
#     sxy <- idxy[s == i, , drop = FALSE] # coordinates of i-th unique combination in start
#     txy <- idxy[e == i, , drop = FALSE] # coordinates of i-th unique combination in end
#     sid[[i]] <- sxy$id
#     if (nrow(txy) > 0) { # kNN search unless no-analogue climate
#       knn <- data.frame(
#         yaImpute::ann(as.matrix(txy[, -1]), as.matrix(sxy[, -1]), k = 1)$knnIndexDist
#       )
#       tid[[i]] <- txy[knn[, 1], "id"] # the IDs of the closest matches
#       d[[i]] <- sqrt(knn[, 2]) # their corresponding geographic distances
#     } else { # else statement for no-analogue climates
#       tid[[i]] <- rep(NA, nrow(sxy)) # flag destinations as missing for no analogues
#       d[[i]] <- rep(Inf, nrow(sxy)) # flag distances as infinity for no analogues
#     }
#   }

  sid <- do.call("c", sid)
  tid <- do.call("c", tid)
  d <- do.call("c", d)

  sxy <- dplyr::full_join(tibble::tibble(id = sid), idxy)[2:3]
  txy <- dplyr::left_join(tibble::tibble(id = tid), idxy)[2:3]
  names(txy) <- c("target_X", "target_Y")

  dist_tab <- dplyr::full_join(sid, txy, distance = d)

  # }

  # dist_kNN_search(idxy, s, e, u)
  # dist_simple_search(idxy, s, e, u)

  out <- dplyr::inner_join(data, dist_tab, by = "id")

  if (is.null(max_dist)) {
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




# EXAMPLE DATA FROM Hamann et al. 2015

# Simple example data in raster form
temp_rbrick <- readRDS("analysis/rbrick-temp-qcs.rds")
glimpse(temp_rbrick)
slopedat <- vocc::calcslope(temp_rbrick)
mnraster_brick1 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
start_temp <- mnraster_brick1[[1]]
end_temp <- mnraster_brick2[[3]]


temp_rbrick <- readRDS("analysis/rbrick-temp-WCVI.rds")
glimpse(temp_rbrick)
slopedat <- vocc::calcslope(temp_rbrick)
mnraster_brick1 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
start_temp <- mnraster_brick1[[1]]
end_temp <- mnraster_brick2[[3]]

# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data1 <- list(temp = start_temp)
end_data1 <- list(temp = end_temp)


out1 <- dist_based_vocc(
  start_data = start_data1,
  end_data = end_data1,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.5),
  cell_size = 4,
  max_dist = NULL,
  delta_t = 5,
  raster = TRUE
)

out1 <- left_join(out1, slopedat, by = c("x", "y")) %>% select(-icell)
out1$C_per_decade <- out1$slope * 10
out1$km_per_decade <- out1$distance * 10 / 5 # dived by delta_t
# out1$speed_per_decade <- out1$speed*10
head(out1)

# ggplot(out1) +
#   geom_segment(aes(x, y,
#    xend = target_X, yend = target_Y,
#     colour = C_per_decade), size = 1) +
#   #scale_colour_gradient2(low = low_col, high = high_col) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   #labs(colour = col_label) +
#   #coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()



# PLOT VECTORS
plot_vocc <- function(df,
                      vec_col = "C_per_decade",
                      fill_col = "C_per_decade",
                      col_label = "Local\nclimate trend\n(°C/decade)",
                      #col_guide = TRUE,
                      fill_label = "Local\nclimate trend\n(°C/decade)",
                      raster_alpha = 1,
                      lwd = 1,
                      vec_alpha = 1,
                      low_col = "Steel Blue 4",
                      high_col = "Red 3", 
                      coast = NULL,
                      isobath = NULL) {
  
  df <- df[order(-df$distance), ]
  
  colour <- df[[vec_col]]
  fill <- df[[fill_col]]

  gvocc <- ggplot(df, aes(x, y)) +
    geom_raster(aes(fill = fill), alpha = raster_alpha) +
    scale_fill_viridis_c(trans = "sqrt") +
    # scale_fill_gradient2(low = low_col, high = high_col) +
    # scale_fill_gradient2(low = low_col, midpoint = mean(df$temp_s), high = high_col) +
    labs(fill = fill_label) +
    ggquiver::geom_quiver(aes(x, y,
      u = target_X - x, v = target_Y - y,
      colour = colour #,  alpha = (df[["temp_s"]]/max(df[["temp_s"]]))-0.2
    ), vecsize = 0, lwd = lwd, alpha = vec_alpha) +
    #scale_colour_gradient2(low = "Dark Slate Gray", mid = "grey70", high = scales::muted(high_col, l = 25, c = 90)) +
    scale_colour_gradient2(low = low_col, high = high_col) +
    guides(colour = FALSE) +
    xlab("UTM") + ylab("UTM") +
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

  if (!is.null(coast)) {
    gvocc <- gvocc +
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
        geom_polygon(
          data = coast, aes_string(x = "X", y = "Y", group = "PID"),
          fill = "grey87", col = "grey70", lwd = 0.2
        )
      gvocc
    }, silent = TRUE)
  }
  gvocc
}

head(out1)
gvocc <- plot_vocc(out1,
  #vec_col = "Dark Slate Gray",
  low_col = "White",
  high_col = "White", 
  vec_col = "C_per_decade",
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  lwd = 1.5,
  raster_alpha = 1,
  vec_alpha = 0.5,
  isobath = isobath
)
gvocc


# ggplot(out1) +
#   geom_segment(aes(x, y,
#     xend = target_X, yend = target_Y,
#     colour = C_per_decade), arrow=arrow(length=unit(0.2, "cm")), size=0.5) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()
#

# Two variable example data
var1_s <- SDMTools::asc2dataframe("eg_data/PC1-6190.asc")
var2_s <- SDMTools::asc2dataframe("eg_data/PC2-6190.asc")
var1_e <- SDMTools::asc2dataframe("eg_data/PC1-2020s.asc")
var2_e <- SDMTools::asc2dataframe("eg_data/PC2-2020s.asc")


# data with two climate variables
start_data2 <- list(var1 = var1_s, var2 = var2_s)
end_data2 <- list(var1 = var1_e, var2 = var2_e)

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
