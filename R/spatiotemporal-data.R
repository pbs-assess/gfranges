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
#' @param dummy_year Single year (for each area) on which to base the survey grid. 
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


#' Revised plot survey sets
plot_survey_sets <- function(pred_dat, raw_dat, fill_column = c("combined", "bin", "pos"),
  fill_scale =
    ggplot2::scale_fill_viridis_c(trans = "sqrt", option = "C"),
  colour_scale =
    ggplot2::scale_colour_viridis_c(trans = "sqrt", option = "C"),
  pos_pt_col = "#FFFFFF60",
  bin_pt_col = "#FFFFFF40",
  pos_pt_fill = "#FFFFFF05",
  pt_size_range = c(0.5, 9),
  show_legend = TRUE,
  extrapolate_depth = TRUE,
  extrapolation_buffer = 0,
  show_model_predictions = TRUE,
  show_raw_data = TRUE,
  utm_zone = 9,
  fill_label = "Predicted\nbiomass\ndensity (kg/m^2)",
  pt_label = "Tow density (kg/km^2)",
  rotation_angle = 0,
  rotation_center = c(500, 5700),
  show_axes = TRUE,
  xlim = NULL,
  ylim = NULL,
  x_buffer = c(-5, 5),
  y_buffer = c(-5, 5),
  north_symbol = FALSE,
  north_symbol_coord = c(130, 5975),
  north_symbol_length = 30,
  cell_size = 2, circles = FALSE) {
  
  
  fill_column <- match.arg(fill_column)
  # browser()
  if (!extrapolate_depth) {
    pred_dat <- filter(
      pred_dat,
      akima_depth >= min(raw_dat$depth, na.rm = TRUE) - extrapolation_buffer,
      akima_depth <= max(raw_dat$depth, na.rm = TRUE) + extrapolation_buffer,
      akima_depth > 0
    )
  }
  
  pred_dat$id <- NA # for circles
  if (show_model_predictions && !circles) {
    # turn grid into explicit rectangles for possible rotation:
    pred_dat <- lapply(seq_len(nrow(pred_dat)), function(i) {
      row_dat <- pred_dat[i, , drop = FALSE]
      X <- row_dat$X
      Y <- row_dat$Y
      data.frame(
        X = c(
          X - cell_size / 2, X + cell_size / 2,
          X + cell_size / 2, X - cell_size / 2
        ),
        Y = c(
          Y - cell_size / 2, Y - cell_size / 2,
          Y + cell_size / 2, Y + cell_size / 2
        ),
        combined = row_dat$combined,
        bin = row_dat$bin,
        pos = row_dat$pos,
        # year = row_dat$year,
        id = i
      )
    }) %>% bind_rows()
  }
  
  if (north_symbol) {
    north <- data.frame(
      X = c(north_symbol_coord[1], north_symbol_coord[1]),
      Y = c(north_symbol_coord[2], north_symbol_coord[2] + north_symbol_length)
    )
    north_lab_coord <- c(north$X[1], north$Y[1] - 15)
    
    north <- gfplot:::rotate_df(north, rotation_angle, rotation_center)
    
    north_sym <- data.frame(
      X = north$X[1],
      Xend = north$X[2],
      Y = north$Y[1],
      Yend = north$Y[2]
    )
    
    r <- gfplot:::rotate_coords(north_lab_coord[1], north_lab_coord[2],
      rotation_angle = rotation_angle,
      rotation_center = rotation_center
    )
    north_lab_coord <- c(r$x, r$y)
  }
  
  coast <- gfplot:::load_coastline(range(raw_dat$lon) + c(-1, 1),
    range(raw_dat$lat) + c(-1, 1),
    utm_zone = utm_zone
  )
  coast <- gfplot:::rotate_df(coast, rotation_angle, rotation_center)
  
  isobath <- gfplot:::load_isobath(range(raw_dat$lon) + c(-5, 5),
    range(raw_dat$lat) + c(-5, 5),
    bath = c(100, 200, 500), utm_zone = 9
  )
  isobath <- gfplot:::rotate_df(isobath, rotation_angle, rotation_center)
  
  pred_dat <- gfplot:::rotate_df(pred_dat, rotation_angle, rotation_center)
  raw_dat <- gfplot:::rotate_df(raw_dat, rotation_angle, rotation_center)
  
  if (is.null(xlim) || is.null(ylim)) {
    xlim <- range(raw_dat$X) + x_buffer
    ylim <- range(raw_dat$Y) + y_buffer
  }
  
  g <- ggplot()
  
  if (show_model_predictions && !circles) {
    g <- g + ggplot2::geom_polygon(
      data = pred_dat, aes_string("X", "Y",
        fill = fill_column,
        colour = fill_column, group = "id"
      )
    ) +
      fill_scale + colour_scale
  }
  if (show_model_predictions && circles) {
    g <- g + ggplot2::geom_point(
      data = pred_dat, aes_string("X", "Y",
        fill = fill_column, colour = fill_column, group = "id"
      ), size = cell_size, pch = 15
    ) +
      fill_scale + colour_scale
  }
  
  
  g <- g +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    gfplot:::theme_pbs() +
    coord_equal(xlim = xlim, ylim = ylim) +
    guides(
      shape = ggplot2::guide_legend(override.aes = list(colour = "grey30")),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey30"))
    ) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2
    ) +
    guides(shape = FALSE, colour = FALSE) +
    labs(size = pt_label, fill = fill_label) +
    ylab("Northing") + xlab("Easting")
  
  if (show_raw_data) {
    g <- g +
      geom_point(
        data = filter(raw_dat, present == 0),
        aes_string(x = "X", y = "Y"),
        col = if (show_model_predictions) bin_pt_col else "grey20",
        pch = 4, size = cell_size
      ) +
      geom_point(
        data = filter(raw_dat, present == 1),
        aes_string(
          x = "X", y = "Y",
          size = "density * 1e6"
        ), fill = pos_pt_fill,
        col = if (show_model_predictions) pos_pt_col else "grey20", pch = 4
      )
  }
  
  
  if (!show_legend) {
    g <- g + theme(legend.position = "none")
  }
  
  if (!show_axes) {
    g <- g + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }
  
  suppressWarnings({
    suppressMessages({
      g <- g + geom_path(
        data = isobath, aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)"
        ),
        inherit.aes = FALSE, lwd = 0.5, col = "grey99", alpha = 0.4
      )})})
  
  if (north_symbol) {
    g <- g + ggplot2::geom_segment(
      data = north_sym,
      aes_string(x = "X", y = "Y", xend = "Xend", yend = "Yend"),
      inherit.aes = FALSE, colour = "grey30", lwd = 0.8,
      arrow = ggplot2::arrow(length = unit(0.7, "char"))
    )
    g <- g + ggplot2::annotate("text",
      label = "N", colour = "grey30",
      x = north_lab_coord[1], y = north_lab_coord[2]
    )
  }
  
  g
}


