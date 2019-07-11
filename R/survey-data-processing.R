#' Tidy the survey set data for use in modeling
#'
#' @param dat Output from [get_survey_sets()].
#' @param survey The name of a survey (see get_ssids()).
#' @param years The years.
#' @param utm_zone UTM zone.
#' @param density_column Name of the density column.
#'
#' @export
tidy_survey_sets <- function(dat, survey, years, utm_zone = 9,
  density_column = "density_kgpm2") {

  # Make sure here are no duplicated fishing events in surveyed tows
  # Could be there because of the sample ID column being merged in
  dat <- dat[!duplicated(
    select(dat, year, fishing_event_id)
  ), , drop = FALSE]

  dat <- dat %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years)

  names(dat)[names(dat) %in% density_column] <- "density"

  dat <- select(dat, year, survey_series_id, survey_abbrev,
    longitude, latitude, depth_m, density, fishing_event_id, catch_weight) %>%
    rename(X = longitude, Y = latitude) %>%
    rename(ssid = survey_series_id) %>%
    rename(depth = depth_m)
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))

  dat$lat <- dat$Y
  dat$lon <- dat$X
  if (nrow(dat) >= 1) {
    dat <- as_tibble(gfplot:::ll2utm(dat, utm_zone = utm_zone))
  }

  dat
}

load_bath <- function(utm_zone = 9) {
  data("bctopo", package = "PBSdata", envir = environment())
  bath <- rename(bctopo, X = x, Y = y, depth = z)
  ll2utm(bath, utm_zone = utm_zone)
}

#' Interpolate survey bathymetry
#' @export
#' @rdname survey-spatial-modelling
interp_survey_bathymetry <- function(dat, utm_zone = 9) {

  .dat <- dat[is.na(dat$depth), , drop = FALSE]
  # reduce size first for speed:
  bath <- load_bath(utm_zone = utm_zone) %>%
    filter(
      X < max(.dat$X + 20),
      X > min(.dat$X - 20),
      Y < max(.dat$Y + 20),
      Y > min(.dat$Y - 20),
      depth > 0
    )

  xo <- sort(unique(.dat$X))
  yo <- sort(unique(.dat$Y))

  ii <- suppressWarnings(akima::interp(
    x = bath$X,
    y = bath$Y,
    z = log(bath$depth),
    xo = xo,
    yo = yo, extrap = TRUE, linear = TRUE
  ))

  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(.dat$X, .dat$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>%
    select(-Var1, -Var2)
  z <- mutate(z, akima_depth = exp(akima_depth))
  dat <- left_join(dat, z, by = c("X", "Y"))
  list(data = dat, bath = bath)
}

#' Add missing depths
#' 
#' @param bath Output from [interp_survey_bathymetry()].
#' 
#' @export
#' 
#' @rdname survey-spatial-modelling
add_missing_depths <- function(dat, survey, years, bath) {
if (sum(is.na(dat$depth)) > 0) {
  bath <- bath$data
  bath <- bath %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years) %>%
    select(fishing_event_id, akima_depth)
  dat <- left_join(dat, bath, by = "fishing_event_id")
  dat$depth[is.na(dat$depth)] <- dat$akima_depth[is.na(dat$depth)]
  dat <- dat %>% select(-akima_depth)
  dat
} else {
  dat
}
}

#' Scale survey predictors
#' 
#' @export
#' 
#' @rdname survey-spatial-modelling
scale_predictors <- function(dat, 
  predictors = c(quo(log_depth)) 
  ) {
  # to put spatial decay parameter on right scale
  
  for (i in seq_len(length(predictors))) {
        
      var <- predictors[[i]]
        
        var_mean <- paste0(quo_name(var),"_mean") 
        var_sd <- paste0(quo_name(var),"_sd")
        var_scaled <- paste0(quo_name(var),"_scaled")
        var_scaled2 <- paste0(quo_name(var),"_scaled2")
        var_scaled3 <- paste0(quo_name(var),"_scaled3")
        
         dat <- mutate(dat,
          !! var_mean := mean(!! var, na.rm = TRUE),
          !! var_sd := sd(!! var, na.rm = TRUE)
        )
          
         avg <- dat[[1,var_mean]]
         sd <- dat[[1,var_sd]]
         
         dat <- mutate(dat,
         !! var_scaled := ((!! var) - avg) / sd,
         !! var_scaled2 := (((!! var) - avg) / sd)*(((!! var) - avg) / sd),
         !! var_scaled3 := (((!! var) - avg) / sd)*(((!! var) - avg) / sd)*(((!! var) - avg) / sd)
    )
  }
  dat <- mutate(dat, X10 = X / 10, Y10 = Y / 10) 
}


initf <- function(init_b0, n_time, n_knots, n_beta, type = "lognormal") {
  ini <- list(
    gp_sigma = rlnorm(1, log(1), 0.05),
    gp_theta = rlnorm(1, log(2), 0.05),
    B = c(init_b0, rnorm(n_beta, 0, 0.05)),
    spatialEffectsKnots =
      matrix(runif(n_time * n_knots, -0.05, 0.05),
        nrow = n_time, ncol = n_knots
      )
  )
  if (type == "lognormal") {
    ini$cv <- array(rlnorm(1, log(1.0), 0.05), dim = 1)
  }
  ini
}


#' Plot the output from a geostatistical model of survey data
#'
#' Takes the output from [fit_survey_sets()] and creates a map of the model
#' predictions and/or the raw data. Includes a number of options for customizing
#' the map including the ability to rotate the map.
#'
#' @param pred_dat The `predictions` element of the output from
#'   [fit_survey_sets()].
#' @param raw_dat The `data` element of the output from [fit_survey_sets()].
#' @param fill_column The name of the column to plot. Options are `"combined"`
#'   for the combined model, `"bin"` for the binary component model, or `"pos"`
#'   for the positive component model.
#' @param fill_scale A ggplot `scale_fill_*` object.
#' @param colour_scale A ggplot `scale_colour_*` object. You likely want this to
#'   match `fill_scale` unless you want the map to look strange.
#' @param pos_pt_col The color for positive set location points.
#' @param bin_pt_col The color for binary set location points.
#' @param pos_pt_fill The fill color for positive set location points.
#' @param pt_size_range The range of point sizes for positive set location
#'   points.
#' @param show_legend Logical for whether or not to show the legend.
#' @param extrapolate_depth Logical for whether or not to show predictions
#'   across all depths in the survey domain (the default) or to not extrapolate
#'   beyond the range of the observed sets in the data set.
#' @param extrapolation_buffer A buffer to add to the minimum and maximum
#'   observed depths if `extrapolate_depth = TRUE`.
#' @param show_model_predictions Logical for whether or not to show the
#'   geostatistical model predictions.
#' @param show_raw_data Logical for whether or not to show the raw data.
#' @param utm_zone The UTM zone to plot in. Should match the zone used in
#'   [fit_survey_sets()].
#' @param fill_label A label to use in the legend for the fill color.
#' @param pt_label A label to use in the legend for the point size.
#' @param rotation_angle An angle to rotate the entire map. Can be useful to
#'   make a map of the BC coast take up less. Defaults to not rotating the map.
#'   The groundfish synopsis report uses `rotation_angle = 40`.
#' @param rotation_center The coordinates around which to rotate the mouth.
#'   These should be in UTM coordinates.
#' @param show_axes Logical for whether or not to show the axes.
#' @param xlim X axis limits in UTM coordinates. The synopsis report uses
#'   `c(360, 653)`. Defaults to the range of the data.
#' @param ylim Y axis limits in UTM coordinates. The synopsis report uses
#'   `c(5275, 6155)`. Defaults to the range of the data.
#' @param x_buffer A buffer in UTM coordinates to extend the X axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param y_buffer A buffer in UTM coordinates to extend the Y axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param north_symbol Logical for whether to include a north symbol.
#' @param north_symbol_coord Coordinates for the north symbol in UTM
#'   coordinates.
#' @param north_symbol_length Length of the north assemble arrow.
#' @param cell_size The size of the grid cells for the model predictions.
#' @param circles Logical for whether to plot the model predictions in circles.
#'   This analysis report uses this for the IPHC survey.
#'
#' @return
#' A ggplot object.
#'
#' @export
#' @family spatial survey modelling functions
#' @examples
#' \donttest{
#' set.seed(123)
#' # pop_surv <- get_survey_sets("pacific ocean perch")
#' # or use built-in data:
#' fit <- fit_survey_sets(pop_surv,
#'   years = 2015,
#'   survey = "SYN QCS")
#'
#' # The combined model:
#' plot_survey_sets(fit$predictions, fit$data, fill_column = "combined")
#' # The positive component model:
#' plot_survey_sets(fit$predictions, fit$data, fill_column = "pos")
#' # Add a custom color scale for the binary model:
#' plot_survey_sets(fit$predictions, fit$data, fill_column = "bin") +
#'   ggplot2::scale_fill_gradient2(midpoint = 0.5,
#'     high = scales::muted("red"),
#'     mid = "white",
#'     low = scales::muted("blue"), limits = c(0, 1), breaks = c(0, 0.5, 1)) +
#'   ggplot2::scale_colour_gradient2(midpoint = 0.5,
#'     high = scales::muted("red"),
#'     mid = "white",
#'     low = scales::muted("blue"), limits = c(0, 1))
#' }

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

    north <- rotate_df(north, rotation_angle, rotation_center)

    north_sym <- data.frame(
      X = north$X[1],
      Xend = north$X[2],
      Y = north$Y[1],
      Yend = north$Y[2]
    )

    r <- rotate_coords(north_lab_coord[1], north_lab_coord[2],
      rotation_angle = rotation_angle,
      rotation_center = rotation_center
    )
    north_lab_coord <- c(r$x, r$y)
  }

  coast <- load_coastline(range(raw_dat$lon) + c(-1, 1),
    range(raw_dat$lat) + c(-1, 1),
    utm_zone = utm_zone
  )
  coast <- rotate_df(coast, rotation_angle, rotation_center)

  isobath <- load_isobath(range(raw_dat$lon) + c(-5, 5),
    range(raw_dat$lat) + c(-5, 5),
    bath = c(100, 200, 500), utm_zone = 9
  )
  isobath <- rotate_df(isobath, rotation_angle, rotation_center)

  pred_dat <- rotate_df(pred_dat, rotation_angle, rotation_center)
  raw_dat <- rotate_df(raw_dat, rotation_angle, rotation_center)

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
      ), size = cell_size, pch = 21
    ) +
      fill_scale + colour_scale
  }
  if (show_raw_data) {
    g <- g +
      geom_point(
        data = filter(raw_dat, present == 0),
        aes_string(x = "X", y = "Y"),
        col = if (show_model_predictions) bin_pt_col else "grey50",
        pch = 4, size = 1.55
      ) +
      geom_point(
        data = filter(raw_dat, present == 1),
        aes_string(
          x = "X", y = "Y",
          size = "density * 1e6"
        ), fill = pos_pt_fill,
        col = if (show_model_predictions) pos_pt_col else "grey30", pch = 21
      )
  }

  g <- g +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    theme_pbs() +
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
        inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
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
