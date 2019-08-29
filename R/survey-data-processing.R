#' Tidy the survey set data for use in modeling
#'
#' @param dat Output from [get_survey_sets()].
#' @param survey The name of a survey (see get_ssids()).
#' @param years The years.
#' @param utm_zone UTM zone.
#' @param density_column Name of the density column.
#'
#' @export
tidy_survey_sets <- function(dat, survey, years,
                             utm_zone = 9,
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

  dat <- select(
    dat, year, survey_series_id, survey_abbrev,
    longitude, latitude, depth_m, density, fishing_event_id, catch_weight
  ) %>%
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
#' @param dat
#' @param predictors List of variable names must be wrapped in 'quo()'.
#'
#' @export
#'
scale_predictors <- function(dat, predictors = c(quo(log_depth))) {
  # to put spatial decay parameter on right scale

  for (i in seq_len(length(predictors))) {
    var <- predictors[[i]]
    var_mean <- paste0(quo_name(var), "_mean")
    var_sd <- paste0(quo_name(var), "_sd")
    var_scaled <- paste0(quo_name(var), "_scaled")
    var_scaled2 <- paste0(quo_name(var), "_scaled2")
    var_scaled3 <- paste0(quo_name(var), "_scaled3")

    dat <- mutate(
      dat,
      !!var_mean := mean(!!var, na.rm = TRUE),
      !!var_sd := sd(!!var, na.rm = TRUE)
    )

    avg <- dat[[1, var_mean]]
    sd <- dat[[1, var_sd]]

    dat <- mutate(
      dat,
      !!var_scaled := ((!!var) - avg) / sd,
      !!var_scaled2 := (((!!var) - avg) / sd) * (((!!var) - avg) / sd),
      !!var_scaled3 :=
        (((!!var) - avg) / sd) * (((!!var) - avg) / sd) * (((!!var) - avg) / sd)
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
