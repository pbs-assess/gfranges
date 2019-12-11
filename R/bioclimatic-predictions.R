#' Biennial bioclimatic estimates
#'
#' @param yr Year for which bioclimatic predictions will be estimated
#' @param model_for_prior_years sdmTMB model for all years prior to yr above
#' @param pred_grid Appropriate prediction grid containing unique covariate values 
#'    for each year in model and the year to be estimated. Can include additional 
#'    years before and/or after above period.
#'
#' @export
bioclim_by_year <- function(yr, model_for_prior_years, pred_grid) {
  nd <- pred_grid
  
  nd_trim <- nd %>% filter(year < yr)
  fake_years <- nd %>%
    mutate(year = year - 2) %>%
    filter(year > 2007) %>%
    filter(year < yr)
  
  fake <- sort(unique(fake_years$year))
  true <- sort(unique(nd$year))
  
  if (!identical(fake, true)) "Something is wrong with the pred_grid years."
  
  prior_years <- predict(model_for_prior_years, newdata = nd_trim, return_tmb_object = FALSE)
  fake_est <- predict(model_for_prior_years, newdata = fake_years, return_tmb_object = FALSE)
  fake_est$year <- fake_est$year + 2
  
  bioclim_start <- filter(prior_years, year >= yr - 2) %>%
    filter(year <= yr - 1) %>%
    mutate(bioclim = est)
  bioclim_end <- filter(fake_est, year >= yr) %>%
    filter(year <= yr + 1) %>%
    mutate(bioclim = est_non_rf + omega_s)
  bioclim_all <- dplyr::bind_rows(bioclim_start, bioclim_end) %>%
    select(X, Y, year, bioclim)
  
  bioclim_all
}

#' Bioclimatic estimates for multiyear period 
#' 
#' @param yr First year for which bioclimatic predictions will be estimated
#'
#' @export
bioclim_multi_year <- function(yr, model_for_prior_years, pred_grid) {
  nd <- pred_grid
  period2_range <- max(nd$year) - yr + 1
  
  nd_trim <- nd %>% filter(year < yr)
  fake_years <- nd %>%
    mutate(year = year - 2) %>%
    filter(year > 2007) %>%
    filter(year < yr)
  
  fake <- sort(unique(fake_years$year))
  true <- sort(unique(model_for_prior_years$data$year))
  
  if (!identical(fake, true)) "Model years doesn't match filtered pred_grid years."
  
  prior_years <- predict(model_for_prior_years, newdata = nd_trim, return_tmb_object = FALSE)
  
  past_predictions <- prior_years %>%
    mutate(past_est = est, bioclim = est) %>%
    select(X, Y, year, past_est, bioclim)
  
  fake_est <- predict(model_for_prior_years, newdata = fake_years, return_tmb_object = FALSE)
  fake_est$year <- fake_est$year + 2
  
  bioclim <- filter(fake_est, year >= yr + 2 - 2) %>%
    filter(year <= yr + 2 - 1) %>%
    mutate(bioclim = est_non_rf + omega_s)
  
  
  if (period2_range >= 4) {
    fake_minus4 <- nd %>%
      mutate(year = year - 4) %>%
      filter(year > 2007)
    
    fake <- sort(unique(fake_minus4$year))
    true <- sort(unique(nd_trim$year))
    
    if (!identical(fake, true)) "Model years doesn't match filtered pred_grid years -4."
    
    fake_est_minus4 <- predict(model_for_prior_years, newdata = fake_minus4, return_tmb_object = FALSE)
    fake_est_minus4$year <- fake_est_minus4$year + 4
    # sort(unique(fake_est_minus4$year))
    
    bioclim_4 <- filter(fake_est_minus4, year >= yr + 4 - 2) %>%
      filter(year <= yr + 4 - 1) %>%
      mutate(bioclim = est_non_rf + omega_s)
    
    bioclim <- dplyr::bind_rows(bioclim, bioclim_4)
  }
  
  if (period2_range >= 6) {
    fake_minus6 <- nd %>%
      mutate(year = year - 6) %>%
      filter(year > 2007)
    fake <- sort(unique(fake_minus6$year))
    true <- sort(unique(nd_trim$year))
    
    if (!identical(fake, true)) "Model years doesn't match filtered pred_grid years -6."
    
    fake_est_minus6 <- predict(model_for_prior_years, newdata = fake_minus6, return_tmb_object = FALSE)
    fake_est_minus6$year <- fake_est_minus6$year + 6
    
    bioclim_6 <- filter(fake_est_minus6, year >= yr + 6 - 2) %>%
      filter(year <= yr + 6 - 1) %>%
      mutate(bioclim = est_non_rf + omega_s)
    
    bioclim <- dplyr::bind_rows(bioclim, bioclim_6)
  }
  
  bioclim_all <- dplyr::bind_rows(past_predictions, bioclim) %>%
    select(X, Y, year, past_est, bioclim)
  
  bioclim_all
}
