collapse_outliers <- function(.x, outliers) {
  .x_max <- quantile(.x, outliers[2], na.rm = T)
  .x_min <- quantile(.x, outliers[1], na.rm = T)
  .x[.x > .x_max] <- .x_max
  .x[.x < .x_min] <- .x_min
  .x
}

#' @param y_i Response vector
#' @param X_ij Covariate matrix
#' @param X_pj Covariate prediction matrix
#' @param pred_dat Prediction data frame
#' @param offset Optional offset vector
#' @param knots Number of SPDE knots
#' @param nu Student-t degrees of freedom parameter (fixed)
#' @param student_t Logical for Student-t distribution. If `FALSE`, then a
#'   normal distribution observation model is used.
#' @param group_by_genus Logical. If `TRUE`, a hierarchical random effect
#'   structure is used.
vocc_regression <- function(dat, y_i, X_ij,
  X_pj, pred_dat,
  offset = rep(0, length(y_i)),
  knots = 200, nu = 7, student_t = TRUE,
  group_by_genus = FALSE, binomial = FALSE,
  interaction_column,
  main_effect_column,
  split_effect_column,
  nlminb_loops = 0,
  newton_steps = 1,
  setseed = 42,
  par_init = NULL
  ) {

  if (binomial) student_t <- FALSE

  dat$species_id <- as.integer(as.factor(dat$species))
  dat$genus_id <- as.integer(as.factor(dat$genus))

  pred_dat <- left_join(pred_dat,
    distinct(select(dat, species, species_id)), by = "species")
  pred_dat <- left_join(pred_dat,
    distinct(select(dat, genus, genus_id)), by = "genus")

  low <- split(pred_dat, pred_dat[["species_id"]]) %>%
    purrr::map_dbl(~min(.x[[split_effect_column]])) %>% 
    as.numeric()
  high <- split(pred_dat, pred_dat[["species_id"]]) %>%
    purrr::map_dbl(~max(.x[[split_effect_column]])) %>%
    as.numeric()
  
  chop_cols <- c(
    which(interaction_column == colnames(X_pj)),
    which(main_effect_column == colnames(X_pj)))
# browser()
  # if (outliers[1] > 0 || outliers[2] < 1) {
  #   y_i <- collapse_outliers(y_i, outliers = outliers)
  #   X_ij[,2] <- collapse_outliers(X_ij[,2], outliers = outliers)
  # }

  # spde <- sdmTMB::make_mesh(dat, c("x", "y"), n_knots = knots, seed = setseed)
  spde <- sdmTMB::make_spde(dat$x, dat$y, n_knots = knots, seed = setseed)
  # map <- sdmTMB::plot(spde)
  # map
  
  n_s <- nrow(spde$mesh$loc)
  n_k <- length(unique(dat$species))
  n_m <- length(unique(dat$genus))

  data <- dat
  data$sdm_orig_id <- seq(1, nrow(data))
  data$sdm_x <- spde$x
  data$sdm_y <- spde$y
  fake_data <- unique(data.frame(sdm_x = spde$x, sdm_y = spde$y))
  fake_data[["sdm_spatial_id"]] <- seq(1, nrow(fake_data))
  data <- base::merge(data, fake_data,
    by = c("sdm_x", "sdm_y"),
    all.x = TRUE, all.y = FALSE
  )
  data <- data[order(data$sdm_orig_id), , drop = FALSE]
  A_sk <- INLA::inla.spde.make.A(spde$mesh,
    loc = as.matrix(fake_data[, c("sdm_x", "sdm_y"), drop = FALSE])
  )

  genus_index_k_df <- data.frame(species_id = dat$species_id,
    rockfish = dat$rockfish,
    genus_id = dat$genus_id) %>%
    dplyr::distinct()

  tmb_data <- list(
    y_i = y_i,
    X_ij = X_ij,
    X_pj = X_pj,
    X_k2 = cbind(low, high),
    chop_cols = chop_cols - 1L,
    A_sk = A_sk,
    A_spatial_index = data$sdm_spatial_id - 1L,
    spde = spde$spde$param.inla[c("M0", "M1", "M2")],
    k_i = dat$species_id - 1L,
    k_p = pred_dat$species_id - 1L,
    m_i = dat$genus_id - 1L,
    m_p = pred_dat$genus_id - 1L,
    n_k = n_k,
    nu = nu, # Student-t DF
    student_t = as.integer(student_t),
    binomial = as.integer(binomial),
    offset_i = offset,
    genus_index_k = genus_index_k_df$genus_id - 1L,
    rockfish_vec = which(genus_index_k_df$rockfish == "ROCKFISH") - 1L
  )

  tmb_param <- list(
    b_j = rep(0, ncol(X_ij)),
    log_gamma = rep(-1, ncol(X_ij)),
    log_gamma_genus = rep(-1, ncol(X_ij)),
    # ln_tau_O = rep(1, 1L),
    ln_tau_O = rep(1, n_k),
    ln_kappa = -2,
    ln_phi = -1,
    omega_sk = matrix(0, nrow = n_s, ncol = n_k),
    b_re = matrix(0, nrow = n_k, ncol = ncol(X_ij)),
    b_re_genus = matrix(0, nrow = n_m, ncol = ncol(X_ij))
  )

  message("Fitting fixed effects only...")
  tmb_map <- list(
    log_gamma = as.factor(rep(NA, ncol(X_ij))),
    log_gamma_genus = as.factor(rep(NA, ncol(X_ij))),
    # ln_tau_O = as.factor(rep(NA, 1L)),
    ln_tau_O = as.factor(rep(NA, n_k)),
    ln_kappa = factor(NA),
    omega_sk = as.factor(matrix(NA, nrow = n_s, ncol = n_k)),
    b_re = as.factor(matrix(NA, nrow = n_k, ncol = ncol(X_ij))),
    b_re_genus = as.factor(matrix(NA, nrow = n_m, ncol = ncol(X_ij)))
  )

  if (binomial) tmb_map <- c(tmb_map, list(ln_phi = factor(NA)))
  
  set_par_value <- function(opt, par) as.numeric(opt$par[par == names(opt$par)])
  if (is.null(par_init)) {
    obj_fe <- TMB::MakeADFun(
      data = tmb_data, parameters = tmb_param, map = tmb_map,
      random = NULL, DLL = "vocc_regression"
    )
    opt_fe <- nlminb(obj_fe$par, obj_fe$fn, obj_fe$gr,
      control = list(eval.max = 1e3, iter.max = 1e3))
    tmb_param$b_j <- set_par_value(opt_fe, "b_j")
    if (!binomial) tmb_param$ln_phi <- set_par_value(opt_fe, "ln_phi")
  }
  
  message("Fitting fixed and random effects...")

  tmb_map <- list()
  if (binomial) tmb_map <- c(tmb_map, list(ln_phi = factor(NA)))

  if (group_by_genus) {
    obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression",
      random = c("omega_sk", "b_re", "b_re_genus"), map = tmb_map)
  } else {
    tmb_map <- c(tmb_map, list(
      log_gamma_genus = as.factor(rep(NA, ncol(X_ij))),
      b_re_genus = as.factor(matrix(NA, nrow = n_m, ncol = ncol(X_ij)))
    ))
    obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression",
      random = c("omega_sk", "b_re"), map = tmb_map)
  }
  
  if (!is.null(par_init)) {
    nlminb_start <- par_init
  } else {
    nlminb_start <- obj$par
  }
  
  opt <- stats::nlminb(
    start = nlminb_start, objective = obj$fn, gradient = obj$gr,
    control = list(eval.max = 1e4, iter.max = 1e4))
  
  if (nlminb_loops > 1) {
    for (i in seq(2, nlminb_loops, length = max(0, nlminb_loops - 1))) {
      temp <-  opt[c("iterations", "evaluations")]
      opt <- stats::nlminb(
        start = opt$par, objective = obj$fn, gradient = obj$gr,
        control = list(eval.max = 1e4, iter.max = 1e4))
       opt[["iterations"]] <-  opt[["iterations"]] + temp[["iterations"]]
       opt[["evaluations"]] <-  opt[["evaluations"]] + temp[["evaluations"]]
    }
  } 
  if (newton_steps > 0) {
    for (i in seq_len(newton_steps)) {
      g <- as.numeric(obj$gr( opt$par))
      h <- optimHess(opt$par, fn =  obj$fn, gr =  obj$gr)
       opt$par <- opt$par - solve(h, g)
       opt$objective <-  obj$fn( opt$par)
    }
  }
  
  sdr <- sdreport(obj)
  
  s <- summary(sdr)

  ids_unique <- distinct(select(dat, species, species_id)) %>% arrange(species_id)
  n_spp <- nrow(ids_unique)
  n_coefs <- ncol(X_ij)
  ids <- do.call("rbind", replicate(n_coefs, ids_unique, simplify = FALSE))
  ids[["coefficient"]] <- rep(colnames(X_ij), each = n_spp)

  ids_genus <- distinct(select(dat, genus, genus_id)) %>% arrange(genus_id)
  n_genus <- nrow(ids_genus)
  ids_genus <- do.call("rbind", replicate(n_coefs, ids_genus, simplify = FALSE))
  ids_genus[["coefficient"]] <- rep(colnames(X_ij), each = n_genus)

  b_re_species <- as.data.frame(s[grep("^b_re$", row.names(s)), , drop = FALSE])
  b_re_species <- bind_cols(ids, b_re_species)

  b_re <- as.data.frame(s[grep("^combined_re$", row.names(s)), , drop = FALSE])
  b_re <- bind_cols(ids, b_re)

  if (group_by_genus) {
    b_re_genus <- as.data.frame(s[grep("^b_re_genus$", row.names(s)), , drop = FALSE])
    b_re_genus <- bind_cols(ids_genus, b_re_genus)
  } else {
    b_re_genus <- NA
  }
  # browser()
  
  # save slope estimates 
  ids_pred <- distinct(select(pred_dat, species, species_id, type)) %>% arrange(species_id)
  n_pred <- nrow(ids_pred)
  n_cols <- ncol(tmb_data$X_k2)
  d_ids <- do.call("rbind", replicate(n_cols, ids_pred, simplify = FALSE))
  d_ids[["chopstick"]] <- rep(colnames(tmb_data$X_k2), each = n_pred)
  deltas <- as.data.frame(s[grep("^delta_k$", row.names(s)), , drop = FALSE])
  deltas <- bind_cols(d_ids, deltas)
  
  r <- obj$report()
  nd <- dat
  nd$omega_s <- r$omega_sk_A_vec
  nd$eta_i <- r$eta_i
  nd$residual <- y_i - r$eta_i

  list(obj = obj, opt = opt, sdr = sdr, coefs = b_re,
    coefs_genus = b_re_genus, data = nd,
    mesh = spde, seed = setseed,
    group_by_genus = group_by_genus, 
    nu = nu, y_i = y_i, X_ij = X_ij,
    X_pj = X_pj, pred_dat = pred_dat,  
    deltas = deltas,
    tmb_data = tmb_data,
    genus_index_k_df = genus_index_k_df,
    b_re_species = b_re_species)
  
}

get_aic <- function(x, k = 2) {
  L <- -x$opt$objective
  df <- length(x$opt$par)
  if (!"b_j" %in% names(x$opt$par)) { # reml
    s <- row.names(summary(x$sdr))
    df <- df + sum(grepl("^b_j$", s))
  }
  -2 * L + k * df
}

# https://en.wikipedia.org/wiki/Location%E2%80%93scale_family
pt_ls <- function(q, df, mu, sigma) stats::pt((q - mu)/sigma, df)
qres_student <- function(object) {
  dispersion <- exp(object$opt$par[["ln_phi"]])
  y <- object$y_i
  mu <- object$data$eta_i
  u <- pt_ls(q = y, df = object$nu, mu = mu, sigma = dispersion)
  stats::qnorm(u)
}

