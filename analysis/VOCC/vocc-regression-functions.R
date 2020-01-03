collapse_outliers <- function(.x, outliers) {
  .x_max <- quantile(.x, outliers[2])
  .x_min <- quantile(.x, outliers[1])
  .x[.x > .x_max] <- .x_max
  .x[.x < .x_min] <- .x_min
  .x
}

#' @param y_i Response vector
#' @param X_ij Covariate matrix
#' @param offset Optional offset vector
#' @param knots Number of SPDE knots
#' @param nu Student-t degrees of freedom parameter (fixed)
#' @param student_t Logical for Student-t distribution. If `FALSE`, then a
#'   normal distribution observation model is used.
#' @param group_by_genus Logical. If `TRUE`, a hierarchical random effect
#'   structure is used.
vocc_regression <- function(dat, y_i, X_ij, offset = rep(0, length(y_i)),
  knots = 200, nu = 7, student_t = TRUE,
  group_by_genus = FALSE, binomial = FALSE) {

  if (binomial) student_t <- FALSE

  dat$species_id <- as.integer(as.factor(dat$species))
  dat$genus_id <- as.integer(as.factor(dat$genus))

  # if (outliers[1] > 0 || outliers[2] < 1) {
  #   y_i <- collapse_outliers(y_i, outliers = outliers)
  #   X_ij[,2] <- collapse_outliers(X_ij[,2], outliers = outliers)
  # }

  spde <- sdmTMB::make_spde(dat$x, dat$y, n_knots = knots)
  # map <- sdmTMB::plot_spde(spde)

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

  genus_index_k_df <- data.frame(species_id = dat$species_id, genus_id = dat$genus_id) %>% 
    dplyr::distinct()
  
  tmb_data <- list(
    y_i = y_i,
    X_ij = X_ij,
    A_sk = A_sk,
    A_spatial_index = data$sdm_spatial_id - 1L,
    spde = spde$spde$param.inla[c("M0", "M1", "M2")],
    k_i = dat$species_id - 1L,
    m_i = dat$genus_id - 1L,
    n_k = n_k,
    nu = nu, # Student-t DF
    student_t = as.integer(student_t),
    binomial = as.integer(binomial),
    offset_i = offset,
    genus_index_k = genus_index_k_df$genus_id - 1L
  )

  tmb_param <- list(
    b_j = rep(0, ncol(X_ij)),
    log_gamma = rep(-1, ncol(X_ij)),
    log_gamma_genus = rep(-1, ncol(X_ij)),
    ln_tau_O = rep(1, 1L),
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
    ln_tau_O = as.factor(rep(NA, 1L)),
    ln_kappa = factor(NA),
    omega_sk = as.factor(matrix(NA, nrow = n_s, ncol = n_k)),
    b_re = as.factor(matrix(NA, nrow = n_k, ncol = ncol(X_ij))),
    b_re_genus = as.factor(matrix(NA, nrow = n_m, ncol = ncol(X_ij)))
  )

  if (binomial) tmb_map <- c(tmb_map, list(ln_phi = factor(NA)))
  obj_fe <- TMB::MakeADFun(
    data = tmb_data, parameters = tmb_param, map = tmb_map,
    random = NULL, DLL = "vocc_regression"
  )
  opt_fe <- nlminb(obj_fe$par, obj_fe$fn, obj_fe$gr)
  set_par_value <- function(opt, par) as.numeric(opt$par[par == names(opt$par)])
  tmb_param$b_j <- set_par_value(opt_fe, "b_j")
  if (!binomial) tmb_param$ln_phi <- set_par_value(opt_fe, "ln_phi")

  message("Fitting fixed and random effects...")

  tmb_map <- list()
  if (binomial) tmb_map <- c(tmb_map, list(ln_phi = factor(NA)))

  if (group_by_genus) {
    obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression",
      random = c("b_j", "omega_sk", "b_re", "b_re_genus"), map = tmb_map)
  } else {
    tmb_map <- c(tmb_map, list(
      log_gamma_genus = as.factor(rep(NA, ncol(X_ij))),
      b_re_genus = as.factor(matrix(NA, nrow = n_m, ncol = ncol(X_ij)))
    ))
    obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression",
      random = c("b_j", "omega_sk", "b_re"), map = tmb_map)
  }
  opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(eval.max = 1e4, iter.max = 1e4))
  sdr <- sdreport(obj)
  sdr

  s <- summary(sdr)

  # mutate(as.data.frame(s[row.names(s) == "b_j", ]), coefficient = colnames(X_ij)) %>%
  #   select(coefficient, Estimate, `Std. Error`)
  #
  # s[grep("ln|log", row.names(s)), ]
  # s[grep("sigma", row.names(s)), , drop = FALSE]

  ids <- distinct(select(dat, species, species_id)) %>% arrange(species_id)
  n_spp <- nrow(ids)
  n_coefs <- ncol(X_ij)
  ids <- do.call("rbind", replicate(n_coefs, ids, simplify = FALSE))
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

  r <- obj$report()
  nd <- dat
  nd$omega_s <- r$omega_sk_A_vec
  nd$eta_i <- r$eta_i
  nd$residual <- nd$biotic_vel - r$eta_i

  list(obj = obj, opt = opt, sdr = sdr, coefs = b_re, coefs_genus = b_re_genus, data = nd,
    group_by_genus = group_by_genus, nu = nu, y_i = y_i, X_ij = X_ij, b_re_species = b_re_species)
}

add_colours <- function(coefs, species_data = stats, add_spp_data = TRUE, manual_colours = TRUE) {
  if (add_spp_data) {
    coefs <- left_join(coefs, species_data)
  }

  if (manual_colours) {

    # sort(unique(d$species))
    # sort(unique(coefs$species))

    species <- c(
      "Arrowtooth Flounder",
      "Canary Rockfish",
      "Curlfin Sole",
      "Darkblotched Rockfish",
      "Dover Sole",
      "English Sole",
      "Flathead Sole",
      "Greenstriped Rockfish",
      "Lingcod",
      "Longspine Thornyhead",
      "North Pacific Spiny Dogfish",
      "Pacific Cod",
      "Pacific Halibut",
      "Pacific Ocean Perch",
      "Petrale Sole",
      "Quillback Rockfish",
      "Redbanded Rockfish",
      "Rex Sole",
      "Sablefish",
      "Sand Sole",
      "Sharpchin Rockfish",
      "Shortspine Thornyhead",
      "Silvergray Rockfish",
      "Southern Rock Sole",
      "Splitnose Rockfish",
      "Walleye Pollock",
      "Widow Rockfish",
      "Yelloweye Rockfish",
      "Yellowmouth Rockfish",
      "Yellowtail Rockfish"
    )

    #   ## To choose specific colours for specific species
    #   # RColorBrewer::brewer.pal(n = 10, name = 'Spectral')
    #   # RColorBrewer::display.brewer.pal(n = 10, name = 'Spectral')
    #   # gfutilities::rich.colors(n = 20, alpha = 1)

    colours <- c(
      "#3288BD", # "Arrowtooth Flounder",
      "#9E0142", # "Canary Rockfish",
      "#3288BD", # "Curlfin Sole",
      "#9E0142", # "Darkblotched Rockfish",
      "#3288BD", # "Dover Sole",
      "#3288BD", # "English Sole",
      "#3288BD", # "Flathead Sole",
      "#9E0142", # "Greenstriped Rockfish",
      "#66C2A5", # ""Lingcod",
      "#D53E4F", # "Longspine Thornyhead",
      "#FDAE61", # "North Pacific Spiny Dogfish",
      "#ABDDA4", # ""Pacific Cod",
      "#5E4FA2", # "Pacific Halibut",
      "#9E0142", # "Pacific Ocean Perch",
      "#3288BD", # "Petrale Sole",
      "#9E0142", # "Quillback Rockfish",
      "#9E0142", # "Redbanded Rockfish",
      "#3288BD", # "Rex Sole",
      "#ABDDA4", # ""Sablefish",
      "#3288BD", # "Sand Sole",
      "#9E0142", # "Sharpchin Rockfish",
      "#D53E4F", # "Shortspine Thornyhead",
      "#9E0142", # "Silvergray Rockfish",
      "#3288BD", # "Southern Rock Sole",
      "#9E0142", # "Splitnose Rockfish",
      "#ABDDA4", # "Walleye Pollock",
      "#9E0142", # "Widow Rockfish",
      "#9E0142", # "Yelloweye Rockfish",
      "#9E0142", # "Yellowmouth Rockfish",
      "#9E0142" # "Yellowtail Rockfish"
    )

    colour_key <- as_tibble(cbind(species, colours))
    colour_key$species <- as.factor(colour_key$species)

    out <- left_join(coefs, colour_key, by = "species")
    missing_colours <- out$species[is.na(out$colours)]

    if (length(missing_colours) > 0) {
      stop(paste(missing_colours, "need a colour assigned."))
    }
  } else {
    N <- length(unique(coefs$species))
    # species <- unique(coefs$species)
    species <- levels(coefs$species)
    colours <- gfutilities::rich.colors(n = N, alpha = 1)
    colour_key <- as_tibble(cbind(species, colours))
    colour_key$species <- as.factor(colour_key$species)
    out <- left_join(coefs, colour_key, by = "species")
  }
  out <- arrange(out, species)
  out
}

plot_coefs <- function(coloured_coefs, order_by = "Estimate") {

  # coloured_coefs <- out
  coloured_coefs <- filter(coloured_coefs, coefficient != "(Intercept)")
  coloured_coefs$order_by <- coloured_coefs[[order_by]]
  colour_list <- coloured_coefs$colours # c(unique(b_re$colours))


  p <- ggplot(coloured_coefs, aes(
    forcats::fct_reorder(species, -order_by), #-Estimate),
    Estimate,
    colour = species,
    ymin = Estimate + qnorm(0.025) * `Std. Error`,
    ymax = Estimate + qnorm(0.975) * `Std. Error`
  )) +
    geom_hline(yintercept = 0, colour = "darkgray") +
    scale_colour_manual(values = colour_list) +
    geom_pointrange() + coord_flip() + xlab("") +
    # facet_wrap(~group) +
    theme(legend.position = "none")
  p
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
