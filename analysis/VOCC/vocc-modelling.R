library(dplyr)
library(ggplot2)
setwd(here::here())

library(TMB)
files <- list.files("../rockfish-vocc-temp/perc_50/1/", full.names = TRUE)
.d <- purrr::map_dfr(files, readRDS)

setwd("analysis/VOCC/")

d <- select(.d, species, log_density, after, cell_type, log_depth, icell, start_time, X, Y, vect_dist)
d <- mutate(d, source = ifelse(cell_type == "source", 1, 0))

unique(d$start_time)

d <- filter(d, start_time == "2013")
nrow(d)

ggplot(d, aes(X, Y, colour = cell_type)) + geom_point(size = 0.1, alpha = 0.3) +
  facet_wrap(~species) + coord_fixed()

ggplot(d, aes(X, Y, colour = log_density)) + geom_point(size = 0.1, alpha = 0.3) +
  facet_wrap(~species) + coord_fixed() + scale_color_viridis_c()

# 'scratch' code was here.

data <- d
# formula <- log_density ~ after * source + scale(log_depth) + as.factor(start_time)
formula <- log_density ~ after * source + scale(log_depth) + scale(vect_dist)
species_k <- as.integer(as.factor(d$species))
cell_m <- as.integer(as.factor(d$icell))

X_ij <- model.matrix(formula, data)
head(X_ij)
mf <- model.frame(formula, data)
y_i <- model.response(mf, "numeric")

spde <- sdmTMB::make_spde(d$X, d$Y, n_knots = 120)
sdmTMB::plot_spde(spde)
# data$sdm_spatial_id <- 1:nrow(data)
n_s <- nrow(spde$mesh$loc)
n_k <- length(unique(species_k))
n_re <- 4

data$sdm_orig_id <- seq(1, nrow(data))
data$sdm_x <- spde$x
data$sdm_y <- spde$y
fake_data <- unique(data.frame(sdm_x = spde$x, sdm_y = spde$y))
fake_data[["sdm_spatial_id"]] <- seq(1, nrow(fake_data))
data <- base::merge(data, fake_data, by = c("sdm_x", "sdm_y"),
  all.x = TRUE, all.y = FALSE)
data <- data[order(data$sdm_orig_id),, drop=FALSE]
A_sk <- INLA::inla.spde.make.A(spde$mesh,
  loc = as.matrix(fake_data[, c("sdm_x", "sdm_y"), drop = FALSE]))

tmb_data <- list(
  y_i = y_i,
  X_ij = X_ij,
  A_sk = A_sk,
  A_spatial_index = data$sdm_spatial_id - 1L,
  spde = spde$spde$param.inla[c("M0", "M1", "M2")],
  k_i = species_k - 1L,
  intercept_i = rep(1, nrow(d)),
  after_i = d$after,
  source_i = d$source,
  n_k = n_k,
  m_i = cell_m - 1L,
  interaction_position = grep("after:source", colnames(X_ij)) - 1
)

tmb_params <- list(
  b_j = rep(0, ncol(tmb_data$X_ij)),
  ln_tau_E = -1,
  ln_kappa = -3,
  ln_phi = 2.7,
  epsilon_sk = matrix(0, nrow = n_s, ncol = n_k),
  b_re = matrix(0, nrow = n_k, ncol = n_re),
  log_gamma = c(2.5, 2, 2, -2),
  b_cell = rep(0, length(unique(tmb_data$m_i))),
  log_varphi = -5
)

TMB::compile("basic_spatial_re.cpp")
dyn.load(dynlib("basic_spatial_re"))

tmb_map <- list(
  ln_tau_E = as.factor(NA),
  ln_kappa = as.factor(NA),
  epsilon_sk = factor(rep(NA, length(tmb_params$epsilon_sk))),
  b_re = factor(matrix(NA, nrow = n_k, ncol = n_re)),
  log_gamma = factor(rep(NA, length(tmb_params$log_gamma))),
  log_varphi = as.factor(NA),
  b_cell = factor(rep(NA, length(tmb_params$b_cell)))
)

tmb_obj <- TMB::MakeADFun(
  data = tmb_data, parameters = tmb_params, map = tmb_map,
  random = NULL, DLL = "basic_spatial_re"
)

tmb_opt <- stats::nlminb(
  start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,
  control = list(eval.max = 1e4, iter.max = 1e4)
)

set_par_value <- function(opt, par) {
  as.numeric(opt$par[par == names(opt$par)])
}
tmb_params$b_j <- set_par_value(tmb_opt, "b_j")
tmb_params$ln_phi <- set_par_value(tmb_opt, "ln_phi")

tmb_random <- c("epsilon_sk", "b_re", "b_cell")

tmb_obj <- TMB::MakeADFun(
  data = tmb_data, parameters = tmb_params,
  random = tmb_random, DLL = "basic_spatial_re"
)

tmb_opt <- stats::nlminb(
  start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,
  control = list(eval.max = 1e4, iter.max = 1e4)
)

sdr <- TMB::sdreport(tmb_obj)
sdr
colnames(X_ij)

s <- summary(sdr)
s

co <- as.data.frame(s[row.names(s) == "b_baci_interaction", ])
co$species <- as.character(unique(as.factor(d$species)))
ggplot(co, aes(species, Estimate,
  ymin = Estimate - 2 * `Std. Error`, ymax = Estimate + 2 * `Std. Error`
)) +
  geom_pointrange() + coord_flip()
