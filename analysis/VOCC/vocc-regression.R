library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))

d <- readRDS("~/Downloads/multi-spp-biotic-vocc.rds")
d <- na.omit(d) %>% as_tibble()

X_ij <- model.matrix(~scale(temp_vel), data = d)
# X_ij <- model.matrix(~scale(DO_vel), data = d)
hist(X_ij[,2], breaks = 100)
range(X_ij[,2])

y_i <- d$biotic_vel
# y_i <- d$biotic_trend
# y_i <- log(d$biotic_CV)
hist(y_i, breaks = 100)
range(y_i)

# ggplot(d, aes(x, y, colour = temp_vel)) + geom_point() +
#   facet_wrap(~species) +
#   scale_color_viridis_c()
# 
# ggplot(d, aes(x, y, colour = biotic_vel)) + geom_point() +
#   facet_wrap(~species) +
#   scale_color_viridis_c()

# -----------------------------

d$species_id <- as.integer(as.factor(d$species))

spde <- sdmTMB::make_spde(d$x, d$y, n_knots = 150)
# sdmTMB::plot_spde(spde)

n_s <- nrow(spde$mesh$loc)
n_k <- length(unique(d$species))

data <- d
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

tmb_data <- list(
  y_i = y_i,
  X_ij = X_ij,
  A_sk = A_sk,
  A_spatial_index = data$sdm_spatial_id - 1L,
  spde = spde$spde$param.inla[c("M0", "M1", "M2")],
  k_i = d$species_id - 1L,
  n_k = n_k,
  nu = 7 # Student-t DF
)
tmb_param <- list(
  b_j = rep(0, ncol(X_ij)),
  log_gamma = rep(-1, ncol(X_ij)),
  ln_tau_O = 1,
  ln_kappa = -2,
  ln_phi = -1,
  omega_sk = matrix(0, nrow = n_s, ncol = n_k),
  b_re = matrix(0, nrow = n_k, ncol = ncol(X_ij))
)

obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression", random = c("b_j", "omega_sk", "b_re"))
opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(eval.max = 1e4, iter.max = 1e4))
sdr <- sdreport(obj)
sdr

s <- summary(sdr)

mutate(as.data.frame(s[row.names(s) == "b_j", ]), coefficient = colnames(X_ij)) %>%
  select(coefficient, Estimate, `Std. Error`)

s[grep("ln|log", row.names(s)), ]
s[grep("sigma", row.names(s)), , drop = FALSE]

ids <- distinct(select(d, species, species_id)) %>% arrange(species_id)
n_spp <- nrow(ids)
n_coefs <- ncol(X_ij)
ids <- do.call("rbind", replicate(n_coefs, ids, simplify = FALSE))
ids[["coefficient"]] <- rep(colnames(X_ij), each = n_spp)

b_re <- as.data.frame(s[grep("b_re", row.names(s)), , drop = FALSE])
b_re <-  bind_cols(ids, b_re)
b_re

