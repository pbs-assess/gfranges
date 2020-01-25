# library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))
stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats$genus <- tolower(stats$group)

# model <- "multi-spp-biotic-vocc"
model_age <- "multi-spp-biotic-vocc-mature"
# model_age <- "scrambled-vocc-mature"
# model_age <- "scrambled2-vocc-mature"
# model_age <- "scrambled3-vocc-mature"
d <- readRDS(paste0("data/", model_age, "-with-fished.rds"))
d <- na.omit(d) %>% as_tibble()

d <- suppressWarnings(left_join(d, stats, by = "species"))

.x <- filter(d, species == "Arrowtooth Flounder")
.x <- filter(d, species == "Canary Rockfish")

library(sdmTMB)

nrow(.x)
ggplot(.x, aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2()
spde <- make_spde(x = .x$x, y = .x$y, n_knots = 80)
plot_spde(spde)
m <- sdmTMB(biotic_trend ~ 1, data = .x, spatial_only = TRUE, spde = spde, silent = TRUE)
# m

# names(s)
# s <- m$tmb_obj$simulate()
# s$omega_s_A[1]
# length(s$eta_i)
# length(s$omega_s_A)
# s$x <- .x$x
# s$y <- .x$y
# s$yobs <- s$eta_i + s$omega_s_A
# s$yobs <- s$omega_s_A

# .s <- data.frame(x = s$x, y = s$y, yobs = s$yobs)
# ggplot(.s, aes(x, y, fill = yobs)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(limits = range(c(.x$biotic_trend, s$yobs))) +
#   coord_fixed()

# exp(m$model$par[["ln_kappa"]])
# exp(m$model$par[["ln_phi"]])
# m$tmb_obj$report()$sigma_O
# names(m$tmb_obj$report())

# set.seed(1)
# s <- sdmTMB::sim(x = .x$x, y = .x$y, initial_betas = m$model$par[["b_j"]],
#   X = rep(1, nrow(.x)),
#   sigma_O = m$tmb_obj$report()$sigma_O, kappa = exp(m$model$par[["ln_kappa"]]),
#   phi = exp(m$model$par[["ln_phi"]]))

# sigma_O <- m$tmb_obj$report()$sigma_O
sigma_O <- sd(.x$biotic_trend - mean(.x$biotic_trend))
kappa <- exp(m$model$par[["ln_kappa"]])
rf_omega <- RandomFields::RMmatern(nu = 1, var = sigma_O^2, scale = 1 / kappa)
omega_s <- sdmTMB:::rf_sim(model = rf_omega, .x$x, .x$y)
omega_s <- omega_s - mean(omega_s)
# sd(omega_s)
# sigma_O
# observed <- rnorm(length(omega_s), m$model$par[["b_j"]], exp(m$model$par[["ln_phi"]]))
# observed <- rnorm(length(omega_s), omega_s + m$model$par[["b_j"]], exp(m$model$par[["ln_phi"]]))
# observed <- rnorm(length(omega_s), omega_s + mean(.x$biotic_trend), exp(m$model$par[["ln_phi"]]))
observed <- rnorm(length(omega_s), omega_s + mean(.x$biotic_trend), 0.001)
s <- data.frame(x = .x$x, y = .x$y, observed = observed)

o <- ggplot(.x, aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(limits = range(c(.x$biotic_trend, s$observed))) +
  coord_fixed()

n <- ggplot(s, aes(x, y, fill = observed)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(limits = range(c(.x$biotic_trend, s$observed))) +
  coord_fixed()

cowplot::plot_grid(o, n)

# sd(.x$biotic_trend)
# sd(s$observed)
#
# mean(.x$biotic_trend)
# mean(s$observed)

# quantile(x$biotic_trend)
# quantile(s$observed)

# spde <- make_spde(x = s$x, y = s$y, n_knots = 150)
# m2 <- sdmTMB(observed ~ 1, data = s, spatial_only = TRUE, spde = spde, silent = TRUE)
# m
# m2
