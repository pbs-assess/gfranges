# library(glmmTMB)
# m <- glmmTMB(log_density ~ 1 + after * source + scale(X) * scale(Y) +
#     (1 | icell) + 
#     (after * source | species), data = d, verbose = TRUE)
# 
# s <- summary(m)
# s
# 
# b <- broom.mixed::tidy(m)
# # a <- broom.mixed::augment(m)
# 
# # m$fit
# 
# r <- ranef(m)
# rd <- as.data.frame(r)
# 
# # co <- coef(m)[[1]]$species[,"after:source"]
# co <- fixef(m)[[1]][["after:source"]]
# 
# filter(rd, term == "after:source") %>% 
#   ggplot(aes(grp, condval + co, ymin = co + condval + 2*condsd, ymax = co + condval - 2*condsd)) + 
#       geom_pointrange() + 
#       coord_flip()
# 
# 
# # library(mgcv)
# library(gamm4)
# 
# m2 <- gamm4(log_density ~ after * source + s(X, Y, k=20),
#   random = ~(1 | icell) + (after * source | species),
#   data = d, verbose = 1L)

# --------------------------

# data <- d
# formula <- log_density ~ after * source
# 
# X_ij <- model.matrix(formula, data)
# mf   <- model.frame(formula, data)
# y_i  <- model.response(mf, "numeric")
# 
# spde <- sdmTMB::make_spde(d$X, d$Y, n_knots = 50)
# sdmTMB::plot_spde(spde)
# data$sdm_spatial_id <- 1:nrow(data)
# n_s <- nrow(spde$mesh$loc)
# 
# tmb_data <- list(
#   y_i        = y_i,
#   X_ij       = X_ij,
#   A          = spde$A,
#   A_spatial_index = data$sdm_spatial_id - 1L,
#   spde       = spde$spde$param.inla[c("M0","M1","M2")]
# )
# 
# tmb_params <- list(
#   b_j        = rep(0, ncol(X_ij)),
#   ln_tau_O   = 0,
#   ln_kappa   = 0,
#   ln_phi     = 0,
#   omega_s    = rep(0, n_s)
# )
# 
# tmb_random <- "omega_s"
# 
# TMB::compile("basic_spatial.cpp")
# dyn.load(dynlib("basic_spatial"))
# 
# tmb_obj <- TMB::MakeADFun(
#   data = tmb_data, parameters = tmb_params,
#   random = tmb_random, DLL = "basic_spatial")
# tmb_obj$fn()
# 
# tmb_opt <- stats::nlminb(
#   start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,
#   control = list(eval.max = 1e4, iter.max = 1e4))
# 
# sdr <- TMB::sdreport(tmb_obj)
# sdr
# 
# m <- lm(formula, data = d)
# m

# -------------------------------------------------