# library(TMB)
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
d <- readRDS("data/mature-all-do-untrimmed.rds")
# d <- readRDS("data/mature-all-temp-untrimmed.rds")
d <- readRDS("data/mature-all-do-dvocc.rds")
d <- na.omit(d) %>% as_tibble()

all_species <- unique(d$species)

null_number <- 3
trim_threshold <- 0.05

if (trim_threshold == 0.05) { trim_percent <- 95}
if (trim_threshold == 0.1) { trim_percent <- 90}
if (trim_threshold == 0.2) { trim_percent <- 80}
if (trim_threshold == 0.5) { trim_percent <- 50}

#####################################
### SIMULATE FAKE TREND LAYER FOR EACH SPECIES
with_nulls <- list()
for (i in seq_along(all_species)) {
 
.x <- filter(d, species == all_species[[i]])
bio5perc <- sum(.x$mean_biomass, na.rm = TRUE) * 0.01
s <- sort(.x$mean_biomass)
bio_sum <- cumsum(s)
lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
.x <- filter(.x, mean_biomass > lower_density_threshold)

# unique(.x$species)
nrow(.x)
ggplot(.x, aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2()
spde <- make_spde(x = .x$x, y = .x$y, n_knots = 200)
plot_spde(spde)
# browser()
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


# s <- sdmTMB::sim(x = .x$x, y = .x$y, initial_betas = m$model$par[["b_j"]],
#   X = rep(1, nrow(.x)),
#   sigma_O = m$tmb_obj$report()$sigma_O, kappa = exp(m$model$par[["ln_kappa"]]),
#   phi = exp(m$model$par[["ln_phi"]]))

# sigma_O <- m$tmb_obj$report()$sigma_O
set.seed(i + null_number)
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

s <- data.frame(x = .x$x, y = .x$y, fake_trend = observed)

# o <- ggplot(.x, aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(limits = range(c(.x$biotic_trend, s$fake_trend))) +
#   coord_fixed()
# 
# n <- ggplot(s, aes(x, y, fill = fake_trend)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(limits = range(c(.x$biotic_trend, s$fake_trend))) +
#   coord_fixed()
# 
# print(cowplot::plot_grid(o, n))

.s <- left_join(.x, s)
with_nulls[[i]] <- .s
}

newdata <- do.call(rbind, with_nulls)

# saveRDS(newdata, file = paste0("data/mature-all-temp-with-null-1-untrimmed.rds"))
saveRDS(newdata, file = paste0("data/mature-all-do-with-null-", null_number, "-untrimmed.rds"))




#####################################
### FAILED ATTEMPT TO SIMULATE FAKE VELOCITY (without a true gradient) LAYER FOR EACH SPECIES
# with_nulls <- list()
# for (i in seq_along(all_species)) {
#   
#   .x <- filter(d, species == all_species[[i]])
#   bio5perc <- sum(.x$mean_biomass, na.rm = TRUE) * 0.01
#   s <- sort(.x$mean_biomass)
#   bio_sum <- cumsum(s)
#   lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
#   .x <- filter(.x, mean_biomass > lower_density_threshold)
# 
#   nrow(.x)
#   ggplot(.x, aes(x, y, fill = biotic_vel)) + geom_tile(width = 4, height = 4) +
#     scale_fill_gradient2()
#   spde <- make_spde(x = .x$x, y = .x$y, n_knots = 200)
#   plot_spde(spde)
#   # browser()
#   .x$biotic_vel <- collapse_outliers(.x$biotic_vel, c(0.005, 0.995))
#   m <- sdmTMB(biotic_vel ~ 1, data = .x, family = student(link = "identity"), 
#     spatial_only = TRUE, spde = spde, silent = F)
#   # m
#   set.seed(i + null_number)
#   sigma_O <- sd(.x$biotic_vel - mean(.x$biotic_vel))
#   # sigma_O <-  exp(m$model$par[["ln_phi"]])
#   kappa <- exp(m$model$par[["ln_kappa"]])
#   # exp(m$model$par[["ln_tau_O"]])
#   rf_omega <- RandomFields::RMmatern(nu = 1, var = sigma_O^2, scale = 1/kappa)
#   # rf_omega1 <- RandomFields::RPt(rf_omega, nu = 1)
#   
#   omega_fun <- function(model, x, y) {
#     suppressMessages(RandomFields::RFsimulate(model, x, y, n=1)$variable1 )}
#   omega_s <- omega_fun(rf_omega, .x$x,  .x$y) # sdmTMB:::rf_sim(model = rf_omega, .x$x, .x$y)
#   omega_s <- omega_s - mean(omega_s)
#   # observed <- rt(length(omega_s), 4)
#   
#   observed <- rnorm(length(omega_s), omega_s + mean(.x$biotic_trend), 0.001)
#   
#   s <- data.frame(x = .x$x, y = .x$y, fake_vel = observed)
#   
#   o <- ggplot(.x, aes(x, y, fill = biotic_vel)) + geom_tile(width = 4, height = 4) +
#     scale_fill_gradient2(limits = range(c(.x$biotic_vel, s$fake_vel))) +
#     coord_fixed()
#   n <- ggplot(s, aes(x, y, fill = fake_vel)) + geom_tile(width = 4, height = 4) +
#     scale_fill_gradient2(limits = range(c(.x$biotic_vel, s$fake_vel))) +
#     coord_fixed()
# 
#   print(cowplot::plot_grid(o, n))
#   # browser()
#   .s <- left_join(.x, s)
#   with_nulls[[i]] <- .s
# }
# 
# newvel <- do.call(rbind, with_nulls)
# 
# # saveRDS(newdata, file = paste0("data/mature-all-temp-with-null-1-untrimmed.rds"))
# saveRDS(newvel, file = paste0("data/mature-all-do-with-null-", null_number, "-newvel.rds"))
# 
# # newdata <- newvel 




##########################################
### TRIM EACH SPECIES LAYERS TO INCLUDE PROPORTION OF MEAN TOTAL BIOMASS

# newdata <- readRDS("data/mature-all-do-with-null-1-untrimmed.rds")

trimmed.dat <- list()
for (i in seq_along(all_species)) {
  .x <- filter(newdata, species == all_species[[i]])
  bio5perc <- sum(.x$mean_biomass, na.rm = TRUE) * trim_threshold
  s <- sort(.x$mean_biomass)
  bio_sum <- cumsum(s)
  lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
  trimmed.dat[[i]] <- filter(.x, mean_biomass > lower_density_threshold)
}
data <- do.call(rbind, trimmed.dat)

# saveRDS(data, file = paste0("data/mature-", trim_percent, "-all-temp-with-null-", null_number, ".rds"))
saveRDS(data, file = paste0("data/mature-", trim_percent, "-all-do-with-null-", null_number, ".rds"))




#####################################
### PLOT REAL AND FAKE TREND DATA
data <- readRDS("data/mature-95-all-do-with-null-3.rds")

plots <- list()
for (i in seq_along(all_species)) {

  .x <- filter(data, species == all_species[[i]])

o <- ggplot(.x, aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(limits = range(c(.x$biotic_trend, .x$fake_trend)), guide=FALSE) + 
  xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
  coord_fixed() + ggtitle(paste(all_species[[i]])) + theme_void ()


n <- ggplot(.x, aes(x, y, fill = fake_trend)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(limits = range(c(.x$biotic_trend, .x$fake_trend)), guide=FALSE) + 
  xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
  coord_fixed() + ggtitle(paste(" ")) + theme_void ()

# t <- ggplot(.x, aes(x, y, fill = temp_trend)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(guide=FALSE) + 
#   xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
#   coord_fixed() + ggtitle(paste(" "))
# 
# d <- ggplot(.x, aes(x, y, fill = DO_trend)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2( guide=FALSE) + 
#   xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
#   coord_fixed() + ggtitle(paste(" "))

plots[[i]] <- cowplot::plot_grid(o, n) 
}

pdf(paste0("null-", null_number, "-trends.pdf"))
plots
dev.off()



#####################################
#### PLOT REAL AND FAKE VELOCITY DATA
data$fake_vel <- data$fake_trend / data$biotic_grad
data$fake_vel <- collapse_outliers(data$fake_vel, c(0.005, 0.995))
data$biotic_vel <- collapse_outliers(data$biotic_vel, c(0.005, 0.995))
data$temp_vel <- collapse_outliers(data$temp_vel, c(0.005, 0.995))
data$DO_vel <- collapse_outliers(data$DO_vel, c(0.005, 0.995))

plots2 <- list()
for (i in seq_along(all_species)) {
  
  .x <- filter(data, species == all_species[[i]])
  
  o <- ggplot(.x, aes(x, y, fill = biotic_vel)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2(limits = range(c(.x$biotic_vel, .x$fake_vel)), guide=FALSE) + 
    xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
    coord_fixed() + theme_void () + ggtitle(paste(all_species[[i]]), subtitle = "biotic vel")
  
  n <- ggplot(.x, aes(x, y, fill = fake_vel)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2(limits = range(c(.x$biotic_vel, .x$fake_vel)), guide=FALSE) + 
    xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
    coord_fixed() + theme_void () + ggtitle(paste(" "), subtitle = "fake vel")
  
  t <- ggplot(.x, aes(x, y, fill = temp_grad)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2(guide=FALSE) + 
    xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
    coord_fixed() + theme_void () + ggtitle(paste(" "), subtitle = "temp grad")
  
  d <- ggplot(.x, aes(x, y, fill = biotic_grad)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2( guide=FALSE) + 
    xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
    coord_fixed() + theme_void () + ggtitle(paste(" "), subtitle = "biotic grad")
  
  plots2[[i]] <- cowplot::plot_grid(o, n, t, d) 
}

pdf(paste0("null-", null_number, "-newvel.pdf"))
  plots2
dev.off()




# plots_90 <- plots

### PLOT REAL BIOTIC GRADIENTS
# grad_plots <- list()
# for (i in seq_along(all_species)) {
#   
#   .x <- filter(newdata, species == all_species[[i]])
#   
#   o <- ggplot(.x, aes(x, y, fill = biotic_grad)) + 
#     geom_tile(width = 4, height = 4) +  
#     scale_fill_gradient2(trans = fourth_root_power) +
#    # limits = range(c(.x$biotic_trend, .x$fake_trend))) + 
#     xlim(min(newdata$x), max(newdata$x)) + ylim(min(newdata$y), max(newdata$y)) +
#     coord_fixed() + ggtitle(paste(all_species[[i]]))
#   
#   n <- ggplot(.x, aes(x, y, fill = temp_grad)) + 
#     geom_tile(width = 4, height = 4) + scale_fill_gradient2() + 
#     xlim(min(data$x), max(data$x)) + ylim(min(data$y), max(data$y)) +
#     coord_fixed() + ggtitle(paste(" "))
# 
#   grad_plots[[i]] <- cowplot::plot_grid(o, n) 
# }
# 
# 
# grad_plots 


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
