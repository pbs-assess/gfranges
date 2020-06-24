# .rs.restartR()
# library(TMB)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sdmTMB)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
null_number <- 5
trim_threshold <- 0.05

if (trim_threshold == 0.05) { trim_percent <- 95}
if (trim_threshold == 0.1) { trim_percent <- 90}
if (trim_threshold == 0.2) { trim_percent <- 80}
if (trim_threshold == 0.5) { trim_percent <- 50}

# age <- "mature"
# d1 <- readRDS(paste0("data/", age, "-all-do-dvocc.rds")) %>% mutate(age = "mature")
# 
# age <- "immature"
# d2 <- readRDS(paste0("data/", age, "-all-do-dvocc.rds")) %>% mutate(age = "immature")

age <- "mature"
d1 <- readRDS(paste0("data/", age, "-all-do-newclim3.rds")) %>% mutate(age = "mature")

age <- "immature"
d2 <- readRDS(paste0("data/", age, "-all-do-newclim3.rds")) %>% mutate(age = "immature")


d <- rbind(d1, d2) 

d <- na.omit(d) %>% as_tibble() %>% mutate(species_age = paste(age, species))

d <- filter(d, species_age != "immature Shortraker Rockfish") 
d <- filter(d, species_age != "immature Curlfin Sole")

all_species <- unique(d$species_age)


### ADD SPECIES TRAITS 
stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")

stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
# stats$group[stats$group == "SHARK"] <- "SHARKS & SKATES"
# stats$group[stats$group == "SKATE"] <- "SHARKS & SKATES"
stats$group[stats$group == "HAKE"] <- "COD"


# d <- na.omit(d) %>% as_tibble()
d <- suppressWarnings(left_join(d, stats, by = "species")) 
select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>%
  as.data.frame()



#####################################
### SIMULATE FAKE TREND LAYER FOR EACH SPECIES
with_nulls <- list()
for (i in seq_along(all_species)) {
 
.x <- filter(d, species_age == all_species[[i]])

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
# observed <- rnorm(length(omega_s), omega_s + mean(.x$biotic_trend), 0.001)
observed <- omega_s + mean(.x$biotic_trend)

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
# saveRDS(newdata, file = paste0("data/", age, "-all-do-with-null-", null_number, "-untrimmed.rds"))
saveRDS(newdata, file = paste0("data/all-biotic-null-", null_number, "-untrimmed.rds"))


##########################################
### TRIM EACH SPECIES LAYERS TO INCLUDE PROPORTION OF MEAN TOTAL BIOMASS
# age <- "mature"
# age <- "immature"
# newdata <- readRDS(paste0("data/", age, "-all-do-with-null-", null_number, "-untrimmed-allvars.rds"))
newdata <- readRDS(paste0("data/all-biotic-null-", null_number, "-untrimmed.rds"))

trimmed.dat <- list()
for (i in seq_along(all_species)) {
  .x <- filter(newdata, species_age == all_species[[i]])
  bio5perc <- sum(.x$mean_biomass, na.rm = TRUE) * trim_threshold
  s <- sort(.x$mean_biomass)
  bio_sum <- cumsum(s)
  lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
  trimmed.dat[[i]] <- filter(.x, mean_biomass > lower_density_threshold)
}
data <- do.call(rbind, trimmed.dat)

#### add in globally scaled and squashed climate data 
vars <- readRDS(("data/all-newclim-untrimmed.rds")) %>% select(
    -fishing_trend,  -mean_effort,  -fishing_vel,  -fishing_grad,
    -DO_vel,  -DO_dvocc,  -DO_trend,  -DO_grad,  -mean_DO,  -dvocc_both,
    -temp_vel,  -temp_dvocc,  -temp_trend,  -temp_grad,  -mean_temp)

data <- left_join(data, vars)

### TRIM CELLS THAT EXCEED MEAN CONDITIONS MEASURED
# # test filter of cells by observed depth, DO and temp values 
# range(survey_sets$depth_m, na.rm = T) 
#   18 1308
# quantile(survey_sets$depth_m, c(0.005, 0.995), na.rm = T)
# 0.5% 99.5% 
#   23  1112 
# alldata2 <- alldata %>% filter(depth > 18) %>% filter(depth < 1308)
data <- data %>% filter(depth > 23) %>% filter(depth < 1112) # 99th
# alldata2 <- alldata %>% filter(depth > 31) %>% filter(depth < 523.8) # 95th
# alldata <- alldata %>% filter(mean_DO > 0.23) %>% filter(mean_DO < 7.91) # full range
data <- data %>%
  filter(mean_DO > 0.28) %>%
  filter(mean_DO < 7.06) # 0.005 and 0.995
# alldata <- alldata2 %>% filter(mean_temp > 2.61) %>% filter(mean_temp < 14.31) # full range
data <- data %>%
  filter(mean_temp > 3.07) %>%
  filter(mean_temp < 11.3) # 0.005 and 0.995

saveRDS(data, file = paste0("data/all-", trim_percent, "-newclim-more2016-with-null-", null_number, ".rds"))
# saveRDS(data, file = paste0("data/mature-", trim_percent, "-all-temp-with-null-", null_number, ".rds"))
# saveRDS(data, file = paste0("data/", age, "-", trim_percent, "-all-do-with-null-", null_number, ".rds"))

# ggplot(data = data, aes(mean_DO_scaled, log(mean_biomass+1))) + 
#   geom_point(alpha= .2) + facet_wrap(~species)
# ggplot(data = data, aes(mean_temp_scaled, log(mean_biomass+1))) + 
#   geom_point(alpha= .2)  + facet_wrap(~species)
ggplot(data, aes(mean_DO_scaled, DO_trend_scaled)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~species)


#####################################
### PLOT REAL AND FAKE TREND DATA
# data <- readRDS(paste0("data/", age, "-", trim_percent, "-all-newclim-with-null-", null_number, ".rds"))
data <- readRDS(paste0("data/all-", trim_percent, "-newclim-more2016-with-null-", null_number, ".rds"))

plots <- list()
for (i in seq_along(all_species)) {

  .x <- filter(data, species_age == all_species[[i]])

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

# pdf(paste0(age, "-null-", null_number, "-trends-new.pdf"))
pdf(paste0("all-null-", null_number, "-trends-newclim2.pdf"))

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
  
  .x <- filter(data, species_age == all_species[[i]])
  
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

pdf(paste0(age, "-null-", null_number, "-newvel.pdf"))
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
