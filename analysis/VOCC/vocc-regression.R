library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))

# model <- "multi-spp-biotic-vocc"
model <- "multi-spp-biotic-vocc-mature"
# model <- "multi-spp-biotic-vocc-immature"

d <- readRDS(paste0("data/", model, ".rds"))
d <- na.omit(d) %>% as_tibble()

stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats$genus <- tolower(stats$group)

d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  filter(species != "Longspine Thornyhead") %>% 
  #filter(species != "Sand Sole") %>% 
  filter(species != "Bocaccio") %>% 
filter(species != "Curlfin Sole") 

select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>% 
  as.data.frame()

# ggplot(d, aes(x, y, colour = temp_vel)) + geom_point() +
#   facet_wrap(~species) +
#   scale_color_viridis_c()
#
# ggplot(d, aes(x, y, colour = biotic_vel)) + geom_point() +
#   facet_wrap(~species) #+ scale_color_viridis_c()

source("vocc-regression-functions.R")

### Local biomass velocity from 2007-2018

# y <- d$biotic_vel
# d$temp_vel_squashed <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))
# x <- model.matrix(~ scale(temp_vel_squashed) + I(scale(temp_vel_squashed)^2), data = d)
# 
# y <- collapse_outliers(y, c(0.005, 0.995))
# hist(y, breaks = 100)
# hist(x[,2], breaks = 100)
# hist(x[,3], breaks = 100)
#
# .d <- d
# .d$y_i <- y
# .d$x_scaled <- x[,2]
# .d$x_scaled_sq <- x[,3]
# ggplot(.d, aes(x_scaled, y_i)) + facet_wrap(~species) + geom_point()
# ggplot(.d, aes(x_scaled_sq, y_i)) + facet_wrap(~species) + geom_point()
#
# cube_root <- function(x) sign(x) * abs(x)^(1/3)
# fourth_root <- function(x) sign(x) * abs(x)^(1/4)
# hist(cube_root(y), breaks = 100)
# hist(cube_root(d$temp_vel), breaks = 100)
#
# hist(fourth_root(y), breaks = 100)
# hist(fourth_root(d$temp_vel), breaks = 100)
#
# ggplot(.d, aes(fourth_root(temp_vel), fourth_root(y_i))) +
#   facet_wrap(~species) + geom_point(alpha = 0.1)
#
# y <- fourth_root(d$biotic_vel)
# x <- model.matrix(~ fourth_root(temp_vel), data = d)
# plot(x[,2], y, col = "#00000005")

# y <- d$biotic_grad
# hist(log(collapse_outliers(y, c(0, 1))), breaks = 100)
#
# hist(collapse_outliers(x[,2], c(0.001, 0.999)), breaks = 100)
# hist(collapse_outliers(y, c(0.01, 0.99)), breaks = 100);abline(v = 0)
#
# # fourth <- function(x) ifelse(x > 0, x^0.25, -(-x)^0.25)
# # hist(fourth(collapse_outliers(y, c(0.01, 0.99))), breaks = 100);abline(v = 0)
#
# y <- d$biotic_trend
# x <- model.matrix(~ scale(temp_vel), data = d)
# bio_temp <- vocc_regression(log(y), x, offset = log(d$biotic_grad),
#   knots = 200, group_by_genus = FALSE, student_t = FALSE)

# y <- d$biotic_vel
# .y <- y
# hist(y, xlim = c(-10, 10), breaks = 2000)
# lims <- c(-2, 2)
# y[.y > lims[2]] <- 1L
# y[.y < lims[1]] <- 0L
# y[.y <= lims[2] & .y >= lims[1]] <- NA
# sum(.y <= lims[2] & .y >= lims[1])/length(y)
# table(y)
#
# d2 <- d[!is.na(y), , drop = FALSE]
# y <- as.numeric(na.omit(y))
# x <- model.matrix(~ temp_vel, data = d2)

y <- collapse_outliers(d$biotic_vel, c(0.005, 0.995))
d$temp_vel_squashed <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))
d$do_vel_squashed <-  collapse_outliers(d$DO_vel, c(0.005, 0.995))
plot(scale(do_vel_squashed)~scale(temp_vel_squashed), data=d, col = "#00000010")

x <- model.matrix(~ scale(temp_vel_squashed) + scale(do_vel_squashed) + scale(do_vel_squashed):scale(temp_vel_squashed), data = d)

hist(y)
hist(x[,2])
bio_temp <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)

bio_temp_genus <- vocc_regression(d, y, x, knots = 200,
  group_by_genus = TRUE, student_t = TRUE, nu = 5)

bio_temp$sdr
bio_temp_genus$sdr

get_aic(bio_temp_genus) - get_aic(bio_temp)

# saveRDS(bio_temp, file = "data/bio_temp_mature.rds")
# saveRDS(bio_temp_genus, file = "data/bio_temp_genus_mature.rds")

# sdmTMB:::get_convergence_diagnostics(bio_temp)
# sdmTMB:::get_convergence_diagnostics(bio_temp_genus)

bio_temp2 <- add_colours(bio_temp$coefs)
bio_temp3 <- plot_coefs(bio_temp2)
bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC")) + facet_wrap(~coefficient, scales = "free_x") #
bio_temp_plot

bio_temp2 <- add_colours(bio_temp_genus$coefs)
bio_temp3 <- plot_coefs(bio_temp2)
bio_temp_plot <- bio_temp3 + ggtitle(paste("Immature biotic velocity by thermal VOCC with genus re")) #+ facet_wrap(~genus)
bio_temp_plot

library(ggsidekick) # for fourth_root_power
# ggplot(bio_temp$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(trans = "fourth_root_power") +
#   facet_wrap(~species)

ggplot(bio_temp$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + 
  facet_wrap(~species)

bio_temp$data %>%
  mutate(resid_upper = quantile(bio_temp$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(bio_temp$data$residual, probs = 0.025)) %>%  # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>% 
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + 
  facet_wrap(~species)

norm_resids <- qres_student(bio_temp)
norm_resids <- norm_resids[is.finite(norm_resids)]
# qqnorm(norm_resids)
hist(norm_resids)

norm_resids <- qres_student(bio_temp_genus)
norm_resids <- norm_resids[is.finite(norm_resids)]
# qqnorm(norm_resids)
hist(norm_resids)

#
# qqnorm(bio_temp$data$residual)
# qqline(bio_temp$data$residual)


## Ordered by increasing max weight and split by rockfish or not
# bio_temp3 <- plot_coefs(bio_temp2, order_by = "max_weight")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC ordered by increasing max weight")) +
#   facet_wrap(~rockfish, scales="free_y")
# bio_temp_plot

## Ordered by increasing max age and split by rockfish or not
# bio_temp_age <- na.omit(bio_temp2)
# bio_temp3 <- plot_coefs(bio_temp_age, order_by = "max_age")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of max age")) + facet_wrap(~rockfish, scales = "free_y")
# bio_temp_plot


# ## Ordered by increasing depth and split by rockfish or not
# bio_temp3 <- plot_coefs(bio_temp2, order_by = "large_depth")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# bio_temp_plot

x <- model.matrix(~ scale(temp_vel_squashed), data = d)

hist(y)
hist(x[,2])
bio_temp <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)

bio_temp_genus <- vocc_regression(d, y, x, knots = 200,
  group_by_genus = TRUE, student_t = TRUE, nu = 5)

bio_temp$sdr
bio_temp_genus$sdr

get_aic(bio_temp_genus) - get_aic(bio_temp)

y <- d$biotic_vel
x <- model.matrix(~ scale(DO_vel), data = d)

bio_do <- vocc_regression(y, x, knots = 100)
bio_do2 <- add_colours(bio_do$coefs)
bio_do3 <- plot_coefs(bio_do2)
bio_do_plot <- bio_do3 + ggtitle(paste("Biotic velocity by DO VOCC"))
bio_do_plot

# ## Ordered by increasing depth and split by rockfish or not
# bio_do3 <- plot_coefs(bio_do2, order_by = "large_depth")
# bio_do_plot <- bio_do3 + ggtitle(paste("Biotic velocity by DO VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# bio_do_plot



### Local biomass trend from 2007-2018

y <- d$biotic_trend
x <- model.matrix(~ scale(temp_vel), data = d)

trend_temp <- vocc_regression(y, x, knots = 100)
trend_temp2 <- add_colours(trend_temp$coefs)
trend_temp3 <- plot_coefs(trend_temp2)
trend_temp_plot <- trend_temp3 + ggtitle(paste("Biotic trend by thermal VOCC"))
trend_temp_plot


y <- d$biotic_trend
x <- model.matrix(~ scale(DO_vel), data = d)

trend_do <- vocc_regression(y, x, knots = 100)
trend_do2 <- add_colours(trend_do$coefs)
trend_do3 <- plot_coefs(trend_do2)
trend_do_plot <- trend_do3 + ggtitle(paste("Biotic trend by DO VOCC"))
trend_do_plot

### Local biomass variability from 2007-2018

y <- d$biotic_CV
x <- model.matrix(~ scale(temp_vel), data = d)

CV_temp <- vocc_regression(y, x, knots = 100)
CV_temp2 <- add_colours(CV_temp$coefs)
CV_temp3 <- plot_coefs(CV_temp2)
CV_temp_plot <- CV_temp3 + ggtitle(paste("Biotic CV by thermal VOCC"))
CV_temp_plot


y <- d$biotic_CV
x <- model.matrix(~ scale(DO_vel), data = d)

CV_do <- vocc_regression(y, x, knots = 100)
CV_do2 <- add_colours(CV_do$coefs)
CV_do3 <- plot_coefs(CV_do2)
CV_do_plot <- CV_do3 + ggtitle(paste("Biotic CV by DO VOCC"))
# CV_do_plot <- NULL

png(
  file = paste0("figs/", model, "100-knots-29-spp.png"),
  res = 600,
  units = "in",
  width = 8.5,
  height = 11
)
gridExtra::grid.arrange(
  grobs = c(list(
    bio_temp_plot,
    bio_do_plot,
    trend_temp_plot,
    trend_do_plot,
    CV_temp_plot,
    CV_do_plot
  )),
  nrow = 3,
  top = grid::textGrob(paste(model, " knots = 100"))
)
dev.off()
