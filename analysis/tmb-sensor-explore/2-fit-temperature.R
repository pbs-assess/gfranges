library(tidyverse)
library(sdmTMB)

d_trawl <- readRDS("analysis/tmb-sensor-explore/sensor-data-processed.rds")

set.seed(39201114)
withheld_fe <- sample(d_trawl$fishing_event_id, size = round(nrow(d_trawl) * .2))
d_withheld <- filter(d_trawl, fishing_event_id %in% withheld_fe)
d_fit <- filter(d_trawl, !fishing_event_id %in% withheld_fe)
nrow(d_fit)
nrow(d_withheld)
unique(d_withheld$year)
unique(d_fit$year)

# FIXME: need to work out a bug in production on this new data set
# It's crashing TMB.
d_fit <- d_trawl
d_withheld <- d_trawl
spde <- make_spde(d_fit$X, d_fit$Y, n_knots = 200)
plot_spde(spde)

m_temp1 <- sdmTMB(d_fit, temperature_c ~ 0 + as.factor(year) + depth_scaled,
  # time_varying =  ~ depth_scaled + depth_scaled2,
  time = "year", spde = spde, family = gaussian(link = "identity"), ar1_fields = FALSE,
  anisotropy = FALSE, include_spatial = TRUE, silent = FALSE)

m_temp1$model$par

# 2 * plogis(m_temp1$model$par[["ar1_phi"]]) - 1
exp(m_temp1$model$par[["ln_phi"]])

predictions <- predict(m_temp1)
predictions$data$residuals <- residuals(m_temp1)

plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", colour = column)) +
    geom_point() +
    facet_wrap(~year) +
    coord_fixed()
}

plot_map(predictions$data, "est") +
  scale_colour_viridis_c() +
  ggtitle("Prediction (fixed effects + all random effects)")

plot_map(predictions$data, "est_fe") +
  ggtitle("Prediction (fixed effects only)") +
  scale_colour_viridis_c()

plot_map(predictions$data, "est_re_s") +
  ggtitle("Spatial random effects only") +
  scale_colour_gradient2()

plot_map(predictions$data, "est_re_st") +
  ggtitle("Spatiotemporal random effects only") +
  scale_colour_gradient2()

ggplot(predictions$data, aes(est, temperature_c)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  geom_abline()

ggplot(predictions$data, aes(est, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions5$data, aes(est, residuals)) +
  geom_point() +
  geom_smooth()


ggplot(predictions$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth()

ggplot(predictions$data, aes(X, Y, colour = residuals)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  scale_color_gradient2()


# Looks a bit curved, let's try with a quadratic

m_temp2 <- sdmTMB(d_fit, temperature_c ~ 0 + as.factor(year) + poly(depth_scaled, 2),
  time = "year", spde = spde, family = gaussian(link = "identity"),
  silent = FALSE)

predictions2 <- predict(m_temp2)
predictions2$data$residuals <- residuals(m_temp2)

ggplot(predictions2$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions2$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth()

ggplot(predictions2$data, aes(X, Y, colour = residuals)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  scale_color_gradient2()


# Try with a third degree polynomial:
m_temp3 <- sdmTMB(d_fit, temperature_c ~ 0 + as.factor(year) + poly(depth_scaled, 3),
  time = "year", spde = spde, family = gaussian(link = "identity"),
  silent = FALSE)

predictions3 <- predict(m_temp3)
predictions3$data$residuals <- residuals(m_temp3)

ggplot(predictions3$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions3$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth()

ggplot(predictions3$data, aes(X, Y, colour = residuals)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  scale_color_gradient2()

# That's getting pretty straight although it looks a bit heteroscedastic

# Perhaps this would work better with log temperature as the response
# since it basically isn't going to go lower than 0 degree celsius

d_fit$log_temperature_c <- log(d_fit$temperature_c)
m_temp4 <- sdmTMB(d_fit, log_temperature_c ~ 0 + as.factor(year) + poly(depth_scaled, 3),
  time = "year", spde = spde, family = gaussian(link = "identity"),
  silent = FALSE)

predictions4 <- predict(m_temp4)
predictions4$data$residuals <- residuals(m_temp4)

ggplot(predictions4$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions4$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth()

ggplot(predictions4$data, aes(X, Y, colour = residuals)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  scale_color_gradient2()

# That might've helped a little but not a lot. Let's try with depth and not log depth.

m_temp5 <- sdmTMB(d_fit, temperature_c ~ 0 + as.factor(year) + poly(depth_m, 3),
  time = "year", spde = spde, family = gaussian(link = "identity"),
  silent = FALSE)

predictions5 <- predict(m_temp5)
predictions5$data$residuals <- residuals(m_temp5)

ggplot(predictions5$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions5$data, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth()

# I don't think that helps with the heteroscedasticity but it made the curvature of the residuals a little bit worse.

ggplot(predictions5$data, aes(est, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(predictions5$data, aes(est, residuals)) +
  geom_point() +
  geom_smooth()

aic <- function(m) {
  k <- length(m$model$par)
  nll <- m$model$objective
  2 * k - 2 * (-nll)
}
aic(m_temp1)
aic(m_temp2)
aic(m_temp3)
aic(m_temp4)
aic(m_temp5)

# Conclusion: log depth, 3rd order polynomial with depth

ggplot(predictions3$data, aes(est, temperature_c)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  geom_abline()

# R2
cor(predictions3$data$est, predictions3$data$temperature_c)^2

# Extrapolate:

predictions3_nd <- predict(m_temp3, newdata = sdmTMB::qcs_grid)

ggplot(predictions3$data, aes(X, Y, colour = residuals)) +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed() +
  scale_color_gradient2()

# A short function for plotting our predictions:
plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed()
}

plot_map(predictions3_nd$data, "est") +
  scale_fill_viridis_c(trans = "sqrt", option = "C") +
  ggtitle("Prediction (fixed effects + all random effects)")

plot_map(predictions3_nd$data, "est_fe") +
  ggtitle("Prediction (fixed effects only)") +
  scale_fill_viridis_c(trans = "sqrt")

plot_map(predictions3_nd$data, "est_re_s") +
  ggtitle("Spatial random effects only") +
  scale_fill_gradient2()

plot_map(predictions3_nd$data, "est_re_st") +
  ggtitle("Spatiotemporal random effects only") +
  scale_fill_gradient2()

temp_cog <- get_cog(predictions3_nd)

temp_cog %>% reshape2::dcast(year ~ coord, value.var = "est") %>%
  ggplot(aes(X, Y, colour = year)) + geom_path(arrow = arrow()) +
  scale_color_viridis_c()

temp_cog %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~coord, scales = "free_y")
