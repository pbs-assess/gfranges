# ### JUST CLIMATE AND FISHING COVARIATES
library(tidyverse)
library(sdmTMB)
library(gfranges)

setwd(here::here("analysis", "VOCC"))

fished <- readRDS("data/fishing-effort-change-w-depth.rds") %>% select(-X, -Y) # remove lat lon

# DO 
do16 <- readRDS(paste0("data/_climate_vocc/DO-vocc-2020-06-20-more2016-scale-2-ssid16-years-6.rds"))
do1n3 <- readRDS(paste0("data/_climate_vocc/DO-vocc-2020-06-20-more2016-scale-2-ssid1n3-years-5.rds"))
do4 <- readRDS(paste0("data/_climate_vocc/DO-vocc-2020-06-20-more2016-scale-2-ssid4-years-6.rds"))

do_vocc <- rbind(do16, do1n3, do4)

DO_values <- do_vocc %>%
  rename(
    DO_vel = velocity,
    DO_dvocc = dvocc,
    DO_trend = trend_per_decade,
    DO_grad = gradient,
    mean_DO = mean
  ) %>%
  select(x, y, DO_vel, DO_dvocc, DO_trend, DO_grad, mean_DO, dvocc_both)

# TEMP
t16 <- readRDS(paste0("data/_climate_vocc/temp-vocc-2020-06-20-more2016-scale-2-ssid16-years-6.rds"))
t1n3 <- readRDS(paste0("data/_climate_vocc/temp-vocc-2020-06-20-more2016-scale-2-ssid1n3-years-5.rds"))
t4 <- readRDS(paste0("data/_climate_vocc/temp-vocc-2020-06-20-more2016-scale-2-ssid4-years-6.rds"))

t_vocc <- rbind(t16, t1n3, t4)

t_values <- t_vocc %>%
  rename(
    temp_vel = velocity,
    temp_dvocc = dvocc,
    temp_trend = trend_per_decade,
    temp_grad = gradient,
    mean_temp = mean
  ) %>%
  select(x, y, temp_vel, temp_dvocc, temp_trend, temp_grad, mean_temp)

d <- left_join(fished, DO_values)
d <- left_join(d, t_values)


# ANNUAL TEMP
at16 <- readRDS(paste0("data/_climate_vocc/annual-temp-vocc-2020-06-20-more2016-scale-2-ssid16-years-6.rds"))
at1n3 <- readRDS(paste0("data/_climate_vocc/annual-temp-vocc-2020-06-20-more2016-scale-2-ssid1n3-years-5.rds"))
at4 <- readRDS(paste0("data/_climate_vocc/annual-temp-vocc-2020-06-20-more2016-scale-2-ssid4-years-6.rds"))

at_vocc <- rbind(at16, at1n3, at4)

at_values <- at_vocc %>%
  rename(
    ann_temp_vel = velocity,
    # ann_temp_dvocc = dvocc,
    ann_temp_trend = trend_per_decade,
    ann_temp_grad = gradient,
    ann_mean_temp = mean
  ) %>%
  select(x, y, ann_temp_vel, #ann_temp_dvocc, 
    ann_temp_trend, ann_temp_grad, ann_mean_temp)


d <- left_join(d, at_values)


#### PREP DATA ####
d$squashed_temp_vel <- collapse_outliers(d$temp_vel, c(0.005, 0.995)) #99th quantile and then droped to be less than 100km
hist(d$squashed_temp_vel, breaks = 100)

d$squashed_ann_temp_vel <- collapse_outliers(d$ann_temp_vel, c(0.005, 0.995)) #99th quantile and then droped to be less than 100km
hist(d$squashed_ann_temp_vel, breaks = 100)

d$squashed_DO_vel <- collapse_outliers(d$DO_vel, c(0.025, 0.975)) #95th quantile and then droped to be less than 100km
hist(d$squashed_DO_vel, breaks = 100)


#### PREP TEMP VARIABLES ####
d$squashed_temp_dvocc <- collapse_outliers(d$temp_dvocc, c(0.005, 0.995))
hist(d$squashed_temp_dvocc, breaks = 100)
# plot(squashed_do_vel ~ squashed_temp_vel, data = d, col = "#00000010")
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)
d$squashed_temp_dvocc_scaled <- scale(d$squashed_temp_dvocc, center = F)
d$mean_temp_scaled2 <- scale(log(d$mean_temp))
d$mean_temp_scaled <- scale((d$mean_temp))

hist((d$mean_temp_scaled))
hist((d$mean_temp_scaled2))
hist(d$squashed_temp_vel_scaled, breaks = 100)

hist(d$temp_trend)
d$temp_trend_scaled <- scale(d$temp_trend, center = FALSE)
# hist(d$temp_trend_scaled)
d$temp_grad_scaled <- scale(d$temp_grad)
# hist(d$temp_grad_scaled)
# d$temp_grad_scaled <- scale(sqrt(d$temp_grad))
# hist(d$temp_grad_scaled)

#### PREP DO VARIABLES ####

d$squashed_DO_vel_scaled <- scale(d$squashed_DO_vel, center = FALSE)
d$squashed_DO_dvocc <- collapse_outliers(d$DO_dvocc, c(0.005, 0.995))
hist(d$squashed_DO_dvocc, breaks = 100)
# d$DO_dvocc_scaled <- scale(d$DO_dvocc, center = F)
d$squashed_DO_dvocc_scaled <- scale(d$squashed_DO_dvocc, center = FALSE)

# hist(d$mean_DO)
d$mean_DO_scaled2 <- scale(log(d$mean_DO))
d$mean_DO_scaled <- scale(d$mean_DO)
hist((d$mean_DO_scaled))
hist((d$mean_DO_scaled2))

hist(d$mean_DO_scaled)
hist(d$DO_trend)
d$squashed_DO_trend <- collapse_outliers(d$DO_trend, c(0.005, 0.995))
hist(d$squashed_DO_trend)
d$squashed_DO_trend_scaled <- scale(d$squashed_DO_trend, center = FALSE)

d$DO_trend_scaled <- scale(d$DO_trend, center = FALSE)
hist(d$DO_trend_scaled)
d$DO_grad_scaled <- scale(d$DO_grad)
hist(d$DO_grad_scaled)
# d$DO_grad_scaled <- scale(sqrt(d$DO_grad))
# hist(d$DO_grad_scaled)

#### PREP FISHING VARIABLES ####

# hist(d$mean_effort)
d$sqrt_effort <- sqrt(d$mean_effort)
d$sqrt_effort_scaled <- scale(sqrt(d$mean_effort), center = F)

d$fishing_trend_scaled <- scale(d$fishing_trend, center = F)
hist(d$fishing_trend_scaled)

# hist(sqrt(d$mean_effort))
d$log_effort <- log(d$mean_effort + 1)
hist(log(d$mean_effort + 1))
d$log_effort_scaled <- scale(d$log_effort, center = F)


saveRDS(d, file = paste0("data/all-newclim-untrimmed-dvocc-med.rds"))


cor(d$ann_temp_trend, d$temp_trend, use = "pairwise.complete.obs")

ggplot(d, aes(ann_temp_trend, temp_trend)) + 
  geom_point(alpha=0.2) +
  coord_cartesian(xlim = c(-1,2.5), ylim = c(-1,2.5)) +
  geom_abline(intercept = 0, slope = 1) +
  gfplot::theme_pbs()


ggplot(d, aes(squashed_ann_temp_vel, squashed_temp_vel)) + 
  geom_point(alpha=0.2) +
  # coord_cartesian(xlim = c(-1,2.5), ylim = c(-1,2.5)) +
  geom_abline(intercept = 0, slope = 1) +
  gfplot::theme_pbs()

ggplot(d, aes(ann_temp_grad, temp_grad)) + 
  geom_point(alpha=0.2) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1) +
  gfplot::theme_pbs()

ggplot(d, aes(abs(temp_dvocc), abs(squashed_temp_vel))) + 
  geom_point(alpha=0.2) +
  # coord_cartesian(xlim = c(-1,2.5), ylim = c(-1,2.5)) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1) +
  gfplot::theme_pbs()

ggplot(d, aes(abs(DO_dvocc), abs(squashed_DO_vel))) + 
  geom_point(alpha=0.2) +
  geom_smooth() +
  # coord_cartesian(xlim = c(-1,2.5), ylim = c(-1,2.5)) +
  geom_abline(intercept = 0, slope = 1) +
  gfplot::theme_pbs()


hist(d$DO_dvocc , breaks = 100)
