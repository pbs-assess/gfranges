### REFINING CLIMATE PREDICTIONS
library(dplyr)
library(sdmTMB)

# load temp model
model1 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-500kn.rds"))
max(model1$gradients)

model1 <- run_extra_optimization(model1)
max(model1$gradients)


# load DO model 
model2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016-depth2.rds")) 
model2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016.rds"))

max(model2$gradients)

model2 <- run_extra_optimization(model2)
max(model2$gradients)

# load prediction grid
nd_all <- readRDS(here::here("analysis/VOCC/data/nd_all_synoptic.rds")) %>% filter(year<2019)

pred_temp <- predict(model1, newdata = nd_all)

predtemp <- pred_temp %>% mutate(
  temp = est,
  temp_omega = omega_s,
  temp_epsilon = epsilon_st,
  temp_scaled = (est - model2$data$temp_mean[1])/model2$data$temp_sd[1], 
  temp_scaled2 = (temp_scaled)^2) %>% select(-est, -est_non_rf, -est_rf, -omega_s, -zeta_s, -epsilon_st)
predtemp <- predtemp %>% filter(year > 2007) 
predtemp$DOY_scaled <- 0

# make DO predictions
pred_do <- predict(model2, newdata = predtemp)

# rename and back transform climate variables
pred_do$log_do <- pred_do$est
pred_do$do_est <- exp(pred_do$log_do)

# may want to filter estimates for depths less than 15 m
# pred_do <- pred_do %>% filter(depth>15)

saveRDS(pred_do, file=here::here(paste0("analysis/VOCC/data/predicted-DO-", Sys.Date(),".rds")))
