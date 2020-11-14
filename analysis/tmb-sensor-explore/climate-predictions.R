### REFINING CLIMATE PREDICTIONS
library(dplyr)
library(sdmTMB)

# load temp model
# used in current models
# model1 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-500kn-2.rds")) 
# ready for next run
model1 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-800kn.rds")) 
max(model1$gradients)
# model1 <- run_extra_optimization(model1)
# max(model1$gradients)

# load DO model 
model2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016b-800kn.rds"))
max(model2$gradients)
# model2 <- run_extra_optimization(model2)
# max(model2$gradients)

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

### to check the rerun model didn't change predictions
# model2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016b-800kn-update.rds"))
# #check DO update against previous pred
# predtemp <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/predicted-DO-2020-06-20-more2016-roms.rds")
# predtemp <- predtemp %>% mutate(
#   log_do_old = log_do,
#   omega_old = omega_s,
#   epsilon_old = epsilon_st) %>% select(-est, -est_non_rf, -est_rf, -omega_s, -zeta_s, -epsilon_st)
# predtemp$DOY_scaled <- 0

# make DO predictions
pred_do <- predict(model2, newdata = predtemp)

ggplot(pred_do, aes(exp(log_do_old), exp(est))) + geom_point(alpha=0.2) + theme_pbs()
plot(pred_do$omega_old, pred_do$omega_s)
plot(pred_do$epsilon_old, pred_do$epsilon_st)

# rename and back transform climate variables
pred_do$log_do <- pred_do$est
pred_do$do_est <- exp(pred_do$log_do)

# may want to filter estimates for depths less than 15 m
# pred_do <- pred_do %>% filter(depth>15)

saveRDS(pred_do, file=here::here(paste0("analysis/VOCC/data/predicted-DO-", Sys.Date(), ".rds")))
