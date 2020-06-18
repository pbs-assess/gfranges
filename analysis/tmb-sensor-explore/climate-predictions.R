### new temp and do prediction grid for vectors
# model1 <- readRDS("~/github/dfo/gfranges/analysis/tmb-sensor-explore/models/model-temp-2008-on-used.rds")
#model2 <- readRDS("~/github/dfo/gfranges/analysis/tmb-sensor-explore/models/model-do-all-DOY-used.rds")
# update model to exclude WCVI 2016 outliers 
# model2 <- readRDS("~/github/dfo/gfranges/analysis/tmb-sensor-explore/models/model-do-without-wcvi2016.rds")

# fix RW additon error
model1 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-temp-all-years-500kn.rds"))
# update model to exclude WCVI 2016 outliers
model2 <- readRDS(here::here("analysis/tmb-sensor-explore/models/model-do-without-wcvi2016.rds"))

nd_all <- readRDS(here::here("analysis/VOCC/data/nd_all_synoptic.rds")) %>% filter(year<2019)

pred_temp <- predict(model1, newdata = nd_all)

# get scaling used in model
d <- all_sensor %>%
  # filter missing do data 
  dplyr::filter(!is.na(do_mlpl)) %>%
  # remove 2007 data because clearly faulty or scaled differently
  dplyr::filter(year>2007)
d$temp_scaled <- scale(d$temperature_c)
attributes(d$temp_scaled)

predtemp <- pred_temp %>% mutate(temp_scaled = (est - 6.49 )/1.296, temp_scaled2 = ((est - 6.49 )/1.296)^2, depth_scaled3 = depth_scaled^3) %>% select(-est, -est_non_rf, -est_rf, -omega_s, -zeta_s, -epsilon_st)
predtemp <- predtemp %>% filter(year > 2007) 
predtemp$DOY_scaled <- 0

pred_do <- predict(model2, newdata = predtemp)

pred_do$temp <- pred_do$temp_scaled*1.296 + 6.49

pred_do$log_do <- pred_do$est
pred_do$do_est <- exp(pred_do$log_do)

# may want to filter estimates for depths less than 15 m
# pred_do <- pred_do %>% filter(depth>15)
# saveRDS(pred_do, file="~/github/dfo/gfranges/analysis/VOCC/data/predicted-DO-new.rds")

saveRDS(pred_do, file=here::here("analysis/VOCC/data/predicted-DO-june-2020.rds"))

