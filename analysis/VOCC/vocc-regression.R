library(TMB)
library(dplyr)
library(ggplot2)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

y_type <- "vel"
# y_type <- "trend"

model_type <- "-vel"
model_type <- "-vel-no-fishing"
# model_type <- "-trend"
# # model_type <- "-trend-no-fish-trend"
# # model_type <- "-trend-no-fishing"
#  model_type <- "-trend-no-covs"
#  model_type <- "-trend-only"
 # model_type <- "-trend-grad"
 # model_type <- "-trend-with-do"
w_genus <- F
is_null <- T


stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats$genus <- tolower(stats$group)

#### LOAD MATURE VOCC DATA
# data_type <- "multi-spp-biotic-vocc-mature"
# d <- readRDS(paste0("data/", data_type, "-with-fished.rds"))

data_type <- "mature-95-all-temp"
 # data_type <- "mature-90-all-temp"
# data_type <- "mature-80-all-temp"
#  data_type <- "mature-50-all-temp"
data_type <- "mature-90-all-do"
# data_type <- "mature-80-all-do"
data_type <- "mature-95-all-do"

#  null_number <- ""
null_number <- "-1"
 null_number <- "-2"
#  null_number <- "-3"

d <- readRDS(paste0("data/", data_type, "-with-null", null_number, ".rds"))
d <- na.omit(d) %>% as_tibble()

d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  # filter(species != "Bocaccio") %>%
  # filter(species != "Sand Sole") %>%
  filter(species != "Longspine Thornyhead")

# #### LOAD IMMATURE VOCC DATA
# data_type <- "multi-spp-biotic-vocc-immature"
# d <- readRDS(paste0("data/", data_type, "-with-fished.rds"))
# d <- na.omit(d) %>% as_tibble()
#
# d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
#   # filter(species != "Curlfin Sole") %>%
#   # filter(species != "Bocaccio") %>%
#   filter(species != "Longspine Thornyhead")

select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>%
  as.data.frame()


#### PREP TEMP VARIABLES ####
d$squashed_temp_vel <- collapse_outliers(d$temp_vel, c(0.005, 0.995))
# plot(squashed_do_vel ~ squashed_temp_vel, data = d, col = "#00000010")
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)
d$mean_temp_scaled <- scale(d$mean_temp)
# hist(d$mean_temp_scaled)
# hist(d$temp_trend)
d$temp_trend_scaled <- scale(d$temp_trend, center = FALSE)
# hist(d$temp_trend_scaled)
d$temp_grad_scaled <- scale(d$temp_grad)
# hist(d$temp_grad_scaled)
# d$temp_grad_scaled <- scale(sqrt(d$temp_grad))
# hist(d$temp_grad_scaled)

# ggplot(d, aes(x, y, color = squashed_temp_vel)) + geom_point(size = 2, shape = 15) +
  # scale_color_gradient2(low = "deepskyblue4", high = "red", trans = fourth_root_power)
# # ggplot(d, aes(x,y, color=temp_trend)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x, y, color = temp_trend_scaled)) + geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(low = "deepskyblue4", high = "red")
# ggplot(d, aes(x, y, color = temp_grad)) + geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(high = "deepskyblue4", trans = sqrt)
# ggplot(d, aes(x, y, color = temp_grad_scaled)) + geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(high = "deepskyblue4")
# ggplot(d, aes(x, y, color = mean_temp_scaled)) + geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(low = "deepskyblue4", high = "red")

#### PREP DO VARIABLES ####

d$squashed_do_vel <- collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$squashed_do_vel_scaled <- scale(d$squashed_do_vel, center = FALSE)
# hist(d$mean_DO)
d$mean_DO_scaled <- scale(d$mean_DO)
# hist(d$mean_DO_scaled)
# hist(d$DO_trend)
d$DO_trend_scaled <- scale(d$DO_trend, center = FALSE)
# hist(d$DO_trend_scaled)
d$DO_grad_scaled <- scale(d$DO_grad)
# hist(d$DO_grad_scaled)
# d$DO_grad_scaled <- scale(sqrt(d$DO_grad))
# hist(d$DO_grad_scaled)
# ggplot(d, aes(x,y, color=squashed_do_vel)) + geom_point(size=2, shape=15) +
#   scale_color_gradient2(trans = fourth_root_power)
# # ggplot(d, aes(x,y, color=DO_trend)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_trend_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_grad)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_grad_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=mean_DO_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()



#### PREP FISHING VARIABLES ####

# hist(d$mean_effort)
d$sqrt_effort <- sqrt(d$mean_effort)
d$sqrt_effort_scaled <- scale(sqrt(d$mean_effort), center = F)

d$fishing_trend_scaled <- scale(d$fishing_trend, center = F)
# hist(d$fishing_trend_scaled)

# hist(sqrt(d$mean_effort))
d$log_effort <- log(d$mean_effort + 1)
# hist(log(d$mean_effort + 1))
d$log_effort_scaled <- scale(d$log_effort, center = F)

# ggplot(d, aes(x, y, color = log_effort_scaled)) + 
#   geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(low = "deepskyblue4", high = "red")
# ggplot(d, aes(x, y, color = sqrt_effort_scaled)) + 
#   geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(low = "deepskyblue4", high = "red") # , trans = fourth_root_power)
# ggplot(d, aes(x, y, color = fishing_trend_scaled)) + 
#   geom_point(size = 2, shape = 15) +
#   scale_color_gradient2(low = "deepskyblue4", high = "red") # , trans = fourth_root_power)


#### PREP FISH BIOMASS VARIABLES ####
# range(d$mean_biomass)
d$mean_biomass_scaled <- scale((d$mean_biomass))
d$log_biomass_scaled <- scale(log(d$mean_biomass))
d$log_biomass_scaled2 <- d$log_biomass_scaled^2

# hist(d$log_biomass_scaled)
# d$log_sd_est <- scale(log(d$sd_est))
# # hist(d$log_sd_est)
# d$log_sd_est2 <- d$log_sd_est^2
# d$abs_biotic <- sqrt((d$biotic_trend)^2)
# hist(log(d$abs_biotic))
# plot(log(abs_biotic)~log(sd_est), data=d, col = "#00000010")


#### MAKE FAKE BIOTIC VELOCITY
# hist(d$biotic_vel)
d$squashed_biotic_vel <- collapse_outliers(d$biotic_vel, c(0.005, 0.995))
# hist(d$squashed_biotic_vel)

d$fake_vel <- d$fake_trend / d$biotic_grad
# hist(d$fake_vel)
d$squashed_fake_vel <- collapse_outliers(d$fake_vel, c(0.005, 0.995))
# hist(d$squashed_fake_vel)

# ggplot(d, aes(x, y, color = temp_grad)) + geom_point(size = 1.5, shape = 15) +
#   scale_color_gradient2()
# 
# ggplot(d, aes(scale(temp_grad), scale(log(biotic_grad)))) +
#   geom_point(col = "#00000010") + geom_smooth(method = lm ) +
#   facet_wrap(~species, scales = "free")
# 
# d %>% filter(species == "Pacific Cod") %>%
#   ggplot(aes(x, y, color = squashed_biotic_vel)) +
#   geom_point(size = 1.5, shape = 15) + scale_color_gradient2()
# d %>% filter(species == "Pacific Cod") %>%
#   ggplot(aes(x, y, color = squashed_fake_vel)) +
#   geom_point(size = 1.5, shape = 15) + scale_color_gradient2()

# d %>% filter(species == "Pacific Cod") %>%
#   ggplot(aes(x, y, color = biotic_trend)) +
#   geom_point(size = 1.5, shape = 15) + scale_color_gradient2()
# d %>% filter(species == "Pacific Cod") %>%
#   ggplot(aes(x, y, color = fake_trend)) +
#   geom_point(size = 1.5, shape = 15) + scale_color_gradient2()
#
# d %>% filter(species == "Pacific Cod") %>%
#   ggplot(aes(x, y, color = biotic_grad)) +
#   geom_point(size = 1.5, shape = 15) + scale_color_gradient2()
#
### other possible response variables
# hist((d$sd_est))
# hist(log(d$sd_est))
# hist(log(d$biotic_CV))


############################
############################
#### TREND-BASED COVARIATES

# model_type <- "-trend"
# # model_type <- "-all-main"
# # model_type <- ""
# 
# model_type <- "-vel"


# if (model_type == "-trend") {
# if (model_type == "-trend-no-fish-trend") {
# if (model_type == "-trend-no-fishing") {
  
if (model_type == "-trend-grad") {
  formula <- ~ temp_trend_scaled +
  mean_temp_scaled + temp_trend_scaled:mean_temp_scaled +
  # # log_effort_scaled + fishing_trend_scaled +
  # #fishing_trend_scaled:log_effort_scaled +
  # # fishing_trend_scaled:log_biomass_scaled +
  # #temp_trend_scaled:log_biomass_scaled +
  temp_grad_scaled + #log_biomass_scaled:temp_grad_scaled +
# DO_trend_scaled +
# mean_DO_scaled +  DO_trend_scaled:mean_DO_scaled +
# grad_diff + grad_diff:DO_trend_scaled +
# mean_DO_scaled:mean_temp_scaled
  # log_sd_est + log_sd_est2 +
  log_biomass_scaled #+ log_biomass_scaled2 

x <- model.matrix(formula, data = d)

d_temp <- interaction_df(d, formula,
  x_variable = "temp_trend_scaled",
  split_variable = "mean_temp_scaled",
  N = 2 # increase for final figures
)
# d_bio <- interaction_df(d,
#   formula = formula,
#   x_variable = "temp_trend_scaled",
#   split_variable = "log_biomass_scaled",
#   N = 5
# )
# d_grad <- interaction_df(d, formula,
#   x_variable = "temp_trend_scaled",
#   split_variable = "temp_grad_scaled",
#   # use_quantiles = FALSE,
#   N = 5
# )
# d_fish <- interaction_df(d,
#   formula = formula,
#   x_variable = "fishing_trend_scaled",
#   split_variable = "log_biomass_scaled",
#   N = 5
# )

}

### TREND DO INTERACTIONS

if (model_type == "-trend-with-do") {
formula <- ~ temp_trend_scaled +
  mean_temp_scaled + temp_trend_scaled:mean_temp_scaled +
  # # log_effort_scaled + fishing_trend_scaled +
  # #fishing_trend_scaled:log_effort_scaled +
  # # fishing_trend_scaled:log_biomass_scaled +
  # #temp_trend_scaled:log_biomass_scaled +
  temp_grad_scaled + #log_biomass_scaled:temp_grad_scaled +
  DO_trend_scaled +
  mean_DO_scaled +  DO_trend_scaled:mean_DO_scaled +
  # grad_diff + grad_diff:DO_trend_scaled +
  # mean_DO_scaled:mean_temp_scaled
  # log_sd_est + log_sd_est2 +
  log_biomass_scaled #+ log_biomass_scaled2 

x <- model.matrix(formula, data = d)

d_temp <- interaction_df(d, formula,
  x_variable = "temp_trend_scaled",
  split_variable = "mean_temp_scaled",
  N = 2 # increase for final figures
)

d_do <- interaction_df(d,
  formula = formula,
  x_variable = "DO_trend_scaled",
  split_variable = "mean_DO_scaled",
  N = 2
)
## d_means <- interaction_df(d, formula,
#   x_variable = "mean_temp_scaled",
#   split_variable = "mean_DO_scaled",
#   # use_quantiles = FALSE,
#   N = 5
# )
# d_means2 <- interaction_df(d, formula,
#   x_variable = "mean_DO_scaled",
#   split_variable = "mean_temp_scaled",
#   # use_quantiles = FALSE,
#   N = 5
# )
}

############################
# #### VELOCITY VARIABLES

# if(model_type == "-vel") {
if (model_type == "-vel-no-fishing") {
  formula <- ~ squashed_temp_vel_scaled +
    mean_temp_scaled +  squashed_temp_vel_scaled:mean_temp_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled #+ log_biomass_scaled2
    #squashed_temp_vel_scaled:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  d_temp <- interaction_df(d, formula,
    x_variable = "squashed_temp_vel_scaled",
    split_variable = "mean_temp_scaled",
    N = 5
  )
  # d_bio <- interaction_df(d,
  #   formula = formula,
  #   x_variable = "squashed_temp_vel_scaled",
  #   split_variable = "log_biomass_scaled",
  #   N = 5
  # )
  # d_fish <- interaction_df(d,
  #   formula = formula,
  #   x_variable = "fishing_trend_scaled",
  #   split_variable = "log_effort_scaled",
  #   N = 5
  # )
  
}


### VEL DO INTERACTIONS
# formula <- ~ mean_DO_scaled + mean_temp_scaled +
#   mean_biomass_scaled +
#   mean_temp_scaled:mean_DO_scaled +
#   squashed_do_vel_scaled + squashed_do_vel_scaled:mean_DO_scaled +
#   squashed_temp_vel_scaled + squashed_temp_vel_scaled:mean_temp_scaled
#
# x <- model.matrix(formula, data = d)
#
# d_temp <- interaction_df(d, formula,
#   x_variable = "squashed_temp_vel_scaled",
#   split_variable = "mean_temp_scaled",
#   N = 5
# )
# d_do <- interaction_df(d,
#   formula = formula,
#   x_variable = "squashed_do_vel_scaled",
#   split_variable = "mean_DO_scaled",
#   N = 5
# )

########################
#### INTERACTION DATAFRAMES
########################
# don't include intercepts?
# d_temp$`(Intercept)` <- 0 
# d_bio$`(Intercept)` <- 0 

X_pj <- as.matrix(bind_rows(
  # select(d_do, -chopstick, -species, -genus),
  select(d_temp, -chopstick, -species, -genus)#,
  # select(d_bio, -chopstick, -species, -genus) # ,
  # select(d_fish, -chopstick, -species, -genus) 
  # select(d_grad, -chopstick, -species, -genus)#,
  # select(d_means, -chopstick, -species, -genus)
))

pred_dat <- bind_rows(
  # mutate(d_do, type = "do"),
  mutate(d_temp, type = "temp")#,
  # mutate(d_bio, type = "biomass")
   # mutate(d_fish, type = "fish")
)
# mutate(d_bio, type = "biomass"),
# mutate(d_grad, type = "temp_grad"))
# mutate(d_grad, type = "temp_grad"),
# mutate(d_means, type = "means"))


############################
########################
#### CHOOSE MODEL TYPE
########################

# y_type <- "trend"
#  y_type <- "vel"
# 
# w_genus <- F
# is_null <- F

  if (is_null) {
    null_lab <- "-sim"
  } else {
    null_lab <- ""
  }
  
if (y_type == "trend") {
  
  #### biotic tend 
  
  if (is_null) {
    y <- d$fake_trend
  } else {
    y <- d$biotic_trend
  }
  
  if (w_genus) {
    model_type <- paste0(model_type, "-genus")
    new_model <- vocc_regression(d, y,
      X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
      knots = 200, group_by_genus = T, student_t = F
    )
  } else {
    new_model <- vocc_regression(d, y,
      X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
      knots = 200, group_by_genus = FALSE, student_t = F
    )
  }

} else { 
  
  #### biotic velocity (uses student_t)
  
  if (is_null) {
    y <- d$squashed_fake_vel
  } else {
    y <- d$squashed_biotic_vel
  }
  if (w_genus) {
    model_type <- paste0(model_type, "-genus")
    new_model <- vocc_regression(d, y,
      X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
      knots = 200, group_by_genus = T, student_t = T
    )
  } else {
    new_model <- vocc_regression(d, y,
      X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
      knots = 200, group_by_genus = FALSE, student_t = T
    )
  }

}


date <- format(Sys.time(), "-%m-%d")

saveRDS(new_model, file = paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, ".rds"))

paste0("data/", y_type, "-", data_type, date, model_type, null_lab,  null_number, ".rds")


##############################
#### LOAD TREND MODELS ####

#### ONE JUST BUILT
model <- new_model

#### SAVED MODELS


# #### VELOCITY
# model <- readRDS("data/trend-mature-90-all-temp-01-26-vel-2.rds")
# # model <- readRDS("data/trend-mature-80-all-temp-01-27-vel-2.rds")
# # model <- readRDS("data/trend-mature-50-all-temp-01-27-vel-2.rds")

# model <- readRDS("data/trend-mature-90-all-temp-01-27-vel-sim-2.rds") # not great
# # model <- readRDS("data/trend-mature-80-all-temp-01-27-vel-sim-2.rds") # better
# # model <- readRDS("data/trend-mature-50-all-temp-01-27-vel-sim-2.rds") # good

# # model <- readRDS("data/vel-mature-95-all-temp-01-26-vel-fishing-2.rds")
# # model <- readRDS("data/vel-mature-95-all-temp-01-26-vel-2.rds")
# model <- readRDS("data/vel-mature-90-all-temp-01-26-vel-2.rds") # some small +ve
# # model <- readRDS("data/vel-mature-80-all-temp-01-26-vel-2.rds")
# # model <- readRDS("data/vel-mature-90-all-temp-01-27-vel-sim-2.rds") # good
# # model <- readRDS("data/vel-mature-50-all-temp-01-27-vel-sim-2.rds") # better

nrow(model$data)

model2 <- add_colours(model$coefs) # %>% filter(species != "Shortbelly Rockfish")

# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# mature <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")
# model2 <- add_colours(mmature$coefs) # must be saved as model2 for use in function below
# model2 <- add_colours(model$coefs, last_used = TRUE)

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by) 
  #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model2$coefficient))), decreasing=F))
)


# manipulate::manipulate({
#   plot_coefs(model2, order_by_trait = T, fixed_scales = F, order_by = order_by)
# },
# order_by = manipulate::picker(as.list(sort(names(model2[, 6:15]))))
# )

# model2a <- model2 %>%
#   filter(coefficient != "mean_temp_scaled") %>%
#   filter(coefficient != "mean_DO_scaled") %>%
#   filter(coefficient != "mean_biomass_scaled") %>%
#   filter(coefficient != "mean_temp_scaled:mean_DO_scaled") %>%
#   filter(coefficient != "mean_DO_scaled:mean_temp_scaled")
# manipulate::manipulate({plot_coefs(model2a, order_by = order_by)},
#   order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))

# model2b <- model2 %>% filter(coefficient %in%
#    c("mean_temp_scaled", "mean_DO_scaled", "mean_biomass_scaled",
#    "mean_temp_scaled:mean_DO_scaled", "mean_DO_scaled:mean_temp_scaled"))
# manipulate::manipulate({plot_coefs(model2b, order_by = order_by)},
#   order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))
### SAVE PLOT WITH SELECTED PARAMS
# model3 <- plot_coefs(model2, order_by = "squashed_temp_vel_scaled)")
# model_plot <- model3 +
#   ggtitle(paste("Mature biomass trend"))
#   # ggtitle(paste("Immature biotic trend"))
# model_plot
# ggsave("figs/worm-plot-temp-trend-sort.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/worm-plot-imm-temp-trend-sort.png", width = 10, height = 10, dpi = 300)


##############################
#### CHECK SAMPLE SIZE AND DISTRIBUTION OF MODEL DATA
##############################
# nrow(model$data)
# mean(model$data$mean_biomass)
# range(model$data$mean_biomass)
# hist(log(model$data$mean_biomass))
# hist((model$data$biotic_trend))
# hist((model$data$temp_trend))


##############################
#### MODEL COEFFICIENTS ####
##############################

coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)

##############################
#### CHECK MODEL RESIDUALS ####
##############################
# # 
# # # library(ggsidekick) # for fourth_root_power if gfranges not loaded
# ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(trans = fourth_root_power) + gfplot::theme_pbs() +
#   facet_wrap(~species)
# # 
# # # ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# # # ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)
# # 
# r <- model$obj$report()
# model$data$residual <- model$y_i - r$eta_i
# 
# model$data %>%
#   mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
#   mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
#   mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
#   mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
#   ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2() + #gfplot::theme_pbs() +
#   facet_wrap(~species)
# 
# # # ggsave("figs/vel-model-residuals.png", width = 12, height = 12, dpi = 300)
# # # ggsave("figs/trend-model-residuals.png", width = 12, height = 12, dpi = 300)
# # 
# # norm_resids <- qres_student(model)
# # norm_resids <- norm_resids[is.finite(norm_resids)]
# # # qqnorm(norm_resids)
# hist(norm_resids)
# 
# # norm_resids <- qres_student(model_genus)
# # norm_resids <- norm_resids[is.finite(norm_resids)]
# # hist(norm_resids)
# 
# # qqnorm(model$data$residual)
# # qqline(model$data$residual)

paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, ".rds")