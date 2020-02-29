library(TMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gfranges)
library(future)
plan(multiprocess)
# plan("multisession")
setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

knots <- 500


# y_type <- "vel"
y_type <- "trend"

no_chopsticks <- F
both <- T
# # model_type <- "-vel-both"
# # model_type <- "-vel-both-fishing"
model_type <- "-trend-with-do"

# both <- F
# model_type <- "-vel-temp"
# model_type <- "-trend"
# model_type <- "-trend-grad"
# model_type <- "-dist-vel-temp"

w_genus <- T
is_null <- F


stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")

stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
# stats$group[stats$group == "SHARK"] <- "SHARKS & SKATES"
# stats$group[stats$group == "SKATE"] <- "SHARKS & SKATES"
stats$group[stats$group == "HAKE"] <- "COD"


#### LOAD MATURE VOCC DATA
# data_type <- "multi-spp-biotic-vocc-mature"
# d <- readRDS(paste0("data/", data_type, "-with-fished.rds"))

data_type <- "mature-95-all-temp"
 # data_type <- "mature-90-all-temp"
# data_type <- "mature-80-all-temp"
#  data_type <- "mature-50-all-temp"
# data_type <- "mature-90-all-do"
# data_type <- "mature-80-all-do"
data_type <- "mature-95-all-do"

#  null_number <- ""
null_number <- "-1"
 # null_number <- "-2"
  null_number <- "-3"

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

d$squashed_temp_dvocc <- collapse_outliers(d$temp_dvocc, c(0.005, 0.995))
# plot(squashed_do_vel ~ squashed_temp_vel, data = d, col = "#00000010")
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)
d$squashed_temp_dvocc_scaled <- scale(d$squashed_temp_dvocc, center = F)
d$mean_temp_scaled <- scale(d$mean_temp)
# hist(d$mean_temp_scaled)
# hist(d$temp_trend)
d$temp_trend_scaled <- scale(d$temp_trend, center = FALSE)
# hist(d$temp_trend_scaled)
d$temp_grad_scaled <- scale(d$temp_grad)
# hist(d$temp_grad_scaled)
# d$temp_grad_scaled <- scale(sqrt(d$temp_grad))
# hist(d$temp_grad_scaled)

#### PREP DO VARIABLES ####

d$squashed_DO_vel <- collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$squashed_DO_vel_scaled <- scale(d$squashed_DO_vel, center = FALSE)
d$DO_dvocc_scaled <- scale(d$DO_dvocc, center = F)
d$squashed_DO_dvocc <- collapse_outliers(d$DO_dvocc, c(0.005, 0.995))
d$squashed_DO_dvocc_scaled <- scale(d$squashed_DO_dvocc, center = FALSE)

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
d$squashed_biotic_dvocc <- collapse_outliers(d$biotic_dvocc, c(0.005, 0.995))

d$fake_vel <- d$fake_trend / d$biotic_grad
# hist(d$fake_vel)
d$squashed_fake_vel <- collapse_outliers(d$fake_vel, c(0.005, 0.995))
# hist(d$squashed_fake_vel)
### other possible response variables
# hist((d$sd_est))
# hist(log(d$sd_est))
# hist(log(d$biotic_CV))


############################
############################
#### TREND-BASED COVARIATES

temp_chopstick <- F
DO_chopstick <- F
fishing_chopstick <- F

if (model_type == "-trend") {
  formula <- ~ temp_trend_scaled +
    mean_temp_scaled + temp_trend_scaled:mean_temp_scaled +
    log_biomass_scaled #+ log_biomass_scaled2 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  x_type <- "trend"
}
  
if (model_type == "-trend-grad") {
  formula <- ~ temp_trend_scaled +
  mean_temp_scaled + temp_trend_scaled:mean_temp_scaled +
  temp_grad_scaled + 
  log_biomass_scaled 

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  x_type <- "trend"
}

### TREND DO INTERACTIONS

if (model_type == "-trend-with-do") {

formula <- ~ temp_trend_scaled +
  mean_temp_scaled + 
  temp_trend_scaled:mean_temp_scaled +
  # log_effort_scaled + fishing_trend_scaled +
  # temp_grad_scaled + 
  DO_trend_scaled +
  mean_DO_scaled +  
  DO_trend_scaled:mean_DO_scaled +
  log_biomass_scaled 

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "trend"
}

############################
# #### VELOCITY VARIABLES

if(model_type == "-vel-temp") {
  formula <- ~ squashed_temp_vel_scaled + 
    mean_temp_scaled +  
    squashed_temp_vel_scaled:mean_temp_scaled +
    log_biomass_scaled #+ log_biomass_scaled2
 
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  x_type <- "vel"
}

if(model_type == "-vel-both") {
# if (model_type == "-vel-no-fishing") {
  formula <- ~ squashed_temp_vel_scaled + 
    squashed_DO_vel_scaled +
    mean_temp_scaled +  
    squashed_temp_vel_scaled:mean_temp_scaled +
    mean_DO_scaled +  
    squashed_DO_vel_scaled:mean_DO_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled #+ log_biomass_scaled2
    #squashed_temp_vel_scaled:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "vel"
  
}

if(model_type == "-vel-both-fishing") {
  # if (model_type == "-vel-no-fishing") {
  formula <- ~ squashed_temp_vel_scaled + 
    squashed_DO_vel_scaled +
    mean_temp_scaled +  
    squashed_temp_vel_scaled:mean_temp_scaled +
    mean_DO_scaled +  
    squashed_DO_vel_scaled:mean_DO_scaled +
    log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled #+ log_biomass_scaled2
  #squashed_temp_vel_scaled:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "vel"
}

if (model_type == "-dist-vel-temp") {
  formula <- ~ squashed_temp_dvocc_scaled + 
    mean_temp_scaled +  
    squashed_temp_dvocc_scaled:mean_temp_scaled +
    log_effort_scaled + fishing_trend_scaled +
    fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled # + log_biomass_scaled2
  # squashed_temp_vel_scaled:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  x_type <- "vel"
}

if (model_type == "-dist-vel-both") {
  formula <- ~ squashed_temp_dvocc_scaled + 
    squashed_DO_dvocc_scaled +
    mean_temp_scaled +  
    squashed_temp_dvocc_scaled:mean_temp_scaled +
    mean_DO_scaled + 
    squashed_DO_dvocc_scaled:mean_DO_scaled +
    log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled # + log_biomass_scaled2
    # squashed_temp_vel_scaled:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "vel"
}


 if(no_chopsticks){
#   temp_chopstick <- F
  DO_chopstick <- F
#   fishing_chopstick <- F
# # would need to add dummy data then?
 }
 
if (is_null) {
  null_lab <- "-sim"
} else {
  null_lab <- ""
}

if (w_genus) {
  genus_lab <- "-genus"
} else {
  genus_lab <- ""
}


if(temp_chopstick){
  split_effect_column <- "mean_temp_scaled"
  
  if(x_type == "trend"){
  interaction_column <- "temp_trend_scaled:mean_temp_scaled"
  main_effect_column <- "temp_trend_scaled"
  } else {
    if(x_type == "vel") {
      interaction_column <- "squashed_temp_vel_scaled:mean_temp_scaled"
      main_effect_column <- "squashed_temp_vel_scaled"
    } else {
      interaction_column <- "squashed_temp_dvocc_scaled:mean_temp_scaled"
      main_effect_column <- "squashed_temp_dvocc_scaled"
    }
  }
  
  pred_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 2 # increase for final figures
  ) %>% mutate(type = "temp")
  
  X_pj <- as.matrix(select(pred_dat, -chopstick, -species, -genus, -type))
  
  if (y_type == "trend") {
    
    #### biotic tend 
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }
    
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      new_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      new_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
    
  } else { 
    
    #### biotic velocity (uses student_t)
    
    if (is_null) {
      y <- d$squashed_fake_vel
    } else {
      y <- d$squashed_biotic_vel
      
      if(model_type == "-dist-vel-temp") {
        y <- d$squashed_biotic_dvocc
        # y <- d$biotic_trend
      }
    }
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      new_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      new_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  }
}


if(DO_chopstick){
  split_effect_column <- "mean_DO_scaled"
  
  if(x_type == "trend"){
    interaction_column <- "DO_trend_scaled:mean_DO_scaled"
    main_effect_column <- "DO_trend_scaled"
  } else {
    if(x_type == "vel") {
      interaction_column <- "squashed_DO_vel_scaled:mean_DO_scaled"
      main_effect_column <- "squashed_DO_vel_scaled"
    } else {
      interaction_column <- "squashed_DO_dvocc_scaled:mean_DO_scaled"
      main_effect_column <- "squashed_DO_dvocc_scaled"
    }
  }
  
  
  pred_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 2 # increase for final figures
  ) %>% mutate(type = "DO")
  
  X_pj <- as.matrix(select(pred_dat, -chopstick, -species, -genus, -type))
  if (y_type == "trend") {
    
    #### biotic tend 
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }
    
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      DO_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      DO_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
    
  } else { 
    
    #### biotic velocity (uses student_t)
    
    if (is_null) {
      y <- d$squashed_fake_vel
    } else {
      y <- d$squashed_biotic_vel
      
      if(model_type == "-dist-vel-temp") {
        y <- d$squashed_biotic_dvocc
        # y <- d$biotic_trend
      }
    }
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      DO_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      DO_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  }
}

if(fishing_chopstick){
  split_effect_column <- "log_effort_scaled"
  interaction_column <- "fishing_trend_scaled:log_effort_scaled"
  main_effect_column <- "fishing_trend_scaled"
  
  pred_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 2 # increase for final figures
  ) %>% mutate(type = "fishing")
  
  X_pj <- as.matrix(select(pred_dat, -chopstick, -species, -genus, -type))

  if (y_type == "trend") {
    
    #### biotic tend 
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }
    
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      fishing_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      fishing_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  } else { 
    #### biotic velocity (uses student_t)
    if (is_null) {
      y <- d$squashed_fake_vel
    } else {
      y <- d$squashed_biotic_vel
      if(model_type == "-dist-vel-temp") {
        y <- d$squashed_biotic_dvocc
        # y <- d$biotic_trend
      }
    }
    if (w_genus) {
      model_type <- paste0(model_type, "-genus")
      fishing_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      fishing_model %<-%  vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = T,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  }
}

############################

if(DO_chopstick){

new_model$DO_pred_dat <- DO_model$pred_dat
new_model$DO_deltas <- DO_model$deltas

par_est <- as.list(DO_model$sdr, "Estimate", report = TRUE)
par_se <- as.list(DO_model$sdr, "Std. Error", report = TRUE)

new_model$DO_delta_diff <- cbind(par_est$diff_delta_k, par_se$diff_delta_k)
}


date <- format(Sys.time(), "-%m-%d")

saveRDS(new_model, file = paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, genus_lab, "-", knots, ".rds"))

paste0("data/", y_type, "-", data_type, date, model_type, null_lab,  null_number, genus_lab, "-", knots, ".rds")


##############################
#### LOAD MODEL JUST BUILT
model <- new_model

nrow(model$data)

model2 <- add_colours(model$coefs) %>%
  # filter(coeffZicient != "mean_DO_scaled" ) %>%
   # filter(coefficient != "temp_grad_scaled" ) %>%
#   filter(coefficient != "mean_temp_scaled" ) %>% 
  filter(coefficient != "log_biomass_scaled" )

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
# # library(ggsidekick) # for fourth_root_power if gfranges not loaded
ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(trans = fourth_root_power) + gfplot::theme_pbs() +
  facet_wrap(~species)
#
# # ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# # ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)
#
r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i

model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() +
  facet_wrap(~species)

# # ggsave("figs/vel-model-residuals.png", width = 12, height = 12, dpi = 300)
# # ggsave("figs/trend-model-residuals.png", width = 12, height = 12, dpi = 300)
#
norm_resids <- qres_student(model)
norm_resids <- norm_resids[is.finite(norm_resids)]
# # qqnorm(norm_resids)
# hist(norm_resids)

# norm_resids <- qres_student(model_genus)
# norm_resids <- norm_resids[is.finite(norm_resids)]
# # hist(norm_resids)
# 
# # qqnorm(model$data$residual)
# # qqline(model$data$residual)

paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, genus_lab, "-", knots, ".rds")