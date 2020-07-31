## REMEMBER TO RESTART R # .rs.restartR()
library(TMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gfranges)
# library(future)
# plan(multiprocess)
# plan("multisession")
setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")


### main modeling choices ###


# age <- "mature"
# age <- "immature"
age <- "both"

no_chopsticks <- F
w_genus <- F
w_family <- F
is_null <- F

null_number <- "-1"

# ## for trends ###
# knots <- 500
# y_type <- "trend"
# #
# # # model_type <- "-trend" # just temp
# # # model_type <- "-trend-do-only" # just DO
# # # model_type <- "-trend-w-age" # an experiment that lacks true chops for imm
# # model_type <- "-trend-w-age2"
# # # model_type <- "-trend-grad"
# # # model_type <- "-trend-w-grad"
# model_type <- "-trend-with-do"
# # model_type <- "-trend-w-fishing"


### for velocities ###
knots <- 350
y_type <- "vel"

# model_type <- "-vel-temp"
# model_type <- "-vel-do"
model_type <- "-vel-both"

# model_type <- "-dist-vel-temp"
# model_type <- "-dist-vel-both"
# model_type <- "-dist-vel-combined" # doesn't converg

### LOAD VOCC DATA
if (age != "both") {
  # data_type <- paste0(age,"-95-all-temp")
  # data_type <- paste0(age,"-90-all-temp")
  # data_type <- paste0(age,"-80-all-temp")
  # data_type <- paste0(age,"-50-all-temp")
  # data_type <- paste0(age,"-90-all-do")
  # data_type <- paste0(age,"-80-all-do")
  # data_type <- paste0(age, "-95-all-do")
}

if (age == "both") {
  # data_type <- paste0("all-95-all-do")
  # data_type <- paste0("all-95-newclim2")
  # data_type <- paste0("all-95-newclim-more2016")
  # data_type <- paste0("all-95-all-newclim")
  data_type <- paste0("all-95-optimized2")
  
}

d <- readRDS(paste0("data/", data_type, "-with-null", null_number, ".rds"))

d$family <- gsub("\\(.*", "", d$parent_taxonomic_unit)
d$true_genus <- d$genus

if (w_family) {
  d$genus <- d$family
}

d <- as_tibble(d) %>%
  # filter(species != "Bocaccio") %>%
  # filter(species != "Sand Sole") %>%
  filter(species != "Shortbelly Rockfish") %>%
  filter(species_age != "immature Shortraker Rockfish") %>%
  filter(species != "Longspine Thornyhead")

# if combining adult and imm in same model
if (age == "both") d <- mutate(d, species_only = species, species = species_age, age_class = age, age = if_else(age_class == "mature", 0, 1))


# plot(d$x, d$y)


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
hist(d$DO_trend_scaled)
# hist(d$squashed_DO_trend_scaled)

# d$DO_trend_scaled <- d$squashed_DO_trend_scaled

#### MAKE FAKE BIOTIC VELOCITY
hist(d$biotic_vel, breaks = 100)
hist(d$biotic_trend, breaks = 50)

if (age == "mature") {
  d$squashed_biotic_vel <- collapse_outliers(d$biotic_vel, c(0.005, 0.995))
  d$biotic_trend <- collapse_outliers(d$biotic_trend, c(0.00001, 0.99999))
  hist(d$biotic_trend, breaks = 50)
} else {
  d$squashed_biotic_vel <- collapse_outliers(d$biotic_vel, c(0.01, 0.98))
  d$squashed_biotic_trend <- collapse_outliers(d$biotic_trend, c(0.0001, 0.9999))
  d$biotic_trend <- collapse_outliers(d$biotic_trend, c(0.00001, 0.99999))
  hist(d$biotic_trend, breaks = 50)
  hist(d$squashed_biotic_trend, breaks = 50)
}

hist(d$squashed_biotic_vel)
hist(d$squashed_temp_vel_scaled, breaks = 100)

d$squashed_biotic_dvocc <- collapse_outliers(d$biotic_dvocc, c(0.005, 0.995))
hist(d$squashed_biotic_dvocc , breaks = 100)

hist(d$squashed_temp_dvocc, breaks = 100)
hist(d$DO_dvocc , breaks = 100)
hist(d$squashed_DO_dvocc , breaks = 100)
hist(d$temp_dvocc , breaks = 100)
hist(d$squashed_temp_dvocc , breaks = 100)

d$fake_vel <- d$fake_trend / d$biotic_grad
# hist(d$fake_vel)
d$squashed_fake_vel <- collapse_outliers(d$fake_vel, c(0.005, 0.98))
hist(d$squashed_fake_vel)


hist(abs(d$dvocc_both), breaks = 100)
d$dvocc_both_scaled <- scale(d$dvocc_both, center = FALSE)
hist(abs(d$dvocc_both_scaled), breaks = 100)

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

if (model_type == "-trend-do-only") {
  formula <- ~ DO_trend_scaled +
    mean_DO_scaled +
    DO_trend_scaled:mean_DO_scaled +
    log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  DO_chopstick <- T
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

if (model_type == "-trend-w-grad") {
  formula <- ~ temp_trend_scaled +
    mean_temp_scaled +
    temp_trend_scaled:mean_temp_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    temp_grad_scaled +
    temp_grad_scaled:mean_temp_scaled +
    temp_grad_scaled:temp_trend_scaled +
    temp_grad_scaled:mean_temp_scaled:temp_trend_scaled +
    # DO_trend_scaled +
    # mean_DO_scaled +
    # DO_trend_scaled:mean_DO_scaled +
    log_biomass_scaled

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  DO_chopstick <- F
  x_type <- "trend"
}

if (model_type == "-trend-w-age") {
  formula <- ~ age +
    temp_trend_scaled +
    mean_temp_scaled +
    temp_trend_scaled:mean_temp_scaled +
    # temp_grad_scaled +
    DO_trend_scaled +
    mean_DO_scaled +
    DO_trend_scaled:mean_DO_scaled +
    age:temp_trend_scaled +
    age:mean_temp_scaled +
    age:temp_trend_scaled:mean_temp_scaled +
    age:DO_trend_scaled +
    age:mean_DO_scaled +
    age:DO_trend_scaled:mean_DO_scaled +
    log_biomass_scaled #+ age:log_biomass_scaled 

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "trend"
}

if (model_type == "-trend-w-age2") {
  formula <- ~ age +
    temp_trend_scaled +
    mean_temp_scaled +
    temp_trend_scaled:mean_temp_scaled +
    # temp_grad_scaled +
    DO_trend_scaled +
    mean_DO_scaled +
    DO_trend_scaled:mean_DO_scaled +
    age:temp_trend_scaled +
    age:mean_temp_scaled +
    # age:temp_trend_scaled:mean_temp_scaled +
    age:DO_trend_scaled +
    age:mean_DO_scaled +
    # age:DO_trend_scaled:mean_DO_scaled +
    log_biomass_scaled #+ age:log_biomass_scaled 
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "trend"
}

if (model_type == "-trend-w-fishing") {
  formula <- ~ temp_trend_scaled +
    mean_temp_scaled +
    temp_trend_scaled:mean_temp_scaled +
    log_effort_scaled + fishing_trend_scaled +
    log_effort_scaled:fishing_trend_scaled +
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

if (model_type == "-vel-temp") {
  formula <- ~ squashed_temp_vel_scaled +
    mean_temp_scaled +
    squashed_temp_vel_scaled:mean_temp_scaled +
    log_biomass_scaled #+ log_biomass_scaled2

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  x_type <- "vel"
}

if (model_type == "-vel-do") {
  # if (model_type == "-vel-no-fishing") {
  formula <- ~ squashed_DO_vel_scaled +
    mean_DO_scaled +
    squashed_DO_vel_scaled:mean_DO_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled

  x <- model.matrix(formula, data = d)

  temp_chopstick <- F
  DO_chopstick <- T
  x_type <- "vel"
}

if (model_type == "-vel-both") {
  # if (model_type == "-vel-no-fishing") {
  formula <- ~ squashed_temp_vel_scaled +
    squashed_DO_vel_scaled +
    mean_temp_scaled +
    squashed_temp_vel_scaled:mean_temp_scaled +
    mean_DO_scaled +
    squashed_DO_vel_scaled:mean_DO_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "vel"
}

if (model_type == "-dist-vel-temp") {
  formula <- ~ squashed_temp_dvocc_scaled +
    mean_temp_scaled +
    squashed_temp_dvocc_scaled:mean_temp_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled # + log_biomass_scaled2
  # squashed_temp_vel_scaled:log_biomass_scaled

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  x_type <- "dvocc"
}

if (model_type == "-dist-vel-both") {
  formula <- ~ squashed_temp_dvocc_scaled +
    squashed_DO_dvocc_scaled +
    mean_temp_scaled +
    squashed_temp_dvocc_scaled:mean_temp_scaled +
    mean_DO_scaled +
    squashed_DO_dvocc_scaled:mean_DO_scaled +
    # log_effort_scaled + fishing_trend_scaled +
    # fishing_trend_scaled:log_effort_scaled +
    log_biomass_scaled

  x <- model.matrix(formula, data = d)

  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "dvocc"
}

if (model_type == "-dist-vel-combined") {
  formula <- ~ dvocc_both_scaled +
    mean_temp_scaled +
    dvocc_both_scaled:mean_temp_scaled +
    mean_DO_scaled +
    dvocc_both_scaled:mean_DO_scaled +
    log_biomass_scaled
  
  x <- model.matrix(formula, data = d)
  
  temp_chopstick <- T
  DO_chopstick <- T
  x_type <- "dvocc"
}

if (no_chopsticks) {
  temp_chopstick <- T
  DO_chopstick <- F
  fishing_chopstick <- F
  # # would need to add dummy data to eliminate temp?
}

if (is_null) {
  null_lab <- "-sim"
} else {
  null_lab <- ""
}

if (w_family) {
  model_type <- paste0(model_type, "-family")
}

if (w_genus) {
  model_type <- paste0(model_type, "-genus")
}

if (DO_chopstick) {
  split_effect_column <- "mean_DO_scaled"

  if (x_type == "trend") {
    interaction_column <- "DO_trend_scaled:mean_DO_scaled"
    main_effect_column <- "DO_trend_scaled"
  } else {
    if (x_type == "vel") {
      interaction_column <- "squashed_DO_vel_scaled:mean_DO_scaled"
      main_effect_column <- "squashed_DO_vel_scaled"
    } else {
      if (model_type == "-dist-vel-combined") {
        interaction_column <- "dvocc_both_scaled:mean_DO_scaled"
        main_effect_column <- "dvocc_both_scaled"
      } else {
      interaction_column <- "squashed_DO_dvocc_scaled:mean_DO_scaled"
      main_effect_column <- "squashed_DO_dvocc_scaled"
      }
    }
  }

  DO_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 3 # increase for final figures
  ) %>% mutate(type = "DO")

  DO_pj <- as.matrix(select(DO_dat, -chopstick, -species, -genus, -type))
  
  if (y_type == "trend") {

    #### biotic tend
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }

    hist(y)

    if (w_genus | w_family) {
      DO_model <- vocc_regression(d, y,
        X_ij = x, X_pj = DO_pj, pred_dat = DO_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      DO_model <- vocc_regression(d, y,
        X_ij = x, X_pj = DO_pj, pred_dat = DO_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  } else {
    if (y_type == "vel") {
      #### biotic velocity (uses student_t)
      if (is_null) {
        y <- d$squashed_fake_vel
      } else {
        y <- d$squashed_biotic_vel
        if (model_type == "-dist-vel-temp") {
          y <- d$squashed_biotic_dvocc
          # y <- d$biotic_trend
        }
      }

      hist(y)


      if (w_genus | w_family) {
        DO_model <- vocc_regression(d, y,
          X_ij = x, X_pj = DO_pj, pred_dat = DO_dat,
          knots = knots, group_by_genus = T, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      } else {
        DO_model <- vocc_regression(d, y,
          X_ij = x, X_pj = DO_pj, pred_dat = DO_dat,
          knots = knots, group_by_genus = FALSE, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      }
    }
  }

  DO_est <- as.list(DO_model$sdr, "Estimate", report = TRUE)
  DO_se <- as.list(DO_model$sdr, "Std. Error", report = TRUE)
  DO_model$pred_dat$est_p <- DO_est$eta_p
  DO_model$pred_dat$se_p <- DO_se$eta_p

  DO_est <- as.list(DO_model$sdr, "Estimate", report = TRUE)
  DO_se <- as.list(DO_model$sdr, "Std. Error", report = TRUE)
  DO_model$delta_diff <-
    cbind(DO_est$diff_delta_k, DO_se$diff_delta_k)
  DO_model$delta_diff <- as.data.frame(DO_model$delta_diff)
  DO_model$delta_diff$type <- "DO"
  DO_model$delta_diff$species <- unique(DO_model$deltas$species)

  names(DO_model$delta_diff) <- c("est", "se", "type", "species")

  date <- format(Sys.time(), "-%m-%d")
  saveRDS(DO_model, file = paste0(
    "data/", y_type, "-", data_type, date,
    model_type, null_lab, null_number, "-", knots, "-DO.rds"
  ))
}

if (fishing_chopstick) {
  split_effect_column <- "log_effort_scaled"
  interaction_column <- "fishing_trend_scaled:log_effort_scaled"
  main_effect_column <- "fishing_trend_scaled"

  F_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 3 # increase for final figures
  ) %>% mutate(type = "fishing")

  F_pj <- as.matrix(select(F_dat, -chopstick, -species, -genus, -type))

  if (y_type == "trend") {

    #### biotic tend
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }

    hist(y)

    if (w_genus | w_family) {
      fishing_model %<-% vocc_regression(d, y,
        X_ij = x, X_pj = F_pj, pred_dat = F_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      fishing_model %<-% vocc_regression(d, y,
        X_ij = x, X_pj = F_pj, pred_dat = F_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  } else {
    if (y_type == "trend") {
      #### biotic velocity (uses student_t)
      if (is_null) {
        y <- d$squashed_fake_vel
      } else {
        y <- d$squashed_biotic_vel
        if (model_type == "-dist-vel-temp") {
          y <- d$squashed_biotic_dvocc
          # y <- d$biotic_trend
        }
      }

      hist(y)

      if (w_genus | w_family) {
        fishing_model %<-% vocc_regression(d, y,
          X_ij = x, X_pj = F_pj, pred_dat = F_dat,
          knots = knots, group_by_genus = T, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      } else {
        fishing_model %<-% vocc_regression(d, y,
          X_ij = x, X_pj = F_pj, pred_dat = F_dat,
          knots = knots, group_by_genus = FALSE, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      }
    }
  }
}

if (temp_chopstick) {
  split_effect_column <- "mean_temp_scaled"

  if (x_type == "trend") {
    interaction_column <- "temp_trend_scaled:mean_temp_scaled"
    main_effect_column <- "temp_trend_scaled"
  } else {
    if (x_type == "vel") {
      interaction_column <- "squashed_temp_vel_scaled:mean_temp_scaled"
      main_effect_column <- "squashed_temp_vel_scaled"
    } else {
      if (model_type == "-dist-vel-combined") {
        interaction_column <- "dvocc_both_scaled:mean_temp_scaled"
        main_effect_column <- "dvocc_both_scaled"
      } else {
      interaction_column <- "squashed_temp_dvocc_scaled:mean_temp_scaled"
      main_effect_column <- "squashed_temp_dvocc_scaled"
      }
    }
  }

  pred_dat <- interaction_df(d, formula,
    x_variable = main_effect_column,
    split_variable = split_effect_column,
    N = 3 # increase for final figures
  ) %>% mutate(type = "temp")

  X_pj <- as.matrix(select(pred_dat, -chopstick, -species, -genus, -type))

  if (y_type == "trend") {

    #### biotic tend
    if (is_null) {
      y <- d$fake_trend
    } else {
      y <- d$biotic_trend
    }

    if (w_genus | w_family) {
      temp_model <- vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = T, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    } else {
      temp_model <- vocc_regression(d, y,
        X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
        knots = knots, group_by_genus = FALSE, student_t = F,
        interaction_column = interaction_column,
        main_effect_column = main_effect_column,
        split_effect_column = split_effect_column
      )
    }
  } else {
    if (y_type == "vel") {
      #### biotic velocity (uses student_t)
      if (is_null) {
        y <- d$squashed_fake_vel
      } else {
        y <- d$squashed_biotic_vel

        if (model_type == "-dist-vel-temp") {
          y <- d$squashed_biotic_dvocc
          # y <- d$biotic_trend
        }
      }

      hist(y)

      if (w_genus | w_family) {
        temp_model <- vocc_regression(d, y,
          X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
          knots = knots, group_by_genus = T, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      } else {
        temp_model <- vocc_regression(d, y,
          X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
          knots = knots, group_by_genus = FALSE, student_t = T,
          interaction_column = interaction_column,
          main_effect_column = main_effect_column,
          split_effect_column = split_effect_column
        )
      }
    }
  }


  temp_est <- as.list(temp_model$sdr, "Estimate", report = TRUE)
  temp_se <- as.list(temp_model$sdr, "Std. Error", report = TRUE)
  temp_model$pred_dat$est_p <- temp_est$eta_p
  temp_model$pred_dat$se_p <- temp_se$eta_p

  temp_est <- as.list(temp_model$sdr, "Estimate", report = TRUE)
  temp_se <- as.list(temp_model$sdr, "Std. Error", report = TRUE)
  temp_model$delta_diff <-
    cbind(temp_est$diff_delta_k, temp_se$diff_delta_k)
  temp_model$delta_diff <- as.data.frame(temp_model$delta_diff)
  temp_model$delta_diff$type <- "temp"
  temp_model$delta_diff$species <- unique(temp_model$deltas$species)

  names(temp_model$delta_diff) <- c("est", "se", "type", "species")

  date <- format(Sys.time(), "-%m-%d")
  saveRDS(temp_model, file = paste0(
    "data/", y_type, "-", data_type, date, model_type,
    null_lab, null_number, "-", knots, "-temp.rds"
  ))
}


############################
# Needs to wait until after models finish
# can't figure out how to do that automatically
# f <- futureOf(new_model)
# count <- 1
# while (!resolved(f)) {
#   cat(count, "\n")
#   Sys.sleep(0.2)
#   count <- count + 1
# }
############################
# Check if finished
print(head(temp_model$deltas))
print(head(DO_model$deltas))
############################



if (DO_chopstick & temp_chopstick) {
  new_model <- temp_model
  new_model$pred_dat <- rbind(temp_model$pred_dat, DO_model$pred_dat)
  new_model$deltas <- rbind(temp_model$deltas, DO_model$deltas)
  new_model$delta_diff <-
    rbind(temp_model$delta_diff, DO_model$delta_diff)
} else {
  new_model <- temp_model
  temp_est <- as.list(new_model$sdr, "Estimate", report = TRUE)
  temp_se <- as.list(new_model$sdr, "Std. Error", report = TRUE)
  new_model$delta_diff <-
    cbind(temp_est$diff_delta_k, temp_se$diff_delta_k)
  new_model$delta_diff <- as.data.frame(new_model$delta_diff)
  new_model$delta_diff$type <- "temp"
  new_model$delta_diff$species <- unique(new_model$deltas$species)
  names(new_model$delta_diff) <- c("est", "se", "type", "species")
}

date <- format(Sys.time(), "-%m-%d")
saveRDS(new_model, file = paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, "-", knots, ".rds"))

new_r <- new_model$obj$report()
new_model$data$residual <- new_model$y_i - new_r$eta_i
new_model$data$eta <- new_r$eta_i

saveRDS(new_model, file = paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, "-", knots, ".rds"))

paste0("data/", y_type, "-", data_type, date, model_type, null_lab, null_number, "-", knots, ".rds")


##############################
#### LOAD MODEL JUST BUILT
model <- DO_model
model <- temp_model
model <- new_model

nrow(model$data)


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
ggplot(model$data, aes(x, y, fill = omega_s)) +
  geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(trans = fourth_root_power) +
  gfplot::theme_pbs() +
  facet_wrap(~species)

# # ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# # ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)

r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i
model$data$eta <- r$eta_i

model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() +
  facet_wrap(~species)

ggsave(here::here("ms", "figs", paste0("spatial-residuals", null_lab, model_type, "-", knots, date, ".pdf")), width = 18, height = 12)






# pdf(paste0("residual-plots-", y_type, null_lab, model_type, date, ".pdf"), width = 18, height = 12)
# 
# if (y_type == "trend") {
#   ggplot(model$data, aes(eta, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
# 
#   ggplot(model$data, aes(biotic_trend)) + geom_histogram() +
#     facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(temp_trend, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(DO_trend, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_temp, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_DO, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_temp^2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_DO^2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_biomass_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_biomass_scaled2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(fishing_trend_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_effort_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
# 
#   model$data %>%
#     mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
#     mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
#     mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
#     mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
#     ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
#     scale_fill_gradient2() + gfplot::theme_pbs() +
#     facet_wrap(~species)
# 
# }
# 
# 
# if (y_type == "vel") {
# 
#   ggplot(model$data, aes(eta, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
# 
#   ggplot(model$data, aes(squashed_biotic_vel)) + geom_histogram() +
#     facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(squashed_temp_vel, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + scale_x_continuous(trans = fourth_root_power) +
#     facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(squashed_DO_vel, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + scale_x_continuous(trans = fourth_root_power) +
#     facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_temp, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_DO, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_temp^2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(mean_DO^2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_biomass_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_biomass_scaled2, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(fishing_trend_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
#   
#   ggplot(model$data, aes(log_effort_scaled, residual)) + geom_point(alpha = 0.2) +
#     geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")
# 
#   model$data %>%
#     mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
#     mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
#     mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
#     mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
#     ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
#     scale_fill_gradient2() + gfplot::theme_pbs() +
#     facet_wrap(~species)
# }
# 
# dev.off()

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
