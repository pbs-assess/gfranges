library(TMB)
library(dplyr)
library(ggplot2)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats$genus <- tolower(stats$group)

#### LOAD MATURE VOCC DATA 
# model_age <- "multi-spp-biotic-vocc-mature"
# model_age <- "scrambled-vocc-mature"
# model_age <- "scrambled2-vocc-mature"
# model_age <- "scrambled3-vocc-mature"
# model_age <- "scrambled4-mature"
# model_age <- "scrambled5-mature"
# model_age <- "all-temp-mature"
# d <- readRDS(paste0("data/", model_age, "-with-fished.rds"))

model_age <- "mature-all-temp"

d <- readRDS(paste0("data/", model_age, "-with-null.rds"))
# d <- na.omit(d) %>% as_tibble()

d <- readRDS("data/mature-all-temp-with-null-and-stats.rds")

d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  # filter(species != "Bocaccio") %>%
  # filter(species != "Sand Sole") %>%
  # filter(species != "Longspine Thornyhead") %>%
  filter(species != "Shortbelly Rockfish")

# #### LOAD IMMATURE VOCC DATA 
# model_age <- "multi-spp-biotic-vocc-immature"
# d <- readRDS(paste0("data/", model_age, "-with-fished.rds"))
# d <- na.omit(d) %>% as_tibble()
# 
d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  # filter(species != "Curlfin Sole") %>%
  # filter(species != "Bocaccio") %>%
  filter(species != "Longspine Thornyhead")

select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>%
  as.data.frame()


#### PREP TEMP VARIABLES ####
d$squashed_temp_vel <- collapse_outliers(d$temp_vel, c(0.005, 0.995))
# plot(squashed_do_vel ~ squashed_temp_vel, data = d, col = "#00000010")
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)
d$mean_temp_scaled <- scale(d$mean_temp)
hist(d$mean_temp_scaled)
hist(d$temp_trend)
d$temp_trend_scaled <- scale(d$temp_trend, center = FALSE)
hist(d$temp_trend_scaled)
d$temp_grad_scaled <- scale(d$temp_grad)
hist(d$temp_grad_scaled)
# d$temp_grad_scaled <- scale(sqrt(d$temp_grad))
# hist(d$temp_grad_scaled)

ggplot(d, aes(x,y, color=squashed_temp_vel)) + geom_point(size=2, shape=15) + scale_color_gradient2(low = "deepskyblue4", high ="red", trans = fourth_root_power)
# ggplot(d, aes(x,y, color=temp_trend)) + geom_point(size=2, shape=15) + scale_color_gradient2()
ggplot(d, aes(x,y, color=temp_trend_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2(low = "deepskyblue4", high ="red")
ggplot(d, aes(x,y, color=temp_grad)) + geom_point(size=2, shape=15) + scale_color_gradient2( high = "deepskyblue4",trans = sqrt)
ggplot(d, aes(x,y, color=temp_grad_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2(high = "deepskyblue4")
ggplot(d, aes(x,y, color=mean_temp_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2(low = "deepskyblue4", high ="red")

# #### PREP DO VARIABLES ####
# 
# d$squashed_do_vel <- collapse_outliers(d$DO_vel, c(0.005, 0.995))
# d$squashed_do_vel_scaled <- scale(d$squashed_do_vel, center = FALSE)
# # hist(d$mean_DO)
# d$mean_DO_scaled <- scale(d$mean_DO)
# hist(d$mean_DO_scaled)
# # hist(d$DO_trend)
# d$DO_trend_scaled <- scale(d$DO_trend, center = FALSE)
# hist(d$DO_trend_scaled)
# d$DO_grad_scaled <- scale(d$DO_grad)
# hist(d$DO_grad_scaled)
# # d$DO_grad_scaled <- scale(sqrt(d$DO_grad))
# # hist(d$DO_grad_scaled)
# ggplot(d, aes(x,y, color=squashed_do_vel)) + geom_point(size=2, shape=15) + scale_color_gradient2(trans = fourth_root_power)
# # ggplot(d, aes(x,y, color=DO_trend)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_trend_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_grad)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=DO_grad_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# ggplot(d, aes(x,y, color=mean_DO_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2()
# 
# plot(DO_grad~temp_grad, data = d, col = "#00000010")
# d$grad_diff <- d$DO_grad - d$temp_grad 
# 
# hist(d$grad_diff)
# ggplot(d, aes(x,y, color=grad_diff)) + geom_point(size=2, shape=15) + scale_color_gradient2()

#### PREP FISHING VARIABLES ####

# hist(d$mean_effort)
d$sqrt_effort <- sqrt(d$mean_effort)
d$sqrt_effort_scaled <- scale(sqrt(d$mean_effort), center=F)

d$fishing_trend_scaled <- scale(d$fishing_trend, center=F)
hist(d$fishing_trend_scaled)

# hist(sqrt(d$mean_effort))
d$log_effort <- log(d$mean_effort)
# hist(log(d$mean_effort))

ggplot(d, aes(x,y, color=log(mean_effort+1))) + geom_point(size=2, shape=15) + scale_color_gradient2(low = "deepskyblue4", high ="red") #, trans = fourth_root_power)
ggplot(d, aes(x,y, color=fishing_trend_scaled)) + geom_point(size=2, shape=15) + scale_color_gradient2(low = "deepskyblue4", high ="red") #, trans = fourth_root_power)


#### PREP FISH BIOMASS VARIABLES ####
d$mean_biomass_scaled <- scale((d$mean_biomass))
d$log_biomass_scaled <- scale(log(d$mean_biomass))
# hist(d$log_biomass_scaled)
d$log_sd_est <- scale(log(d$sd_est))
# hist(d$log_sd_est)
d$log_sd_est2 <- d$log_sd_est^2
# d$abs_biotic <- sqrt((d$biotic_trend)^2)
# hist(log(d$abs_biotic))
# plot(log(abs_biotic)~log(sd_est), data=d, col = "#00000010")

d %>% filter(species == "Pacific Cod") %>% ggplot(aes(x,y, color=biotic_trend)) + geom_point(size=1.5, shape=15) + scale_color_gradient2() 
d %>% filter(species == "Pacific Cod") %>% ggplot(aes(x,y, color=log_sd_est)) + geom_point(size=1.5, shape=15) + scale_color_gradient2()
# d  %>%  ggplot(aes(x,y, color=temp_grad)) + geom_point(size=1.5, shape=15) + scale_color_gradient2()


########################
#### CHOOSE Y 
########################
# hist((d$sd_est))
# hist(log(d$sd_est))
# # hist(log(d$biotic_CV))
# y <- log(d$sd_est)
# # y <- log(d$biotic_CV))

y <- d$biotic_trend

y <- d$fake_trend
hist(y)


############################
############################
#### TREND-BASED COVARIATES

formula <- ~ temp_trend_scaled +
  # DO_trend_scaled +
  # log_sd_est + log_sd_est2 +
  # mean_DO_scaled +  DO_trend_scaled:mean_DO_scaled +
  mean_temp_scaled + temp_trend_scaled:mean_temp_scaled +
  sqrt_effort_scaled + fishing_trend_scaled + fishing_trend_scaled:sqrt_effort_scaled +
  log_biomass_scaled + temp_trend_scaled:log_biomass_scaled +
  temp_grad_scaled + temp_trend_scaled:temp_grad_scaled#+
  # grad_diff + grad_diff:DO_trend_scaled +
  # mean_DO_scaled:mean_temp_scaled 

x <- model.matrix(formula, data = d)

d_pj1 <- interaction_df(d, formula,
  x_variable = "temp_trend_scaled",
  split_variable = "mean_temp_scaled",
  N = 5
)
## d_pj1$`(Intercept)` <- 0 # don't include intercept

# d_pj2 <- interaction_df(d,
#   formula = formula,
#   x_variable = "DO_trend_scaled",
#   split_variable = "mean_DO_scaled",
#   N = 5
# )

## d_pj3 <- interaction_df(d, formula,
#   x_variable = "mean_temp_scaled",
#   split_variable = "mean_DO_scaled",
#   # use_quantiles = FALSE,
#   N = 5
# )
# d_pj4 <- interaction_df(d, formula,
#   x_variable = "mean_DO_scaled",
#   split_variable = "mean_temp_scaled",
#   # use_quantiles = FALSE,
#   N = 5
# )

d_pj2 <- interaction_df(d,
  formula = formula,
  x_variable = "temp_trend_scaled",
  split_variable = "log_biomass_scaled",
  N = 5
)

d_pj3 <- interaction_df(d, formula,
  x_variable = "temp_trend_scaled",
  split_variable = "temp_grad_scaled",
  # use_quantiles = FALSE,
  N = 5
)



X_pj <- as.matrix(bind_rows(
  select(d_pj1, -chopstick, -species, -genus),
  select(d_pj2, -chopstick, -species, -genus),
 select(d_pj3, -chopstick, -species, -genus)#,
# select(d_pj4, -chopstick, -species, -genus)
))

pred_dat <- bind_rows(
  mutate(d_pj1, type = "temp"),
  # mutate(d_pj2, type = "do"))
mutate(d_pj2, type = "biomass"),
mutate(d_pj3, type = "temp_grad"))
# mutate(d_pj2, type = "do"),
# mutate(d_pj3, type = "mean_temp"),
# mutate(d_pj4, type = "mean_do"))

trend_reg <- vocc_regression(d, y,
  X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
  knots = 200, group_by_genus = FALSE, student_t = F
)

# saveRDS(trend_reg, file = paste0("data/trend_by_trends_01-21-", model_age, ".rds"))
# saveRDS(trend_reg, file = paste0("data/trend_by_all_temp_01-23-", model_age, ".rds"))
saveRDS(trend_reg, file = paste0("data/trend_01-24-", model_age, ".rds"))

# trend_reg_genus <- vocc_regression(d, y,
#   X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
#   knots = 200, group_by_genus = T, student_t = F
# )
# 
# saveRDS(trend_reg_genus, file = paste0("data/trend_by_trend_only_01-20-", model_age, "-genus.rds"))

############################
# #### VELOCITY VARIABLES
# formula <- ~ mean_DO_scaled + mean_temp_scaled +
#   mean_biomass_scaled +
#   mean_temp_scaled:mean_DO_scaled +
#   squashed_do_vel_scaled + squashed_do_vel_scaled:mean_DO_scaled +
#   squashed_temp_vel_scaled + squashed_temp_vel_scaled:mean_temp_scaled
# 
# x <- model.matrix(formula, data = d)
# 
# d_pj1 <- interaction_df(d, formula,
#   x_variable = "squashed_temp_vel_scaled",
#   split_variable = "mean_temp_scaled",
#   N = 5
# )
# d_pj2 <- interaction_df(d,
#   formula = formula,
#   x_variable = "squashed_do_vel_scaled",
#   split_variable = "mean_DO_scaled",
#   N = 5
# )
# 
# X_pj <- as.matrix(bind_rows(
#   select(d_pj1, -chopstick, -species, -genus),
#   select(d_pj2, -chopstick, -species, -genus),
#   select(d_pj3, -chopstick, -species, -genus),
#   select(d_pj4, -chopstick, -species, -genus)
# ))
# 
# pred_dat <- bind_rows(
#   mutate(d_pj1, type = "temp"),
#   mutate(d_pj2, type = "do"),
#   mutate(d_pj3, type = "mean_temp"),
#   mutate(d_pj4, type = "mean_do")
# )
# 
# trend_reg <- vocc_regression(d, y,
#   X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
#   knots = 200, group_by_genus = FALSE, student_t = FALSE
# )
#
# saveRDS(trend_reg, file = paste0("data/trend_by_vel_01-??-", model_age, ".rds"))
############################


##############################
#### LOAD VELOCITY MODELS ####
# model <- readRDS("data/trend_by_vel_01-16-multi-spp-biotic-vocc-mature-chopsticks3.rds")
# 
#### VELOCITY MODEL CHOPSTICKS  
# plot_fuzzy_chopsticks(model,
#   x_variable = "squashed_temp_vel_scaled", type = "temp",
#   y_label = "Predicted biomass trend"
# ) +
#   ggtitle("Interation plots for mature abundance")
# # ggtitle("Interation plots for immature abundance")
# 
# plot_fuzzy_chopsticks(model,
#   x_variable = "squashed_do_vel_scaled", type = "do",
#   y_label = "Predicted biomass trend"
# ) +
#   ggtitle("Interation plots for mature abundance")
# # ggtitle("Interation plots for immature abundance")


##############################
#### LOAD TREND MODELS ####

#### ONE JUST BUILT
model <- trend_reg 
model <- trend_reg_genus 

#### SAVED MODELS

# model <- readRDS(("data/trend_by_all_temp_w_fishing_01-23-all-temp-mature.rds"))
# model <- readRDS(("data/trend_by_all_temp_01-23-all-temp-mature.rds"))

model <- readRDS("data/trend_01-24-mature-all-temp-null.rds")

##############################
#### TREND MODEL CHOPSTICKS  
title_all <- "Mature interation plots for all temp years"

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted biomass trend"
  #y_label = "Predicted biomass trend"
) + #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", model_age, ")"))
# ggtitle("Interation plots for immature abundance")

# plot_fuzzy_chopsticks(model,
#   x_variable = "DO_trend_scaled", type = "do",
#   y_label = "Predicted biomass trend"
# ) +
#   ggtitle("Interation plots for mature abundance")
# # ggtitle("Interation plots for immature abundance")

# plot_fuzzy_chopsticks(model,
#   x_variable = "mean_temp_scaled", type = "mean_temp",
#   y_label = "SD in estimated biomass"
#   #y_label = "Predicted biomass trend"
# ) +
ggtitle(paste(title_all))

# plot_fuzzy_chopsticks(model,
#   x_variable = "mean_DO_scaled", type = "mean_do",
#   y_label = "SD in estimated biomass"
#   #y_label = "Predicted biomass trend"
# ) +
ggtitle(paste(title_all))

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "biomass",
  y_label = "Predicted biomass trend"
  #y_label = "Predicted biomass trend"
) + #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all))

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp_grad",
  y_label = "Predicted biomass trend"
  #y_label = "Predicted biomass trend"
) + facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all))

# ggsave("figs/interation-plot-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)


##############################
#### STATISTICS AND PLOTS FOR ALL MODEL TYPES ####

coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)


# model <- readRDS("data/trend_by_sd_01-21-multi-spp-biotic-vocc-mature.rds")
# model <- readRDS("data/trend_by_sd_01-21-scrambled3-vocc-mature.rds")

model2 <- add_colours(model$coefs) #%>% filter(species != "Shortbelly Rockfish")

# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# mature <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")
# model2 <- add_colours(mmature$coefs) # must be saved as model2 for use in function below
# model2 <- add_colours(model$coefs, last_used = TRUE)

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by) #+ ylim(-1,0.5)
  },
order_by = manipulate::picker(as.list(sort(unique(shortener(model2$coefficient)))))
)

manipulate::manipulate({
  plot_coefs(model2, order_by_trait = T, fixed_scales = F, order_by = order_by)
},
order_by = manipulate::picker(as.list(sort(names(model2[, 6:15]))))
)

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

# library(ggsidekick) # for fourth_root_power if gfranges not loaded
ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(trans = fourth_root_power) +
  facet_wrap(~species)

# ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)

r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i

model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() +
  facet_wrap(~species)

# ggsave("figs/vel-model-residuals.png", width = 12, height = 12, dpi = 300)
# ggsave("figs/trend-model-residuals.png", width = 12, height = 12, dpi = 300)

norm_resids <- qres_student(model)
norm_resids <- norm_resids[is.finite(norm_resids)]
# qqnorm(norm_resids)
hist(norm_resids)

# norm_resids <- qres_student(model_genus)
# norm_resids <- norm_resids[is.finite(norm_resids)]
# hist(norm_resids)

# qqnorm(model$data$residual)
# qqline(model$data$residual)


### SAVE PLOT WITH SELECTED PARAMS
# model3 <- plot_coefs(model2, order_by = "squashed_temp_vel_scaled)")
# model_plot <- model3 +
#   ggtitle(paste("Mature biomass trend"))
#   # ggtitle(paste("Immature biotic trend"))
# model_plot
# ggsave("figs/worm-plot-temp-trend-sort.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/worm-plot-imm-temp-trend-sort.png", width = 10, height = 10, dpi = 300)
