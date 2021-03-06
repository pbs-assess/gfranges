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

# model <- "multi-spp-biotic-vocc"
model_age <- "multi-spp-biotic-vocc-mature"
# model_age <- "scrambled-vocc-mature"
# model_age <- "scrambled2-vocc-mature"
# model_age <- "scrambled3-vocc-mature"
d <- readRDS(paste0("data/", model_age, "-with-fished.rds"))
d <- na.omit(d) %>% as_tibble()

d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  filter(species != "Bocaccio") %>%
  filter(species != "Sand Sole") %>%
  filter(species != "Longspine Thornyhead") %>%
  filter(species != "Shortbelly Rockfish")

# model_age <- "multi-spp-biotic-vocc-immature"
# d <- readRDS(paste0("data/", model_age, "with-fished.rds"))
# d <- na.omit(d) %>% as_tibble()
#
# d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
#   filter(species != "Curlfin Sole") %>%
#   filter(species != "Longspine Thornyhead")

select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>%
  as.data.frame()


d$squashed_do_vel <- collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$squashed_temp_vel <- collapse_outliers(d$temp_vel, c(0.005, 0.995))

d$do_vel_squashed <- collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$temp_vel_squashed <- collapse_outliers(d$temp_vel, c(0.005, 0.995))
plot((do_vel_squashed) ~ (temp_vel_squashed), data = d, col = "#00000010")

d$squashed_biomass <- (collapse_outliers(d$mean_biomass, c(0.005, 0.995)))
# plot((DO_trend)~(temp_trend), data=d, col = "#00000010")
# cor(d$DO_trend, d$temp_trend)
#
# plot((temp_vel_squashed)~(temp_trend), data=d, col = "#00000010")
# cor(d$temp_vel_squashed, d$temp_trend)
#
# plot(do_vel_squashed ~ do_trend, data=d, col = "#00000010")
# cor(d$do_vel_squashed, d$do_trend)

# ggplot(d, aes(x, y, colour = temp_vel_squashed)) +
#   geom_point() +
#   facet_wrap(~species) +
#   scale_color_viridis_c(trans = sqrt)
#
# ggplot(d, aes(x, y, colour = collapse_outliers(d$biotic_vel, c(0.005, 0.995)))) +
#   geom_point(size=0.25) +
#   facet_wrap(~species) + scale_color_viridis_c()

# hist((d$sd_est))
# hist(log(d$sd_est))
# hist(log(d$biotic_CV))
y <- log(d$sd_est)
# y <- log(d$biotic_CV))

hist((d$biotic_trend))
y <- d$biotic_trend
hist(y)

mean((d$squashed_do_vel))
hist(scale(d$squashed_do_vel))
d$sqrt_effort <- sqrt(d$mean_effort)
d$log_effort <- log(d$mean_effort)

hist(log(d$mean_effort))
hist(sqrt(d$mean_effort))
hist(d$fishing_trend)
hist(d$temp_trend)
hist(d$DO_trend)
hist(d$DO_grad)
# plot(d$squashed_temp_vel_scaled ~ d$temp_trend)
# plot(d$squashed_temp_vel_scaled ~ d$temp_grad)

d$mean_temp_scaled <- scale(d$mean_temp)
d$mean_biomass_scaled <- scale(d$mean_biomass)
d$mean_DO_scaled <- scale(d$mean_DO)
d$squashed_do_vel_scaled <- scale(d$squashed_do_vel, center = FALSE)
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)
d$temp_trend_scaled <- scale(d$temp_trend, center = FALSE)
d$DO_trend_scaled <- scale(d$DO_trend, center = FALSE)
d$temp_grad_scaled <- scale(d$temp_grad)
d$DO_grad_scaled <- scale(d$DO_grad)

f <- function(sp) {
  g1 <- filter(d, species == sp) %>% ggplot(aes(x, y, fill = biotic_trend)) + geom_tile(width = 4, height = 4 ) + scale_fill_gradient2() + coord_fixed()
  g2 <- filter(d, species == sp) %>% ggplot(aes(x, y, fill = temp_trend_scaled)) + geom_tile(width = 4, height = 4 ) + scale_fill_viridis_c() + coord_fixed()
    g4 <- filter(d, species == sp) %>% ggplot(aes(x, y, fill = mean_temp_scaled)) + geom_tile(width = 4, height = 4 ) + scale_fill_viridis_c() + coord_fixed()

  g3 <- filter(d, species == sp) %>%
    mutate(temp_cat = ifelse(mean_temp_scaled > median(mean_temp_scaled), TRUE, FALSE)) %>%
    ggplot(aes(temp_trend_scaled, biotic_trend, colour = temp_cat)) +
    geom_point() +
    stat_smooth(se = T, method = "lm")

    g5 <- filter(d, species == sp) %>%
    ggplot(aes(mean_temp_scaled, biotic_trend)) +
    geom_point() +
    stat_smooth(se = T, method = "lm")

  print(nrow(filter(d, species == sp)))
  cowplot::plot_grid(g1, g2, g4, g3, g5, ncol = 2, align = "h")
}
f("Splitnose Rockfish")
f("Yellowmouth Rockfish")
f("Walleye Pollock")
f("Yelloweye Rockfish")
f("Silvergray Rockfish")
f("Dover Sole")
f("Sablefish")
f("Greenstriped Rockfish")
f("Redbanded Rockfish")

#### TREND-BASED VARIABLES
formula <- ~ mean_DO_scaled + mean_temp_scaled +
  mean_biomass_scaled +
  mean_temp_scaled:mean_DO_scaled +
  temp_trend_scaled + temp_trend_scaled:mean_temp_scaled +
  DO_trend_scaled + DO_trend_scaled:mean_DO_scaled #+
# temp_grad_scaled + temp_grad_scaled:temp_trend_scaled +
# DO_grad_scaled + DO_grad_scaled:DO_trend_scaled

formula <- ~
  temp_trend_scaled

x <- model.matrix(formula, data = d)

d_pj1 <- interaction_df(d, formula,
  x_variable = "temp_trend_scaled",
  split_variable = "mean_temp_scaled",
  N = 5
)
# d_pj1$`(Intercept)` <- 0 # don't include intercept
d_pj2 <- interaction_df(d,
  formula = formula,
  x_variable = "DO_trend_scaled",
  split_variable = "mean_DO_scaled",
  N = 5
)
############################



#### VELOCITY VARIABLES
formula <- ~ mean_DO_scaled + mean_temp_scaled +
  mean_biomass_scaled +
  mean_temp_scaled:mean_DO_scaled +
  squashed_do_vel_scaled + squashed_do_vel_scaled:mean_DO_scaled +
  squashed_temp_vel_scaled + squashed_temp_vel_scaled:mean_temp_scaled

x <- model.matrix(formula, data = d)

d_pj1 <- interaction_df(d, formula,
  x_variable = "squashed_temp_vel_scaled",
  split_variable = "mean_temp_scaled",
  N = 5
)
d_pj2 <- interaction_df(d,
  formula = formula,
  x_variable = "squashed_do_vel_scaled",
  split_variable = "mean_DO_scaled",
  N = 5
)
############################


##### FOR BOTH RUN FROM HERE ON
d_pj3 <- interaction_df(d, formula,
  x_variable = "mean_temp_scaled",
  split_variable = "mean_DO_scaled",
  # use_quantiles = FALSE,
  N = 5
)
d_pj4 <- interaction_df(d, formula,
  x_variable = "mean_DO_scaled",
  split_variable = "mean_temp_scaled",
  # use_quantiles = FALSE,
  N = 5
)

X_pj <- as.matrix(bind_rows(
  select(d_pj1, -chopstick, -species, -genus),
  select(d_pj2, -chopstick, -species, -genus),
  select(d_pj3, -chopstick, -species, -genus),
  select(d_pj4, -chopstick, -species, -genus)
))

pred_dat <- bind_rows(
  mutate(d_pj1, type = "temp"),
  mutate(d_pj2, type = "do"),
  mutate(d_pj3, type = "mean_temp"),
  mutate(d_pj4, type = "mean_do")
)

head(x)
hist(x[, 2])
hist(x[, 7])
hist(x[, 8])

trend_reg <- vocc_regression(d, y,
  X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
  knots = 120, group_by_genus = FALSE, student_t = FALSE
)
# trend_reg$sdr

#### SAVE WITH APPROPRIATE NAME

saveRDS(trend_reg, file = paste0("data/trend_by_trend_only_01-17-", model_age, ".rds"))


#### CALL VELOCITY MODELS ####

model <- readRDS("data/trend_by_vel_01-16-multi-spp-biotic-vocc-mature-chopsticks3.rds")

plot_fuzzy_chopsticks(model,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = "Predicted biomass trend"
) +
  ggtitle("Interation plots for mature abundance")
# ggtitle("Interation plots for immature abundance")

plot_fuzzy_chopsticks(model,
  x_variable = "squashed_do_vel_scaled", type = "do",
  y_label = "Predicted biomass trend"
) +
  ggtitle("Interation plots for mature abundance")
# ggtitle("Interation plots for immature abundance")


#### CALL TREND MODELS ####

# model <- readRDS("data/trend_by_trend_01-16-multi-spp-biotic-vocc-mature.rds")
model <- readRDS("data/trend_by_trend_01-17-multi-spp-biotic-vocc-mature.rds")
model <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted biomass trend"
) +
  ggtitle("Interation plots for mature abundance")
# ggtitle("Interation plots for immature abundance")

plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "do",
  y_label = "Predicted biomass trend"
) +
  ggtitle("Interation plots for mature abundance")
# ggtitle("Interation plots for immature abundance")

# ggsave("figs/interation-plot-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)


#### STATISTICS AND PLOTS FOR ALL MODEL TYPES ####

coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)

model2 <- add_colours(model$coefs)
# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# model2 <- add_colours(model$coefs, last_used = TRUE )

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by)
},
order_by = manipulate::picker(as.list(sort(unique(shortener(model2$coefficient)))))
)

manipulate::manipulate({
  plot_coefs(model2, order_by_trait = T, fixed_scales = F, order_by = order_by)
},
order_by = manipulate::picker(as.list(sort(names(model2[, 6:15]))))
)

# model2a <- model2 %>%
#   #filter(coefficient != "squashed_temp_vel_scaled") %>%
#   #filter(coefficient != "mean_temp_scaled:squashed_temp_vel_scaled")
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

library(ggsidekick) # for fourth_root_power
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
