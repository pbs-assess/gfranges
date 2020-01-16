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
#model_age <- "scrambled-vocc-mature"
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


d$squashed_do_vel <-  collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$squashed_temp_vel <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))

d$do_vel_squashed <-  collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$temp_vel_squashed <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))
plot((do_vel_squashed)~(temp_vel_squashed), data=d, col = "#00000010")

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

d$mean_temp_scaled <- scale(d$mean_temp)
d$mean_biomass_scaled <- scale(d$mean_biomass)
d$mean_DO_scaled <- scale(d$mean_DO)
d$squashed_do_vel_scaled <- scale(d$squashed_do_vel, center = FALSE)
d$squashed_temp_vel_scaled <- scale(d$squashed_temp_vel, center = FALSE)

formula <- ~ mean_DO_scaled + mean_temp_scaled +
  mean_biomass_scaled + 
  #scale(sqrt_effort, center = F) + 
  #scale(fishing_trend, center = F) +
  #sqrt(mean_effort):scale(fishing_trend) +
  mean_temp_scaled:mean_DO_scaled + 
  # scale(squashed_temp_vel):scale(squashed_do_vel) +
  squashed_do_vel_scaled + squashed_do_vel_scaled:mean_DO_scaled + 
  squashed_temp_vel_scaled + squashed_temp_vel_scaled:mean_temp_scaled

x <- model.matrix(formula, data = d)

d_pj1 <- interaction_df(d, formula = formula,
  x_variable = "squashed_temp_vel_scaled",
  split_variable = "mean_temp_scaled", N = 8)
d_pj1$`(Intercept)` <- 0 # don't include intercept
d_pj2 <- interaction_df(d, formula = formula,
  x_variable = "squashed_do_vel_scaled",
  split_variable = "mean_DO_scaled", N = 8)
d_pj2$`(Intercept)` <- 0 # don't include intercept
X_pj <- as.matrix(bind_rows(select(d_pj1, -species, -genus), 
  select(d_pj2, -species, -genus)))
pred_dat <- bind_rows(mutate(d_pj1, type = "velocity"), mutate(d_pj2, type = "do"))

head(x)
hist(x[,2])
hist(x[,7])
hist(x[,8])
trend_by_vel <- vocc_regression(d, y, X_ij = x, X_pj = X_pj, pred_dat = pred_dat,
  knots = 200, group_by_genus = FALSE, student_t = FALSE)
trend_by_vel$sdr
saveRDS(trend_by_vel, file = paste0("data/trend_by_vel_01-16-", model_age, ".rds"))

# example plot:
par_est <- as.list(trend_by_vel$sdr, "Estimate", report = TRUE)
par_sd <- as.list(trend_by_vel$sdr, "Std. Error", report = TRUE)
pred_dat$est_p <- par_est$eta_p
pred_dat$sd_p <- par_sd$eta_p
filter(pred_dat, type == "do") %>%
  ggplot(aes(squashed_do_vel_scaled, est_p)) +
  geom_line(aes(colour = chopstick)) +
  geom_ribbon(aes(fill = chopstick, 
    ymin = est_p - 1.96 * sd_p, ymax = est_p + 1.96 * sd_p), alpha = 0.3) +
  facet_wrap(vars(species))


# ### TRY FOR SD OF LOG BIOMASS
# hist(log(d$sd_est))
# hist(log(d$biotic_CV))
# y <- log(d$sd_est)
# y <- log(d$biotic_CV))
# sd_reg <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)
# sd_reg$sdr
# 
# saveRDS(sd_reg, file = "data/sd_with_mean_biomass_mature_01-07.rds")


# ### BIOTIC TRENDS AS RESPONSE INSTEAD OF VELOCITY

d$do_trend <- d$DO_trend
# hist(scale(d$fishing_trend))
# hist(scale(d$log_effort))
# hist(sqrt(d$mean_effort))


x <- model.matrix(~scale(mean_temp) + scale(mean_DO) + 
    scale(mean_biomass) +
    sqrt(mean_effort) + 
    #scale(log_effort) + 
    scale(fishing_trend) +
    #scale(log_effort):scale(fishing_trend) +
    scale(mean_temp):scale(mean_DO) + 
    scale(do_trend) + scale(mean_DO):scale(do_trend) +
    scale(temp_trend) + scale(mean_temp):scale(temp_trend)
, data = d)

trend_reg <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = FALSE, student_t = FALSE)
trend_reg$sdr

saveRDS(trend_reg, file = "data/trend_with_fishing_sqrt_effort_01-10.rds")
# saveRDS(trend_reg, file = "data/trend_interacting_with_means_immature_01-08.rds")

# trend_reg_genus <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = TRUE, student_t = FALSE)
# trend_reg_genus$sdr
# get_aic(trend_reg_genus) - get_aic(trend_reg)

# saveRDS(trend_reg_genus, file = "data/trend_reg_genus_mature_01-06.rds")

############################
# 
# y <- collapse_outliers(d$biotic_vel, c(0.005, 0.995))
# hist(y)
# 
# vel_reg <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)
# vel_reg$sdr
# 
# vel_reg_genus <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = TRUE, student_t = TRUE, nu = 5)
# vel_reg_genus$sdr
# 
# get_aic(vel_reg_genus) - get_aic(vel_reg)
# get_aic(vel_reg) - get_aic(model)

# saveRDS(vel_reg, file = "data/interacting_vel_with_means_mature_01-06.rds")
# saveRDS(vel_reg, file = "data/interacting_only_with_means_mature_01-06.rds")
# saveRDS(vel_reg, file = "data/vel_interacting_with_means_mature_01-07.rds")
# saveRDS(vel_reg_genus, file = "data/vel_interacting_with_means_mature_genus_01-07.rds")
# saveRDS(vel_reg, file = "data/vel_with_fishing_interaction_01-10.rds")
# saveRDS(vel_reg, file = "data/vel_with_fishing_trend_01-10.rds")
# saveRDS(vel_reg, file = "data/vel_with_fishing_sqrt_effort_only_01-10.rds")
# 
# saveRDS(vel_reg, file = "data/scrambled_vel_interacting_01-10.rds")
# saveRDS(vel_reg, file = "data/scrambled_vel_interacting_and_effort_01-10.rds")
# saveRDS(vel_reg, file = "data/scrambled2_vel_interacting_and_effort_01-10.rds")
# saveRDS(vel_reg, file = "data/scrambled3_vel_interacting_and_effort_01-10.rds")
# saveRDS(vel_reg, file = "data/vel_interacting_with_means_immature_01-08.rds")

############################

# model <- readRDS(file = "data/interacting_vel_with_means_mature_01-06.rds") #164942.4
# model <- readRDS(file = "data/interacting_only_with_means_mature_01-06.rds") #164943.1
# model <- readRDS(file = "data/interacting_with_means_mature_01-06.rds") #164816.8
# model <- readRDS(file =  "data/vel_interacting_with_means_mature_01-07.rds")
# model <- readRDS(file = "data/vel_interacting_with_means_immature_01-08.rds")
# model <- readRDS("data/vel_with_fishing_interaction_01-10.rds")
# model <- readRDS("data/vel_with_fishing_trend_01-10.rds")
# model <- readRDS("data/vel_with_fishing_effort_01-10.rds")
# model <- readRDS("data/vel_with_fishing_effort_no_biomass_01-10.rds")
# model <- readRDS("data/vel_with_fishing_sqrt_effort_only_01-10.rds")
# model <- readRDS("data/scrambled_vel_interacting_01-10.rds")
# model <- readRDS("data/scrambled_vel_interacting_and_effort_01-10.rds")
# model <- readRDS("data/scrambled2_vel_interacting_and_effort_01-10.rds")
# model <- readRDS("data/scrambled3_vel_interacting_and_effort_01-10.rds")

model <- readRDS(file = "data/trend_interacting_with_means_mature_01-08.rds")
model <- readRDS(file = "data/trend_interacting_with_means_immature_01-08.rds")
# model <- readRDS(file = "data/trend_by_vel_mature_01-08.rds")
# model <- readRDS(file = "data/trend_by_vel_mature_simplified_01-09.rds")

model <- readRDS( "data/trend_with_fishing_vars_01-10.rds")
model <- readRDS( "data/trend_with_fishing_sqrt_effort_01-10.rds")
model <- readRDS( "data/trend_by_vel_with_fishing_sqrt_effort_01-10.rds")

model <- readRDS("data/trend_by_vel_with_fishing_01-15.rds")
model <- readRDS("data/trend_by_vel_with_fishing_01-15-scrambled3-vocc-mature.rds")
model <- readRDS("data/trend_by_vel_with_fishing_01-15_mature.rds")
#saveRDS(model, file = paste0("data/trend_by_vel_with_fishing_01-15.rds"))

model <- trend_by_vel
# model <- sd_reg
# model <- trend_reg
# get_aic(vel_reg)
# get_aic(trend_reg)
get_aic(model)

coef_names <- shortener(unique(model$coefs$coefficient))
coef_names 


s <- summary(model$sdr)
as.data.frame(s[grep("^b_j$", row.names(s)), , drop = FALSE])
betas <- as.list(model$sdr, "Estimate")$b_j
SE <- as.list(model$sdr, "Std. Error")$b_j
lowerCI <- signif(betas + SE*qnorm(0.025))
upperCI <- signif(betas + SE*qnorm(0.975))
overall_betas <-as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

model2 <- add_colours(model$coefs) 


# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# model2 <- add_colours(model$coefs, last_used = TRUE ) 

model2a <- model2 %>% filter(coefficient != "scale(mean_temp)") %>% 
  filter(coefficient != "scale(mean_DO)") %>% 
  filter(coefficient != "scale(mean_biomass)") %>% 
  filter(coefficient != "scale(mean_temp):scale(mean_DO)") %>%
  filter(coefficient != "scale(mean_DO):scale(mean_temp)") 
 
manipulate::manipulate({plot_coefs(model2a, fixed_scales = F, order_by = order_by)}, 
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2a$coefficient))))))

model2b <- model2 %>% filter(coefficient %in% c("scale(mean_temp)", "scale(mean_DO)", "scale(mean_biomass)", "scale(mean_temp):scale(mean_DO)", "scale(mean_DO):scale(mean_temp)"))

manipulate::manipulate({plot_coefs(model2b, order_by = order_by)}, 
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))

manipulate::manipulate({plot_coefs(model2a, order_by_trait = T, fixed_scales = F, order_by = order_by)},
  order_by = manipulate::picker(as.list(sort(names(model2[, 6:15])))))

# nd <- interaction_df(model) 
# predict.sdmTMB(model, newdata = nd)
p <- plot_interaction (model = model, 
  variables = c("mean_temp", "squashed_temp_vel"), choose_x = 2)

p <- plot_interaction (model = model, 
  variables = c("mean_DO", "squashed_do_vel"), choose_x = 2) + 
  scale_colour_manual(values = c("#5E4FA2", "#FDAE61"))

p <- plot_interaction (model = model, 
  variables = c("mean_DO", "mean_temp"), choose_x = 1) 

p <- plot_interaction (model = model, 
  variables = c("mean_DO", "mean_temp"), choose_x = 2) + 
  scale_colour_manual(values = c("#5E4FA2", "#FDAE61"))

p + ylab("Predicted biotic velocity") +
  # ylab("Predicted biotic variability") +
  # ylim(-12, 15) +
  # xlim(-5, 6.8) +
  ggtitle("Interation plots for mature abundance")

ggsave("figs/interation-plot-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)

# p + ylab("Predicted biotic velocity") +
#   ggtitle("Interation plots for immature abundance")
# 
# ggsave("figs/interation-plot-imm-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# 
# p <- plot_interaction (model = model, 
#   variables = c("mean_temp", "temp_trend"), choose_x = 2
# )

p <- plot_interaction (model = model, variables = c("mean_DO", "do_trend")
  , choose_x = 2
) + scale_colour_manual(values = c("#5E4FA2", "#FDAE61")
)
p +
  ylab("Predicted biotic trend") +
  ggtitle("Interation plots for mature abundance")

ggsave("figs/interation-plot-trend-by-temp.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-do.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)

ggsave("figs/interation-plot-trend-by-temp_vel.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)

# p +
#   ylab("Predicted biotic trend") +
#   ggtitle("Interation plots for immature abundance")

# ggsave("figs/interation-plot-imm-trend-by-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)


# print(sdmTMB:::get_convergence_diagnostics(model))
# print(sdmTMB:::get_convergence_diagnostics(model_genus))


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
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>%  # compress tails
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
# # qqnorm(norm_resids)
# hist(norm_resids)

# qqnorm(model$data$residual)
# qqline(model$data$residual)


### SAVE PLOT WITH SELECTED PARAMS
# model2 <- model2 %>% 
#   #filter(coefficient != "scale(squashed_temp_vel)")
#   #filter(coefficient != "scale(squashed_do_vel)")
#   filter(coefficient != "scale(mean_DO):scale(mean_temp)")

# model3 <- plot_coefs(model2, order_by = "scale(mean_temp):scale(squashed_temp_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_DO):scale(squashed_do_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(squashed_temp_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(squashed_do_vel)")
#
# model_plot <- model3 + 
#   ggtitle(paste("Mature biotic velocity"))
#   ggtitle(paste("Immature biotic velocity"))
#   # ggtitle(paste("Mature biotic variability")) 
# model_plot 
# 
# ggsave("figs/worm-plot-vel-do-vel-with-biomass.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/worm-plot-vel-imm-do-vel-with-biomass.png", width = 10, height = 10, dpi = 300)
# 
# model3 <- plot_coefs(model2, order_by = "scale(do_trend)")
# model3 <- plot_coefs(model2, order_by = "scale(temp_trend)")
# #model3 <- plot_coefs(model2, order_by = "scale(do_trend):scale(temp_trend)")
# 
# ## Ordered by increasing depth and split by rockfish or not
# model3 <- plot_coefs(model2, order_by = "large_depth")
# ## Ordered by increasing depth and split by rockfish or not
# model3 <- plot_coefs(model2, order_by = "large_depth")
#
# model_plot <- model3 + 
#   ggtitle(paste("Mature biomass trend"))
#   # ggtitle(paste("Immature biotic velocity"))
# model_plot 
# 
# png(
#   file = paste0("figs/", model, "100-knots-29-spp.png"),
#   res = 600,
#   units = "in",
#   width = 8.5,
#   height = 11
# )
# gridExtra::grid.arrange(
#   grobs = c(list(
#     bio_temp_plot,
#     bio_do_plot,
#     trend_temp_plot,
#     trend_do_plot,
#     CV_temp_plot,
#     CV_do_plot
#   )),
#   nrow = 3,
#   top = grid::textGrob(paste(model, " knots = 100"))
# )
# dev.off()
