#########################
#########################
#### SUPPLEMENTARY FIGURES
#########################
#########################
# # if make-figs not just run
setwd(here::here())
library(TMB)
library(patchwork)
library(gfranges)
library(dotwhisker)
library(tidyverse)

#### load appropriate final models and other data

model_trend <- readRDS(here::here("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-1-600.rds"))
max(model_trend$sdr$gradient.fixed)

# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-18-vel-both-family-order-1-300.rds")
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-20-vel-both-family-1-350.rds")
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-20-vel-both-1-350.rds")
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-21-vel-both-1-400.rds")


# model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized3-11-18-vel-both-1-400.rds") 
# model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-27-vel-both-1-400.rds")
model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-1-600.rds"))
# model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-family-1-600.rds")
# model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-1-700-DO.rds")) # not converged
max(model_vel$sdr$gradient.fixed)

# model <- model_trend
# model <- model_vel

# model_fish <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-06-25-trend-w-fishing-1-500.rds")
# max(model_fish$sdr$gradient.fixed)

# stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav.rds"))
stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth2.rds")) %>% mutate(age = firstup(age))
#########################
#########################
### COEF PLOTS OF MAIN AND NULL MODELS ####
# need stats with lowercase age for now
stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth2.rds"))
model <- readRDS(here::here("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-1-600.rds"))
model <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-1-600.rds"))
model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-both-group-1-600-DO.rds") # with groups

# 
## nulls that are currently illustrated
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-sim-2-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-4-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-5-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-6-600.rds")
model2 <- add_colours(model$coefs, col_var = "mean_group")
model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
(p <- plot_coefs(model2, grouping_taxa = "species_id", grid_facets = T, fixed_scales = F, 
  # order_by = "Intercept"
  order_by = "temp_vel"
))
# colorblindr::cvd_grid(p)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-w-group.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null4nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null3nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null2nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null1nb.pdf"), width = 12.5, height = 8.5)
# 
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-2-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-4-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-5-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-6-600.rds")
# model2 <- add_colours(model$coefs, col_var = "mean_group") 
# model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
# plot_coefs(model2, grouping_taxa = "species_id", order_by = "temp_trend", grid_facets = T, fixed_scales = F)
# # ggsave(here::here("ms", "figs", "supp-all-trend-coefs.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null4nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null3nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null2nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null1nb.pdf"), width = 12.5, height = 8.5)

#########################
#########################
#### GLOBAL COEFS
### 
# model_grad <- readRDS("analysis/VOCC/data/trend-all-95-optimized3-11-24-trend-w-grad-1-400.rds")
# max(model_grad$sdr$gradient.fixed)

model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-temp-1-600.rds")
max(model_vel_t$sdr$gradient.fixed)

model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-30-vel-do-1-600.rds")
max(model_vel_d$sdr$gradient.fixed)

model_age <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-23-vel-w-age-1-400.rds")
model_age <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-w-age-1-500-DO.rds")
max(model_age$sdr$gradient.fixed)

# model_fish <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-19-vel-w-fishing-1-400.rds")
# model_fish <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-w-catch-1-500-DO.rds")
# max(model_fish$sdr$gradient.fixed)


## trend model ####
coef_names <- shortener(unique(model_trend$coefs$coefficient))
coef_names <- c(
  "intercept", "change in T", "mean T", "change in DO", "mean DO",
  "biomass", "interaction (T)", "interaction (DO)"
)
betas <- signif(as.list(model_trend$sdr, "Estimate")$b_j, digits = 3)/ sd(model_trend$y_i)
SE <- signif(as.list(model_trend$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_trend$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_trend$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_trend$y_i)
overall_betas <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas$model <- "trend"

# overall_betas_std <- overall_betas/ sd(model_trend$y_i)

###### TREND MODELS NOT INCLUDED ####
# ## trend model with temp only ####
model_temp <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-1-600.rds")
max(model_temp$sdr$gradient.fixed)
coef_names <- shortener(unique(model_temp$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", "biomass", "interaction (T)")
betas <- signif(as.list(model_temp$sdr, "Estimate")$b_j, digits = 3)/ sd(model_temp$y_i)
SE <- signif(as.list(model_temp$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_temp$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_temp$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_temp$y_i)
overall_betas_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_t$model <- "trend (T only)"

# ## trend model with DO only ####
model_do <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-12-01-trend-do-only-1-600.rds")
coef_names <- shortener(unique(model_do$coefs$coefficient))
coef_names <- c("intercept", "change in DO", "mean DO", "biomass", "interaction (DO)")
betas <- signif(as.list(model_do$sdr, "Estimate")$b_j, digits = 3)/ sd(model_do$y_i)
SE <- signif(as.list(model_do$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_do$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_do$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_do$y_i)
overall_betas_d <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_d$model <- "trend (DO only)"

## trend model with temp and gradient ####
# # version with 2-way interactions
# model_grad <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-23-trend-w-grad-1-500-temp-2-way.rds")
model_grad <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-12-01-trend-grad-1-600.rds")
# coef_names <- shortener(unique(model_grad$coefs$coefficient))
# coef_names <- c("intercept", "change in T", "mean T", "gradient", "biomass", "interaction (T)",  "mean T:Gradient",  "change in T:Gradient")

# # version with 3-way interaction
coef_names <- shortener(unique(model_grad$coefs$coefficient))
coef_names <- c(
  "intercept", "change in T", "mean T",
  "T gradient", "biomass", "interaction (T)"#,
  # "mean T:gradient", "change in T:gradient", "interaction (T):gradient"
)

betas <- signif(as.list(model_grad$sdr, "Estimate")$b_j, digits = 3)/ sd(model_grad$y_i)
SE <- signif(as.list(model_grad$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_grad$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_grad$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_grad$y_i)
overall_betas_g <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_g$model <- "trend (T w gradient)"

## velocity model ####
coef_names <- shortener(unique(model_vel$coefs$coefficient))
coef_names <- c(
  "intercept", "change in T", "change in DO", "mean T", "mean DO",
  "biomass", "interaction (T)", "interaction (DO)"
)
betas <- signif(as.list(model_vel$sdr, "Estimate")$b_j, digits = 3)/ sd(model_vel$y_i)
SE <- signif(as.list(model_vel$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_vel$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_vel$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_vel$y_i)
overall_betas_vel <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel$model <- "velocity"

## temperature velocity model ####
coef_names <- shortener(unique(model_vel_t$coefs$coefficient))
coef_names <- c(
  "intercept", "change in T", "mean T",
  "biomass", "interaction (T)"
)
betas <- signif(as.list(model_vel_t$sdr, "Estimate")$b_j, digits = 3)/ sd(model_vel_t$y_i)
SE <- signif(as.list(model_vel_t$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_vel_t$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_vel_t$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_vel_t$y_i)
overall_betas_vel_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_t$model <- "velocity (T only)"

## DO velocity model ####
coef_names <- shortener(unique(model_vel_d$coefs$coefficient))
coef_names <- c(
  "intercept", "change in DO", "mean DO",
  "biomass", "interaction (DO)"
)
betas <- signif(as.list(model_vel_d$sdr, "Estimate")$b_j, digits = 3)/ sd(model_vel_d$y_i)
SE <- signif(as.list(model_vel_d$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_vel_d$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_vel_d$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_vel_d$y_i)
overall_betas_vel_d <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_d$model <- "velocity (DO only)"

## VEL model with age effect ####
# model_age <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-w-age-1-600.rds") # not converged
# version with 3-way interactions
coef_names <- shortener(unique(model_age$coefs$coefficient))
coef_names <- c(
  "intercept", "immature", "change in T", "mean T", "change in DO", "mean DO",
  "biomass", "interaction (T)", "interaction (DO)", "change in T:immature", "mean T:immature",
  "change in DO:immature", "mean DO:immature", "interaction (T):immature", "interaction (DO):immature"
)

betas <- signif(as.list(model_age$sdr, "Estimate")$b_j, digits = 3)/ sd(model_age$y_i)
SE <- signif(as.list(model_age$sdr, "Std. Error")$b_j, digits = 3)/ sd(model_age$y_i)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))/ sd(model_age$y_i)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)/ sd(model_age$y_i)
overall_betas_age <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_age$model <- "Maturity (3-way)"

# # version with only 2-way interactions
# 
# # model_age2 <- readRDS("analysis/VOCC/data/trend-all-95-newclim-more2016-06-22-trend-w-age2-1-500-DO.rds")
# coef_names <- shortener(unique(model_age2$coefs$coefficient))
# coef_names <- c(
#   "intercept", "immature", "change in T", "mean T", "change in DO", "mean DO",
#   "biomass", "interaction (T)", "interaction (DO)", "change in T:immature", "mean T:immature", "change in DO:immature", "mean DO:immature"
# )
# betas <- signif(as.list(model_age2$sdr, "Estimate")$b_j, digits = 3)
# SE <- signif(as.list(model_age2$sdr, "Std. Error")$b_j, digits = 3)
# lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
# upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
# overall_betas_age2 <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
# overall_betas_age2$model <- "maturity (2-way)"

### plot global coefs for multiple models ####

overall_betas$model_type <- "trend"
overall_betas_t$model_type <- "trend"
overall_betas_d$model_type <- "trend"
overall_betas_g$model_type <- "trend"
overall_betas_vel$model_type <- "velocity"
overall_betas_vel_t$model_type <- "velocity"
overall_betas_vel_d$model_type <- "velocity"
overall_betas_age$model_type <- "velocity"
# overall_betas_age2$model_type <- "trend"

custom_order <- c(
  "intercept", "immature", "biomass",
  "T gradient",
  "mean T", "mean T:immature", "mean T:gradient",
  "change in T", "change in T:immature", "change in T:gradient",
  "interaction (T)", "interaction (T):immature", "interaction (T):gradient",
  "mean DO", "mean DO:immature",
  "change in DO", "change in DO:immature",
  "interaction (DO)", "interaction (DO):immature"
)

# allcoefs2 <- allcoefs %>% rename(term = coefficient, estimate = Estimate, std.error = Std..Error) %>% filter(term != "intercept")

### compare trends and velocities
overall <- rbind.data.frame(
  overall_betas_vel, 
  overall_betas_vel_t, overall_betas_vel_d,  
  # overall_betas_g,
  # overall_betas_t, 
  # overall_betas_d,
  overall_betas
)
overall <- mutate(overall, term = firstup(as.character(coef_names)))
overall2 <- overall %>% rename(
  estimate = betas, std.error = SE
) # %>% filter(term != "intercept")
overall2 <- overall2 %>% mutate(
  term = factor(term,
    levels = firstup(as.character(custom_order))
  ),
  model = firstup(as.character(model))
)

global_vel <- dotwhisker::dwplot(overall2) + # xlim(-10,10) +
  geom_vline(xintercept = 0, colour = "darkgray", alpha = 0.5) +
  # geom_point(aes(term, estimate,  colour = model),
  #    alpha= 0.1, position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) +
  scale_colour_manual(
    name = "Model",
    values = c(
      "#D53E4F",
      "#F46D43",
      "#FDAE61",
      # "#FEE08B",
      # "#ABDDA4",
      # "#3288BD",
      "#5E4FA2"
    )
  ) +   
  coord_cartesian(xlim = c(-0.4, 0.35)) +
  # coord_cartesian(xlim = c(-2, 1.2)) +
  ggtitle("a. Velocity versus trend models") +
  gfplot::theme_pbs() + theme(
    # plot.margin = margin(0.1, 0, 0, 0, "cm"),
    axis.title = element_blank(),
    # legend.title = element_blank(),
    legend.position = c(0.24, 0.85)
  )
# global_vel + grid::textGrob("Standardized coefficient estimate with 95% CI", 
#   just = 0.35, gp = grid::gpar(fontsize = 11))+ plot_layout(height = c(10, 0.02))
# 
# # ggsave(here::here("ms", "figs", "supp-global-coefs-vel-grad.pdf"), width = 5, height = 6)
# ggsave(here::here("ms", "figs", "supp-global-coefs-vel.pdf"), width = 5, height = 6)

# look for sig age effects
overall3 <- rbind.data.frame(overall_betas_vel, 
  # overall_betas_age2, 
  overall_betas_age)
overall3 <- overall3 %>%
  mutate(term = firstup(as.character(coef_names))) %>%
  rename(
    estimate = betas, std.error = SE
  ) %>%
  mutate(
    term = factor(term,
      levels = firstup(as.character(custom_order))
    ),
    model = firstup(as.character(model))
  )

global_age <- dotwhisker::dwplot(overall3) + # xlim(-10,10) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  scale_colour_manual(
    name = "Model",
    values = c(
      "#D53E4F", #"#F46D43",
      "#FDAE61" 
      # "#FEE08B" #, "#3288BD", 
      # "#5E4FA2"
    )
  ) + 
  coord_cartesian(xlim = c(-0.2, 0.2)) +
  ggtitle("b. Global maturity effects") +
  scale_y_discrete(position = "right") +
  gfplot::theme_pbs() + theme(
    # plot.margin = margin(0.1, 0, 0, 0, "cm"),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.9)
  )
global_age
# ggsave(here::here("ms", "figs", "supp-global-coefs-w-age.pdf"), width = 5, height = 4)

(global_vel | global_age) / grid::textGrob("Standardized coefficient estimate with 95% CI", 
  just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

# ggsave(here::here("ms", "figs", "supp-global-coefs-all-trend.pdf"), width = 9, height = 6)
ggsave(here::here("ms", "figs", "supp-global-coefs.pdf"), width = 9, height = 5)

#########################
#### experiment with species level coefs as boxplots ####
# trendcoefs <- add_colours(model$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c(
#     "(Intercept)", "temp_trend_scaled", "mean_temp_scaled",
#     "DO_trend_scaled", "mean_DO_scaled",
#     "log_biomass_scaled",
#     "temp_trend_scaled:mean_temp_scaled", "DO_trend_scaled:mean_DO_scaled"
#   ),
#   labels = c(
#     "intercept", "change in T", "mean T", "change in DO", "mean DO",
#     "biomass", "interaction (T)", "interaction (DO)"
#   )
# ))
# 
# velcoefs1 <- add_colours(model_vel_t$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c(
#     "(Intercept)", "squashed_temp_vel_scaled", "mean_temp_scaled",
#     "log_biomass_scaled", "squashed_temp_vel_scaled:mean_temp_scaled"
#   ),
#   labels = c(
#     "intercept", "change in T", "mean T",
#     "biomass", "interaction (T)"
#   )
# ))
# 
# velcoefs2 <- add_colours(model_vel_d$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c(
#     "(Intercept)", "squashed_DO_vel_scaled", "mean_DO_scaled",
#     "log_biomass_scaled", "squashed_DO_vel_scaled:mean_DO_scaled"
#   ),
#   labels = c(
#     "intercept", "change in DO", "mean DO",
#     "biomass", "interaction (DO)"
#   )
# ))
# 
# trendcoefs$model <- "trend"
# velcoefs1$model <- "velocity (T only)"
# velcoefs2$model <- "velocity (DO only)"
# trendcoefs$model_type <- "trend"
# velcoefs1$model_type <- "velocity"
# velcoefs2$model_type <- "velocity"
# 
# allcoefs <- rbind.data.frame(trendcoefs, velcoefs1, velcoefs2)
# head(allcoefs)
# 
# filter(overall, coef_names != "intercept") %>%
#   # overall %>%
#   ggplot(aes(coef_names, betas)) + # forcats::fct_reorder(coef_names, ) #, colour = model
#   geom_point(aes(forcats::fct_reorder(coefficient, Estimate), Estimate, colour = age), alpha = 0.3, position = position_jitter(width = 0.15), inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")) +
#   geom_boxplot(aes(forcats::fct_reorder(coefficient, Estimate), Estimate, colour = age),
#     notch = T,
#     inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")
#   ) +
#   geom_pointrange(aes(ymin = lowerCI, ymax = upperCI), size = 1, fatten = 3, alpha = .9) +
#   # position = position_jitter(width = 0.2), shape = "|"
#   geom_hline(yintercept = 0, colour = "darkgray") +
#   scale_colour_viridis_d(begin = .8, end = .2) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   # scale_colour_manual(values = c("#D53E4F", "#5E4FA2", "#3288BD")) + #
#   xlab("") +
#   facet_wrap(~model_type) +
#   coord_flip(ylim = c(-2, 2)) + # ylim = c(-10,10)
#   gfplot::theme_pbs()
# 

#########################
#########################
#########################
#### ALL CHOPSTICKS AND SLOPE WORM PLOTS
###
### ALL TREND CHOPS ####
# if model controling for fishing effort desired...
# model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-05-19-trend-w-fishing-1-500.rds")
model <- model_trend
temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
) %>%
  mutate(sort_var = -(all_global_slope))
  # mutate(sort_var = -(diff))
  # mutate(sort_var = -slope_est)

do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)%>%
  mutate(sort_var = -(all_global_slope))
  # mutate(sort_var = -(diff))
  # mutate(sort_var = -slope_est)

temp_slopes <- left_join(temp_slopes, stats)
do_slopes <- left_join(do_slopes, stats)

(p_temp_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass",
  # order_by_chops = NULL,
  slopes = temp_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim = c(-4.5, 6)) +
  xlab("Temperature trend (scaled)") + theme(legend.position = "none"))

p_do_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = "Predicted % change in biomass",
  # order_by_chops = NULL,
  slopes = do_slopes
) + coord_cartesian(ylim = c(-4, 5)) +
  xlab("DO trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

(p_temp_all_slopes <- plot_chopstick_slopes(temp_slopes,
  type = "temp", add_global = F, point_size = 1, alpha_range = c(0.4, 0.99),
  # order_by_chops = c("high"),
  legend_position = "none"
  # legend_position = c(.25, .95)
) +
  coord_flip(ylim = c(-4.5, 3)) + 
  ylab(" "))

p_do_all_slopes <- plot_chopstick_slopes(do_slopes,
  type = "DO", add_global = F, point_size = 1, alpha_range = c(0.4, 0.99),
  # order_by_chops = c("low"),
  legend_position = "none"
  # legend_position = c(.25, .95)
) +
  coord_flip(ylim = c(-2, 3)) + # coord_flip(ylim =c(-3,1)) +
  ylab("slopes")

cowplot::plot_grid(p_temp_all_slopes, p_temp_chops, p_do_all_slopes, p_do_chops,
  # labels = c("a.", "b.", "c.", "d."), label_size = 12,
  labels = c("a.", "", "b.", ""), label_size = 11,
  ncol = 2, rel_widths = c(1, 2.5)
)

ggsave(here::here("ms", "figs", "supp-trend-chopsticks-ordered.pdf"), width = 14, height = 10)
## ggsave(here::here("ms", "figs", "supp-trend-chopsticks-fishing.pdf"), width = 14, height = 11)

# 
# ### if just temp model... no change in chopsticks ####
# temp_slopes <- chopstick_slopes(model_temp,
#   x_variable = "temp_trend_scaled",
#   interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
# )
# temp_slopes <- left_join(temp_slopes, stats)
# p2 <- plot_fuzzy_chopsticks(model_temp,
#   x_variable = "temp_trend_scaled", type = "temp",
#   y_label = "Predicted % change in biomass",
#   # choose_age = "mature",
#   # order_var = "species",
#   slopes = temp_slopes
# ) + coord_cartesian(ylim = c(-11, 7)) +
#   xlab("Temperature trend (scaled)") + theme(legend.position = "none")
# 
# temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
# p1 <- plot_chopstick_slopes(temp_slopes,
#   type = "temp",
#   legend_position = c(.25, .95)
# ) +
#   ylab("Slopes")
# 
# cowplot::plot_grid(p1, p2, rel_widths = c(1, 2.5))
# ggsave(here::here("ms", "figs", "supp-trend-chopsticks-temp-only.pdf"), width = 14, height = 5.5)

### if just temp with gradient model... no change in chopsticks ####
temp_slopes <- chopstick_slopes(model_grad,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)%>%
  mutate(sort_var = -(all_global_slope))

temp_slopes <- left_join(temp_slopes, stats)
p2 <- plot_fuzzy_chopsticks(model_grad,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass",
  slopes = temp_slopes
) + coord_cartesian(ylim = c(-11, 4)) +
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
p1 <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = "none" #c(.25, .75)
) +
  ylab("Slopes")

cowplot::plot_grid(p1, p2, rel_widths = c(1, 2.5))
ggsave(here::here("ms", "figs", "supp-trend-chopsticks-temp-grad.pdf"), width = 14, height = 5.5)

### ALL VELOCITY CHOPS ####


temp_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp"
) %>%
  mutate(sort_var = (diff)) 
  # mutate(sort_var = -(diff)) # orders by high - low slopes
  # mutate(sort_var = slope_est)

do_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
) %>%
  mutate(sort_var = (diff)) 
  # mutate(sort_var = -(diff))
  # mutate(sort_var = slope_est)

temp_vel_slopes <- left_join(temp_vel_slopes, stats)
do_vel_slopes <- left_join(do_vel_slopes, stats)


(p_temp_vel_chops <- plot_fuzzy_chopsticks(model_vel,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  # x_variable = "squashed_temp_dvocc_scaled", type = "temp",
  y_label = "Predicted mature biomass vel",
  # order_by_chops = NULL,
  slopes = temp_vel_slopes # if add, the global slope can be included for insig
) + coord_cartesian(xlim = c(-0.25, 4), ylim = c(-30, 37)) +
  xlab("Temperature velocity (scaled)") + theme(legend.position = "none")
)

p_do_vel_chops <- plot_fuzzy_chopsticks(model_vel,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  # x_variable = "squashed_DO_dvocc_scaled", type = "DO",
  y_label = "Predicted mature biomass vel",
  # order_by_chops = NULL,
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim = c(-30, 37)) +
  xlab("DO velocity (scaled)") + theme(legend.position = "none")

temp_vel_slopes$species[temp_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_vel_slopes$species[do_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_all_vel_slopes <- plot_chopstick_slopes(temp_vel_slopes,
  type = "temp", add_global = F, point_size = 1, alpha_range = c(0.3, 0.99),
  legend_position = "none"
  # legend_position = c(.7, .93)
) + 
  ylab(" ") +
  coord_flip(ylim =c(-6,11))
  # coord_flip()

p_do_all_vel_slopes <- plot_chopstick_slopes(do_vel_slopes,
  type = "DO", add_global = F, point_size = 1, alpha_range = c(0.3, 0.99),
  legend_position = "none"
  # legend_position = c(.25, .08)
) +
  ylab("slopes") +
  # coord_flip(ylim =c(-3,3.5))
  coord_flip()

cowplot::plot_grid(p_temp_all_vel_slopes, p_temp_vel_chops, p_do_all_vel_slopes, p_do_vel_chops, 
  labels = c("a.", "", "b.", ""), label_size = 11,
  ncol = 2, rel_widths = c(1, 2.5)
)

# layout <- "
#       ABBBB
#       CDDDD
#       "
# p_temp_all_vel_slopes + p_temp_vel_chops + p_do_all_vel_slopes + p_do_vel_chops + 
#   plot_layout(design = layout, widths = c(1, 2.5), guides = 'collect')& theme(
#     legend.text = element_text(size = 9),
#     legend.position = "bottom",
#     # legend.justification='left',
#     legend.direction = "horizontal",
#     legend.box = "horizontal",
#     legend.margin=margin(t = 0.1, l= 1, r = 0, unit='cm'),
#     # legend.margin = unit(1.5, "cm"),
#     legend.spacing.x = unit(.1, "cm")
#   )


ggsave(here::here("ms", "figs", "supp-vel-chopsticks-ordered.pdf"), width = 14, height = 10)
## ggsave(here::here("ms", "figs", "supp-vel-chopsticks-catch-family-350.pdf"), width = 14, height = 10)
## ggsave(here::here("ms", "figs", "supp-vel-chopsticks-catch-400.pdf"), width = 14, height = 10)
## ggsave(here::here("ms", "figs", "supp-vel-chopsticks-w-order.pdf"), width = 14, height = 10)
## ggsave(here::here("ms", "figs", "supp-dvocc-chopsticks-ordered.pdf"), width = 14, height = 10)


#########################
#########################
#########################
#### BIOTIC MAPS ####

# age <- "immature"
# d2 <- readRDS(here::here(paste0("analysis/vocc/data/", age, "-optimized-vocc.rds"))) %>% mutate(age_class = "immature") %>% mutate(species_only = species, age = age_class)
# 
# model$data <- d2

biotic_maps <- function(model, 
trends = T,
legend_position =  c(0.02, 0.85),  
maturity = "mature"  
){
  
dat_all <- model$data #%>% filter(species != "mature Shortbelly Rockfish")
dat_all$squashed_biotic_trend <- collapse_outliers(dat_all$biotic_trend, c(0.05, 0.95))
dat <- dat_all %>%  filter(age_class == maturity)  

dat$species_only[dat$species_only == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

if(trends) {
biotic_maps <- plot_vocc(dat,
  fill_col = "squashed_biotic_trend", 
  fill_label = "%", 
  raster_limits = c(min(dat_all$squashed_biotic_trend), max(dat_all$squashed_biotic_trend)),
  vec_aes = NULL,
  raster_cell_size = 4,
  na_colour = "darkcyan", white_zero = TRUE,
  high_fill = "darkcyan",
  mid_fill = "lightcyan1", grey_water = F,
  low_fill = "Red 3", 
  axis_lables = F,
  legend_position = legend_position,
  make_square = F
) 
change_var <- "trends"
} else {
  biotic_maps <- plot_vocc(dat,
  fill_col = "squashed_biotic_vel", 
  fill_label = "km", 
  raster_limits = c(min(dat_all$squashed_biotic_vel), max(dat_all$squashed_biotic_vel)),
  vec_aes = NULL,
  raster_cell_size = 4,
  na_colour = "red 3", white_zero = TRUE,
  high_fill = "darkcyan",
  mid_fill = "lightcyan1", grey_water = F,
  low_fill = "Red 3", 
  axis_lables = T,
  legend_position = legend_position,
  make_square = F
) 
  change_var <- "velocities"
    
}

biotic_maps + facet_wrap(~species_only, ncol = 9) +
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(vjust = 1),
    plot.margin = margin(1, 0, 0, 0, "cm"),
    legend.title = element_text(size= 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) #+ ggtitle(paste("  Biotic", change_var, "for", maturity, "groundfish species"))
}

biotic_maps(model_vel, legend_position =  c(0.02, 0.88))
ggsave(here::here("ms", "figs", "supp-biotic-maps-mature.png"), width = 15, height = 11)

biotic_maps(model_vel, maturity = "immature")
ggsave(here::here("ms", "figs", "supp-biotic-maps-immature.png"), width = 15, height = 10)

biotic_maps(model_vel, trends = F, legend_position =  c(0.02, 0.88))
ggsave(here::here("ms", "figs", "supp-biotic-vel-maps-mature.png"), width = 15, height = 11)

biotic_maps(model_vel, trends = F, maturity = "immature")
ggsave(here::here("ms", "figs", "supp-biotic-vel-maps-immature.png"), width = 15, height = 10)

#########################
#### SPATIAL RESIDUALS
###
# ### for trend model ####
# data <- model$data %>%
#   mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
#   mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
#   mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
#   mutate(residual = if_else(residual < resid_lower, resid_lower, residual))
# 
# data$species_only[data$species_only == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"
# 
# data %>%
#   filter(age == 0) %>%
#   ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(
#     # trans = fourth_root_power, #breaks=scales::extended_breaks(n = 9),
#     # mid = "lavenderblush1"
#     mid = "aliceblue") + gfplot::theme_pbs() +
#   theme(
#     legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
#     axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
#   ) +
#   facet_wrap(~species_only, strip.position = "bottom") +
#   ggtitle("Mature fish biomass trend residuals")
# ggsave(here::here("ms", "figs", "supp-mat-residuals.pdf"), width = 7, height = 7)
# 
# data %>%
#   filter(age == 1) %>%
#   ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(# trans = fourth_root_power, #breaks=scales::extended_breaks(n = 9),
#     # mid = "lavenderblush1"
#     mid = "aliceblue") + gfplot::theme_pbs() +
#   theme(
#     legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
#     axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
#   ) +
#   facet_wrap(~species_only, strip.position = "bottom") +
#   ggtitle("Immature fish biomass trend residuals")
# ggsave(here::here("ms", "figs", "supp-imm-residuals.pdf"), width = 7, height = 6)


### for velocity model ####
data2 <- model_vel$data %>%
  # mutate(resid_upper = quantile(model$data$residual, probs = 0.99999)) %>% # compress tails
  # mutate(resid_lower = quantile(model$data$residual, probs = 0.00001)) %>% # compress tails
  mutate(resid_upper = 45) %>% # compress tails
  mutate(resid_lower = -45) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual))


data2$species_only[data2$species_only == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"

data2 %>%
  filter(age_class == "mature") %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(
    # trans = fourth_root_power, #breaks=scales::extended_breaks(n = 9),
    # mid = "lavenderblush1"
    mid = "aliceblue"
  ) + gfplot::theme_pbs() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 5),
    panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  facet_wrap(~species_only, nrow = 7, strip.position = "bottom") +
  ggtitle("Mature fish biomass velocity residuals")
ggsave(here::here("ms", "figs", "supp-mat-vel-residuals.pdf"), width = 6, height = 8)

data2 %>%
  filter(age_class == "immature") %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(  
    # trans = fourth_root_power, #breaks=scales::extended_breaks(n = 9),
    # mid = "lavenderblush1"
    mid = "aliceblue"
      ) + gfplot::theme_pbs() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 5),
    panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  facet_wrap(~species_only, nrow = 6, strip.position = "bottom") +
  ggtitle("Immature fish biomass velocity residuals")
ggsave(here::here("ms", "figs", "supp-imm-vel-residuals.pdf"), width = 6, height = 7)

#########################
#########################
#########################
#### EXPLORE GRADIENTS ####
alldata <- readRDS(paste0("analysis/VOCC/data/all-newclim-untrimmed-dvocc-med.rds"))
### Gradient maps ####
(grad_do <- plot_vocc(alldata,
  vec_aes = NULL, grey_water = F,
  fill_col = "DO_grad", fill_label = "ml/L per km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F,
  transform_col = fourth_root_power,
  legend_position = c(0.175, 0.25)
) + ggtitle("Dissolved oxygen") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0.2, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))

(grad_temp <- plot_vocc(alldata,
  vec_aes = NULL, grey_water = F,
  fill_col = "temp_grad", fill_label = "ºC per km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "C",
  axis_lables = T,
  transform_col = fourth_root_power,
  legend_position = c(0.175, 0.25)
) + ggtitle("Temperature") +
    ylab("Climate gradient") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0.2, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    ))

(grad_rockfish <- plot_vocc(filter(
  model_vel$data,
  # species == "mature Widow Rockfish"
  species == "mature Redbanded Rockfish"
  # species == "mature Canary Rockfish"
  # species == "mature Shortspine Thornyhead"
),
  vec_aes = NULL, grey_water = F,
  fill_col = "biotic_grad", fill_label = "% biomass \nper km",
  raster_cell_size = 4, na_colour = "yellow", white_zero = F,
  viridis_option = "B",
  axis_lables = T,
  raster_limits = c(0,2.75),
  transform_col = fourth_root_power,
  legend_position = c(0.175, 0.25)
) + ggtitle("Redbanded Rockfish (mature)") +
    # ggtitle("Widow Rockfish") +
    # ggtitle("Canary Rockfish") +
    # ggtitle("Shortspine Thornyhead") +
    ylab("Biotic gradient") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    ))

(grad_flatfish <- plot_vocc(filter(
  model_vel$data, 
  # species == "mature Rex Sole"  
  # species == "mature Arrowtooth Flounder"  
  # species == "mature Slender Sole"
  species == "immature Lingcod"
  # species == "mature Flathead Sole"
  # species == "mature Dover Sole"
),
  vec_aes = NULL, grey_water = F,
  fill_col = "biotic_grad", fill_label = "% biomass \nper km",
  raster_cell_size = 4, na_colour = "black", white_zero = F,
  viridis_option = "B",
  axis_lables = T,
  raster_limits = c(0,2.75),
  transform_col = fourth_root_power,
  legend_position = c(0.175, 0.25)
) +
    # ggtitle("Rex Sole") +
    # ggtitle("Flathead Sole") +
    
    ggtitle("Lingcod (immature)") +
    # ggtitle("Dover Sole") +
    # # ylab("gradient") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))

# grad_sable <- plot_vocc(filter(model$data,
#   species == "mature Sablefish"),
#   vec_aes = NULL,
#   fill_col = "biotic_grad", fill_label = "% biomass \nper km",
#   raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
#   viridis_option = "B",
#   axis_lables = T,
#   transform_col = fourth_root_power,
#   legend_position = c(0.15, 0.25)
# ) +
#   ggtitle("Sablefish") +
#   # # ylab("gradient") +
#   coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
#   theme(
#     plot.margin = margin(0, 0, 0, 0, "cm"),
#     axis.text = element_blank(), axis.ticks = element_blank(),
#     axis.title.x = element_blank(), axis.title.y = element_blank()
#   )
grad_temp + grad_do + grad_rockfish + grad_flatfish + plot_layout(ncol = 2)+ plot_annotation(tag_levels = 'a', tag_suffix = ".")& 
  theme(plot.tag.position = c(.9, .85),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0))
ggsave(here::here("ms", "figs", "supp-gradient-maps-lingcod.png"), width = 6.5, height = 7)

### Scatterplots of coorelation btw biotic & temperature gradients ####
model$data$family <-  stringr::str_to_title(model$data$family)
(ggplot(filter(model$data, age_class == "mature"), aes(temp_grad, biotic_grad)) + 
    geom_point(alpha = 0.05) +
    geom_smooth(method = "lm", alpha = 0.3, colour = "grey40") +
    # facet_grid(species~rockfish) +
    # facet_wrap(~true_genus*species_only) +
    # facet_wrap(~true_genus) +
    facet_wrap(~family) +
    coord_cartesian(xlim = c(0,0.3), ylim = c(0,1)) +
    # scale_x_continuous(trans = fourth_root_power) +
    # scale_y_continuous(trans = fourth_root_power) +
    xlab("Temperature gradients between each cell and its neighbours") +
    ylab("Biotic gradients of mature populations") +
    gfplot::theme_pbs())

ggsave(here::here("ms", "figs", "supp-gradient-cor-family.png"), width = 6, height = 6)

# ggplot(filter(model$data, age == "mature" & genus_id == 7), aes(temp_grad, biotic_grad)) + geom_point(alpha=.1) +
#   geom_smooth(method = "lm", alpha= 0.5, colour = "darkgray") +
#   # facet_grid(species~rockfish) +
#   facet_wrap(~forcats::fct_reorder(species_only, genus_id)) +
#   # coord_cartesian(xlim = c(0,0.3), ylim = c(0,1)) +
#   # scale_x_continuous(trans = fourth_root_power) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   gfplot::theme_pbs()
#
# ggplot(filter(model$data, age == "mature" & genus_id == 5), aes(temp_grad, biotic_grad)) + geom_point(alpha=.1) +
#   geom_smooth(method = "lm", alpha= 0.5, colour = "darkgray") +
#   # facet_grid(species~rockfish) +
#   facet_wrap(~species_only) +
#   # coord_cartesian(xlim = c(0,0.3), ylim = c(0,1)) +
#   # scale_x_continuous(trans = fourth_root_power) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   gfplot::theme_pbs()


#########################

species_map <- function(species, 
  immature = F,
  start_year = 2008,
  ssids = c(1,3),
  max_raster = max(d$est_exp),
  # biotic_lim = c(-20, 25), # currently only applied to
  # biotic_lim = c(-40, 40), # currently only applied to 
  alpha_range = c(0.9, 0.9)) {
  # age <- unique(data[data$species == species, ]$age_class)
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  
  if (immature) {
    age <- "immature"
    d <- readRDS(paste0(
      "analysis/VOCC/data/", spp,
      "/predictions-", spp, covs, "-1n3n4n16-imm-biomass.rds" 
    )) %>% 
      # to convert from kg/m2 to kg/hectare multiply by 10000
      mutate(est_exp = exp(est) * 10000) %>%
      filter(year >= start_year & ssid %in% ssids)
  } else {
    age <- "mature"
    try({
      total_d <- readRDS(paste0(
        "analysis/VOCC/data/", spp,
        "/predictions-", spp, covs, "-1n3n4n16-total-biomass.rds"
      )) %>% 
        # to convert from kg/m2 to kg/hectare multiply by 10000
        mutate(est_exp = exp(est) * 10000) %>%
        filter(year >= start_year & ssid %in% ssids)
    })
    try({
      d <- readRDS(paste0(
        "analysis/VOCC/data/", spp,
        "/predictions-", spp, covs, "-1n3n4n16-mature-biomass.rds"
      )) %>% 
        # to convert from kg/m2 to kg/hectare multiply by 10000
        mutate(est_exp = exp(est) * 10000) %>%
        filter(year >= start_year & ssid %in% ssids)
    })
  }
 
 if (exists("d")) {
   d$est_exp <- exp(d$est) * 10000
   # max_bio <- signif(max(d$est_exp), digits = 2)
   p <- plot_facet_map(d, "est_exp",
     raster_limits = c(0, max_raster),
     legend_position = "none",
     transform_col = fourth_root_power
   ) +
     labs(fill = "kg/ha") +
     ggtitle(paste0("", species, " ", age, " biomass \n(max = ", signif(max_raster, digits = 2), " kg/ha)"))
   
 } else {
   
   # max_bio <- signif(max(total_d$est_exp), digits = 2)
   p <- plot_facet_map(total_d, "est_exp",
     raster_limits = c(0, max(total_d$est_exp)),
     transform_col = fourth_root_power
   ) +
     labs(fill = "kg/ha") +
     ggtitle(paste0("", species, " total biomass \n(max = ", signif(max(total_d$est_exp), digits = 2), " kg/ha)"))
 }
 p <- p + theme(strip.text.x = element_blank())
 p
}

species_map("Redbanded Rockfish", start_year = 2008, max_raster = 20)
ggsave(here::here("ms", "figs", "supp-redbanded-biomass-per-year-1n3.png"), width = 6, height = 5)

species_map("Redbanded Rockfish", start_year = 2008, ssids = c(4))
ggsave(here::here("ms", "figs", "supp-redbanded-biomass-per-year-4.png"), width = 6, height = 5)
# species_map("Lingcod")
# ggsave(here::here("ms", "figs", "supp-lingcod-biomass-per-year-1n3.png"), width = 6, height = 5)
# 
species_map("Lingcod", start_year = 2008, ssids = c(4), max_raster = 14)
# ggsave(here::here("ms", "figs", "supp-lingcod-biomass-per-year-4.png"), width = 6, height = 5)


#################################
##### Time-varying depth for biomass ####
#################################
species_depth_profile <- function(species, immature = F, new_model = T){
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  covs <- "-tv-depth-only"
  try({rm(p)})
  try({rm(m)})
  # browser()
  if(new_model){
    
  if(immature){
    m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp,
      "/mod-imm-biomass-", spp, covs, "-1n3n4n16.rds"
    )))
    m <- sdmTMB:::update_model(m)
  } else { 
  m <- readRDS(here::here(paste0("analysis/VOCC/data/", spp,
    "/mod-mat-biomass-", spp, covs, "-1n3n4n16-new.rds"
  )))
  }
  pd <- expand.grid(
    depth_scaled = seq(min(m$data$depth_scaled),
    max(m$data$depth_scaled), length.out = 100),
    year = unique(m$data$year)
  )
  pd$depth_scaled2 <- pd$depth_scaled^2
  if(immature){
    try({p <- readRDS(here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-imm.rds"))) })
  }else{
    try({p <- readRDS(here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-new.rds"))) })
  }
  
if(!exists("p")){
  p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
  saveRDS(p, here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-temp.rds")))
  p <- p %>% mutate(
    Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
    Biomass = exp(est)* 10000,
    lowCI = exp(est - 1.96 * est_se)* 10000,
    highCI = exp(est + 1.96 * est_se)* 10000)
  
  if(immature){
    saveRDS(p, here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-imm.rds")))
  }else{
    saveRDS(p, here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-new.rds")))
  }
  }

ggplot(filter(p, Depth >15 & year >2007), 
  aes(Depth, Biomass,
  ymin = lowCI, 
  ymax = highCI, 
  group = as.factor(year), fill=year, colour = year)) +
  geom_line(size = 0.5, alpha =0.85) +
  # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
  # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
  scale_fill_viridis_c(option = "C") +
  scale_colour_viridis_c(option = "C") +
  # ylab("DO") +
  geom_ribbon(alpha = 0.1, colour = NA) +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  coord_cartesian(xlim = c(20, 700), ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs() 

  } else {
    m <- readRDS(paste0("analysis/VOCC/data/", spp,
      "/mod-mat-biomass-", spp, covs, "-1n3n4n16-prior-FALSE.rds"
    ))
    
    m <- sdmTMB:::update_model(m)
    
    pd <- expand.grid(
      depth_scaled = seq(min(m$data$depth_scaled),
        max(m$data$depth_scaled), length.out = 100),
      year = unique(m$data$year)
    )
    pd$depth_scaled2 <- pd$depth_scaled^2
    
    try({p <- readRDS(here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-old.rds"))) })
    
    if(!exists("p")){
      p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA, xy_cols = c("X", "Y"))
      saveRDS(p, here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-old.rds")))
      p <- p %>% mutate(
        Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
        Biomass = exp(est)* 10000)
      saveRDS(p, here::here(paste0("analysis/VOCC/data/depth-tv-", spp, "-old.rds")))
    }
    
    ggplot(filter(p, Depth >15 & year >2007), 
      aes(Depth, Biomass,
        group = as.factor(year), fill=year, colour = year)) +
      geom_line(size = 0.5, alpha =0.85) +
      # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
      # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
      scale_fill_viridis_c(option = "C") +
      scale_colour_viridis_c(option = "C") +
      coord_cartesian(xlim = c(20, 700), ylim=c(0, quantile(p$Biomass, 1))) +
      gfplot::theme_pbs() 
  }
}

species_depth_profile("Redbanded Rockfish") + theme(legend.position = c(.2,.6))
ggsave(here::here("ms", "figs", paste0("supp-tv-depth-", "redbanded-rockfish", "-100.png")), width = 4, height = 2.5)


(dp <- species_depth_profile("Redbanded Rockfish", immature = T) + theme(legend.position = c(.2,.6)))

(dp <- species_depth_profile("Lingcod", new_model = F) + theme(legend.position = c(.8,.6)))

sdggsave(here::here("ms", "figs", paste0("supp-tv-depth-", "lingcod", "-100.png")), width = 4, height = 2.5)

(dp <- species_depth_profile("Canary Rockfish", new_model = T) + 
  theme(legend.position = c(.8,.6)))
ggsave(here::here("ms", "figs", paste0("supp-tv-depth-", "canary-rockfish", "-new.png")), width = 4, height = 2.5)

(dp2 <- species_depth_profile("English Sole", new_model = F) + 
  theme(legend.position = c(.8,.6)))
ggsave(here::here("ms", "figs", paste0("supp-tv-depth-", "english-sole", "-old.png")), width = 4, height = 2.5)

(dp3 <- species_depth_profile("English Sole", new_model = T) + 
    theme(legend.position = c(.8,.6)))
ggsave(here::here("ms", "figs", paste0("supp-tv-depth-", "english-sole", "-new.png")), width = 4, height = 2.5)


#### MEAN AGE AND GROWTH RATE ####
chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
) %>% left_join(temp_slopes, stats) %>% filter(age == "Immature") %>%
  ggplot(aes(age_mean, y=(length_50_mat_f / age_mat + 1))) +
  geom_smooth(method = "lm", colour = "black", size =0.5) +
  geom_line(aes(group = species), colour = "grey60") +
  geom_point(aes(shape = age, colour = age), fill = "white", size = 2) +
  scale_colour_manual(values = c("deepskyblue3", "royalblue4")) +
  # scale_fill_manual(values = c("cornflowerblue", "deepskyblue")) +
  # scale_colour_manual(values = c("royalblue4", "darkorchid4")) +
  # scale_fill_manual(values = c("royalblue4", "darkorchid4")) +
  scale_shape_manual(values = c(21, 19)) +
  ylim(0,20) +
  scale_x_continuous(position = "top") +
  # scale_x_log10() +
  scale_y_log10(position = "right") +
  # ylab("Depth range (IQR)") +
  # xlab("Mean depth") +
  gfplot::theme_pbs() + theme(
    
    # legend.position = element_blank(),
    legend.title = element_blank()
  )

ggsave(here::here("ms", "figs", "supp-age-growth.pdf"), width = 5, height = 3.5)

alldata <- readRDS(paste0("analysis/VOCC/data/all-newclim-untrimmed-dvocc-med.rds"))

(depth <- plot_vocc(alldata, # grey_water = T,
  vec_aes = NULL, grey_water = F,
  fill_col = "depth", fill_label = "m",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F, tag_text = "b.",
  viridis_begin = 0,
  viridis_end = .5,
  viridis_dir = -1,
  transform_col = fourth_root_power,
  # raster_limits = c(10, 1300),
  legend_position = c(0.15, 0.3)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))
ggsave(here::here("ms", "figs", "depth-map.pdf"), width = 5, height = 5)
