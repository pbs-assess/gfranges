#### SUPPLEMENTARY FIGURES
# # if make-figs not just run
# setwd(here::here())
# library(TMB)
# library(tidyverse)
# library(patchwork)
# library(gfranges)
# library(dotwhisker)
#### load appropriate final models and other data
# model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")
# model_vel_t <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
# model_vel_d <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")
# 
# stats <- readRDS(paste0("analysis/VOCC/data/life-history-stats.rds"))
# stats$rockfish <- if_else(stats$group == "ROCKFISH", "rockfish", "other fishes")
# stats <- stats %>% separate(species_science_name, " ", into = c("genus", "specific"))
# stats$group[stats$group == "SHARK"] <- "DOGFISH"
# stats$group[stats$group == "HAKE"] <- "COD"
# imm <- mutate(stats, age = "immature") %>%
#   mutate(depth = depth_imm, age_mean = age_imm) %>%
#   select(-depth_imm, -age_imm)
# mat <- mutate(stats, age = "mature") %>% select(-depth_imm, -age_imm)
# stats <- rbind(mat, imm)
# stats$family <- gsub("\\(.*", "", stats$parent_taxonomic_unit)
#
# alldata <- readRDS(paste0("analysis/VOCC/data/all-do-with-null-1-untrimmed-allvars.rds"))
#
# model2 <- add_colours(model$coefs, species_data = stats)
# model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
# model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
# model2 <- model2 %>% group_by(group) %>% mutate(spp_count = length(unique(species))) %>% ungroup()
# model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc=F))
# model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc=F))
# trendeffects <- model2 %>% filter(coefficient %in% c("temp_trend_scaled","DO_trend_scaled")) %>% 
#   transform(coefficient = factor(coefficient, levels = c("temp_trend_scaled","DO_trend_scaled"), labels = c("temperature", "DO")))
# trendeffects <- trendeffects %>% mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc=F))
#########################
#### EXPLORE GRADIENTS ####
### Gradient maps ####
grad_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "DO_grad", fill_label = "ml/L per km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F,
  transform_col = fourth_root_power,
  legend_position = c(0.15, 0.25)
) + ggtitle("dissolved oxygen") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0.2, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

grad_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "temp_grad", fill_label = "ºC per km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "C",
  axis_lables = T,
  transform_col = fourth_root_power,
  legend_position = c(0.15, 0.25)
) + ggtitle("temperature") +
  ylab("climate gradient") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0.2, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

grad_rockfish <- plot_vocc(filter(model$data, 
  # species == "mature Widow Rockfish"),
  # species == "mature Canary Rockfish"),
  species == "mature Shortspine Thornyhead"),
  vec_aes = NULL,
  fill_col = "biotic_grad", fill_label = "% biomass \nper km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "B",
  axis_lables = T,
  transform_col = fourth_root_power,
  legend_position = c(0.15, 0.25)
) + 
  # ggtitle("Widow Rockfish") +
  # ggtitle("Canary Rockfish") +
  ggtitle("Shortspine Thornyhead") +
  ylab("biotic gradient") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

grad_flatfish <- plot_vocc(filter(model$data, 
      # species == "mature Flathead Sole"),
      species == "mature Dover Sole"),
  vec_aes = NULL,
  fill_col = "biotic_grad", fill_label = "% biomass \nper km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "B",
  axis_lables = T,
  transform_col = fourth_root_power,
  legend_position = c(0.15, 0.25)
) + 
  # ggtitle("Flathead Sole") +
  ggtitle("Dover Sole") +
  # # ylab("gradient") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )
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
grad_temp + grad_do +  grad_rockfish + grad_flatfish + plot_layout(ncol = 2)
ggsave(here::here("ms", "figs", "supp-gradient-maps.png"), width = 6, height = 9)



### Scatterplots of coorelation between biotic gradient and temperature gradient ####

ggplot(filter(model$data, age == "mature"), aes(temp_grad, biotic_grad)) + geom_point(alpha=.1) + 
  geom_smooth(method = "lm", alpha= 0.5, colour = "darkgray") + 
  # facet_grid(species~rockfish) +
  facet_wrap(~forcats::fct_reorder(species_only, genus_id)) +
  # coord_cartesian(xlim = c(0,0.3), ylim = c(0,1)) + 
  # scale_x_continuous(trans = fourth_root_power) + 
  # scale_y_continuous(trans = fourth_root_power) +
  gfplot::theme_pbs() 

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
#### GLOBAL COEFS 
#########################
coef_names <- shortener(unique(model$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", "change in DO", "mean DO", 
  "biomass", "interaction (T)", "interaction (DO)")
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas$model <- "trend"
ggplot(overall_betas, aes(coef_names, betas)) + geom_pointrange(aes(ymin = lowerCI, ymax = upperCI)) + coord_flip()

coef_names <- shortener(unique(model_vel_t$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", 
  "biomass", "interaction (T)")
betas <- signif(as.list(model_vel_t$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel_t$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_vel_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_t$model <- "velocity (T only)"

coef_names <- shortener(unique(model_vel_d$coefs$coefficient))
coef_names <- c("intercept", "change in DO", "mean DO", 
  "biomass", "interaction (DO)")
betas <- signif(as.list(model_vel_d$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel_d$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_vel_d <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_d$model <- "velocity (DO only)"

overall_betas$model_type <- "trend"
overall_betas_vel_t$model_type <- "velocity"
overall_betas_vel_d$model_type <- "velocity"

overall <- rbind.data.frame(overall_betas, overall_betas_vel_t, overall_betas_vel_d)

filter(overall, coef_names != "intercept") %>%  
  # overall %>%
  ggplot(aes(forcats::fct_reorder(coef_names, betas), betas, colour = model)) + #forcats::fct_reorder(coef_names, )
  geom_pointrange(aes(ymin = lowerCI, ymax = upperCI),position = position_jitter(width = 0.25)) + # dodge.width = 1.2) + 
  geom_hline(yintercept = 0, colour = "darkgray") +
  scale_colour_manual(values = c("#D53E4F", "#3288BD", "#5E4FA2")) +
  xlab("") +
  coord_flip() +
  gfplot::theme_pbs()

#### add species level coefs ####
trendcoefs <-add_colours(model$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
  levels = c("(Intercept)", "temp_trend_scaled", "mean_temp_scaled", "DO_trend_scaled", "mean_DO_scaled",
    "log_biomass_scaled", "temp_trend_scaled:mean_temp_scaled", "DO_trend_scaled:mean_DO_scaled" ),
  labels = c("intercept", "change in T", "mean T", "change in DO", "mean DO",
    "biomass", "interaction (T)", "interaction (DO)")))

velcoefs1 <- add_colours(model_vel_t$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
  levels = c("(Intercept)", "squashed_temp_vel_scaled", "mean_temp_scaled",
    "log_biomass_scaled", "squashed_temp_vel_scaled:mean_temp_scaled"),
  labels = c("intercept", "change in T", "mean T",
    "biomass", "interaction (T)")))

velcoefs2 <- add_colours(model_vel_d$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
  levels = c("(Intercept)", "squashed_DO_vel_scaled", "mean_DO_scaled",
    "log_biomass_scaled", "squashed_DO_vel_scaled:mean_DO_scaled"),
  labels = c("intercept", "change in DO", "mean DO",
    "biomass", "interaction (DO)")))

trendcoefs$model <- "trend"
velcoefs1$model <- "velocity (T only)"
velcoefs2$model <- "velocity (DO only)"
trendcoefs$model_type <- "trend"
velcoefs1$model_type <- "velocity"
velcoefs2$model_type <- "velocity"

allcoefs <- rbind.data.frame(trendcoefs, velcoefs1, velcoefs2)
head(allcoefs)

filter(overall, coef_names != "intercept") %>%
  # overall %>%
  ggplot(aes(coef_names, betas)) + #forcats::fct_reorder(coef_names, ) #, colour = model
  geom_point(aes(forcats::fct_reorder(coefficient, Estimate), Estimate,  colour = age), alpha= 0.3, position = position_jitter(width = 0.15), inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")) +
  geom_boxplot(aes(forcats::fct_reorder(coefficient, Estimate), Estimate,  colour = age), 
    notch =T,
    inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")) +
  geom_pointrange(aes(ymin = lowerCI, ymax = upperCI), size = 1, fatten = 3, alpha = .9) + #position = position_jitter(width = 0.2), shape = "|"
  geom_hline(yintercept = 0, colour = "darkgray") +
  scale_colour_viridis_d(begin = .8, end = .2) +
  # scale_y_continuous(trans = fourth_root_power) +
  # scale_colour_manual(values = c("#D53E4F", "#5E4FA2", "#3288BD")) + #
  xlab("") +
  facet_wrap(~model_type) +
  coord_flip(ylim = c(-2,2)) + # ylim = c(-10,10)
  gfplot::theme_pbs()

### USE DOTWHISKER ####
# allcoefs2 <- allcoefs %>% rename(term = coefficient, estimate = Estimate, std.error = Std..Error) %>% filter(term != "intercept")
overall2 <- overall %>% rename(term = coef_names, estimate = betas, std.error = SE) %>% filter(term != "intercept")

dotwhisker::dwplot(overall2) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  # geom_point(aes(term, estimate,  colour = model), alpha= 0.1, position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) + 
  scale_colour_manual(values = c("#D53E4F", "#3288BD", "#5E4FA2")) +
  gfplot::theme_pbs()
ggsave(here::here("ms", "figs", "supp-global-coefs.pdf"), width = 5, height = 2.5)


### check for age effect ####
model_age <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-21-trend-w-age-1-500-temp.rds")
model_age <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-22-trend-w-age-1-400-temp.rds")
model_age <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-22-trend-w-age-1-500-DO.rds")
coef_names <- shortener(unique(model_age$coefs$coefficient))
# coef_names <- c("intercept", "change in DO", "mean DO", 
  # "biomass", "interaction (DO)")
betas <- signif(as.list(model_age$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_age$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_age <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_age$model <- "trend (with age effect)"
overall_betas_age <- overall_betas_age %>% 
  rename(term = coef_names, estimate = betas, std.error = SE) %>% filter(term != "intercept")

dotwhisker::dwplot(overall_betas_age, 
  order_vars = c("Intercept", "log_biomass", "age", 
    "temp", "temp_trend", "temp_trend:temp", "age:temp_trend",  "age:temp",  
    "DO", "DO_trend", "DO_trend:DO", "age:DO_trend", "age:DO")) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  # geom_point(aes(term, estimate,  colour = model), alpha= 0.1, position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) + 
  # scale_colour_manual(values = c("#D53E4F", "#3288BD", "#5E4FA2")) +
  gfplot::theme_pbs()

#########################
#### ALL CHOPSTICKS AND SLOPE WORM PLOTS 
### ALL TREND CHOPS ####
temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)
temp_slopes <- left_join(temp_slopes, stats)
do_slopes <- left_join(do_slopes, stats)

p_temp_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass",
  slopes = temp_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim = c(-11, 7)) +
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

p_do_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = "Predicted % change in biomass",
  slopes = do_slopes
) + coord_cartesian(ylim = c(-4, 5)) +
  xlab("DO trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_all_slopes <- plot_chopstick_slopes(temp_slopes,
  type = "temp", add_global = F, point_size = 1,
  legend_position = c(.25, .95)
) +
  ylab(" ")

p_do_all_slopes <- plot_chopstick_slopes(do_slopes,
  type = "DO", add_global = F, point_size = 1,
  legend_position = c(.25, .95)
) +
  coord_flip(ylim = c(-3.1, 1.4)) + # coord_flip(ylim =c(-3,1)) +
  ylab("slopes")

cowplot::plot_grid(p_temp_all_slopes, p_temp_chops, p_do_all_slopes, p_do_chops, ncol = 2, rel_widths = c(1, 2.5))
ggsave(here::here("ms", "figs", "supp-trend-chopsticks.pdf"), width = 14, height = 11)



### ALL VELOCITY CHOPS ####
temp_vel_slopes <- chopstick_slopes(model_vel_t,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp"
) %>%
  mutate(sort_var = slope_est)

do_vel_slopes <- chopstick_slopes(model_vel_d,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
) %>%
  mutate(sort_var = slope_est)

p_temp_vel_chops <- plot_fuzzy_chopsticks(model_vel_t,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = "Predicted mature biomass vel",
  slopes = temp_vel_slopes # if add, the global slope can be included for insig
) + coord_cartesian(ylim = c(-25, 37)) +
  xlab("Temperature velocity (scaled)") + theme(legend.position = "none")

p_do_vel_chops <- plot_fuzzy_chopsticks(model_vel_d,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  y_label = "Predicted mature biomass vel",
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim = c(-25, 37)) +
  xlab("DO velocity (scaled)") + theme(legend.position = "none")

temp_vel_slopes$species[temp_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_vel_slopes$species[do_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_all_vel_slopes <- plot_chopstick_slopes(temp_vel_slopes,
  type = "temp", add_global = F, point_size = 1,
  legend_position = c(.25, .95)
) +
  ylab(" ") +
  # coord_flip(ylim =c(-12,8))
  coord_flip(ylim = c(-14, 7.85))

p_do_all_vel_slopes <- plot_chopstick_slopes(do_vel_slopes,
  type = "DO", add_global = F, point_size = 1,
  legend_position = c(.25, .95)
) +
  ylab("slopes") +
  # coord_flip(ylim =c(-3,3.5))
  coord_flip(ylim = c(-5.75, 3.25))


cowplot::plot_grid(p_temp_all_vel_slopes, p_temp_vel_chops, p_do_all_vel_slopes, p_do_vel_chops, ncol = 2, rel_widths = c(1, 2.5))
ggsave(here::here("ms", "figs", "supp-vel-chopsticks.pdf"), width = 14, height = 11)


#########################
#### SLOPE SCATTERPLOTS 
### investigate interaction slopes by mean age ####
# temp_slopes <- left_join(temp_slopes, stats)
# do_slopes <- left_join(do_slopes, stats)
all_slopes <- rbind(temp_slopes, do_slopes)
all_slopes <- rbind(temp_slopes, do_slopes) %>%  mutate(growth_rate = length_50_mat_f/age_mat)

slope_depth <- slope_scatterplot(all_slopes, "depth",
  col_group = "age", point_size = 3
) +
  # geom_smooth(
  #   data = filter(all_slopes, age == "immature"), inherit.aes = F,
  #   aes_string("depth", "slope_est", colour = "age"), method = "lm", size = 0.5,
  #   fill = "lightgray"
  # ) +
  geom_smooth(method= "lm", size = 0.5, colour = "gray", fill = "lightgray") + # formula = y ~ x + I(x^2),  colour = "gray",
  geom_hline(yintercept = 0, colour = "black", alpha = 0.5,  linetype = "dashed") +
  xlab("mean depth") +
  ylab("slope") +
  facet_grid(type~chopstick, scales = "free") +
  guides(colour = F) +
  theme(
    plot.margin = margin(0, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4)
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank()
  )

slope_age <- slope_scatterplot(all_slopes, "age_mean",
  col_group = "age", point_size = 3
) +
  geom_smooth(
    data = filter(all_slopes, age == "immature"), inherit.aes = F,
    aes_string("age_mean", "slope_est", colour = "age"), method = "lm", size = 0.5,
    fill = "lightgray"
  ) +
  # geom_smooth(method= "lm", size = 0.5, fill = "lightgray") + # formula = y ~ x + I(x^2),  colour = "gray",
  geom_hline(yintercept = 0, colour = "black", alpha = 0.5,  linetype = "dashed") +
  xlab("mean age") +
  ylab("slope") + 
  facet_grid(type~chopstick, scales = "free") +  
  guides(colour = F) +
  theme(
    plot.margin = margin(0, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4)
  )

### investigate immature growth rate ####
slope_growth <- slope_scatterplot(all_slopes, "growth_rate",
  col_group = "age", point_size = 3
) +
  geom_smooth(
    data = filter(all_slopes, age == "immature"), inherit.aes = F,
    aes_string("growth_rate", "slope_est", colour = "age"), method = "lm", size = 0.5,
    fill = "lightgray"
  ) +
  # geom_smooth(method= "lm", size = 0.5, fill = "lightgray") +
  # xlab("mean age") +
  ylab("slope") + 
  facet_grid(type~chopstick, scales = "free") +
  guides(colour = F) +
  theme(
    plot.margin = margin(0, 0.1, 0.1, 0, "cm"),
    strip.background = element_blank(),
    # legend.position = c(.75, .15), legend.title = element_blank(),
    # strip.text = element_blank(),
    axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
  )

cowplot::plot_grid(slope_age, slope_growth, ncol = 2, rel_widths = c(1, 1))
ggsave(here::here("ms", "figs", "supp-slope-scatterplots.pdf"), width = 8, height = 4)

#########################
#### COEFFICIENT SCATTERPLOTS
### investigate mean age ####
## when mean age is less, than negative temperature effects are more likely?
p_age_alone <- coef_scatterplot(
  trendeffects,
  # filter(trendeffects, age == "immature"),
  coef = c("temperature", "DO"),
  x = "age_mean", group = "age", regression = F
) +
  xlab("mean age") +
  scale_colour_viridis_d(begin = .8, end = .21) +
  scale_y_continuous(expand = expansion(mult = .2)) +
  # ggtitle("") +
  guides(colour = F) +
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
    aes_string("age_mean", "Estimate"), method = "lm",
    colour = "darkgray", fill = "lightgray"
  ) +
  geom_point(alpha = 0.5, size = 2) +
  facet_grid(rows = vars(coefficient), scales = "free") + #cols = vars(rockfish), 
  # theme(legend.position = c(.8,.15), legend.title = element_blank()) +
  ylab("trend coefficient")
p_age_alone
ggsave(here::here("ms", "figs", "supp-coef-by-mean-age.pdf"), width = 3.5, height = 2.7)



#########################
#### SPATIAL RESIDUALS
### for trend model ####
data <- model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual))

data$species_only[data$species_only == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"

data %>% filter(age == "mature") %>% 
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() + 
  theme(legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
  facet_wrap(~species_only, strip.position = "bottom") +
  ggtitle("Mature fish biomass trend residuals")
ggsave(here::here("ms", "figs", "supp-mat-residuals.pdf"), width = 7, height = 7)

data %>% filter(age == "immature") %>% 
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() + 
  theme(legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5), 
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
  facet_wrap(~species_only, strip.position = "bottom") + 
  ggtitle("Immature fish biomass trend residuals")
ggsave(here::here("ms", "figs", "supp-imm-residuals.pdf"), width = 7, height = 6)
