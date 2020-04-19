#### SUPPLEMENTARY FIGURES
# # if make-figs not just run
# setwd(here::here())
# library(TMB)
# library(tidyverse)
# library(patchwork)
# library(gfranges)
#
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

#### Gradient maps ####
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



#### Scatterplots of coorelation between biotic gradient and temperature gradient ####

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

#### ALL TREND CHOPS ####
temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)

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
ggsave(here::here("ms", "figs", "supp-trend-chopsticks.pdf"), width = 14, height = 12)



#### ALL VELOCITY CHOPS ####
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
ggsave(here::here("ms", "figs", "supp-vel-chopsticks.pdf"), width = 14, height = 12)



#### COEFFICIENT SCATTERPLOTS ####
## If make-figs not run first...
# model2 <- add_colours(model$coefs, species_data = stats)
# model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
# model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
# model2 <- model2 %>% group_by(group) %>% mutate(spp_count = length(unique(species))) %>% ungroup()
# model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc=F))
# model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc=F))
# trendeffects <- model2 %>% filter(coefficient %in% c("temp_trend_scaled","DO_trend_scaled")) %>% transform(coefficient = factor(coefficient, levels = c("temp_trend_scaled","DO_trend_scaled"), labels = c("temperature", "DO")))
# trendeffects <- trendeffects %>% mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc=F))



#### MEAN AGE ####
#### when mean age is less, than negative temperature effects are more likely?
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


### investigate interaction slopes by mean age
# temp_slopes <- left_join(temp_slopes, stats)
# do_slopes <- left_join(do_slopes, stats)
all_slopes <- rbind(temp_slopes, do_slopes)
all_slopes <- rbind(temp_slopes, do_slopes) %>%  mutate(growth_rate = length_50_mat_f/age_mat)

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
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4)
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank()
  )



#### Investigate immature growth rate ####
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



#### VARIABILITY IN COEFS INCREASES WITH DEPTH ####
do_data <- readRDS(paste0("analysis/VOCC/data/predicted-DO-new.rds")) %>%
  select(X, Y, year, depth, temp, do_est)
do_depth <- ggplot(do_data, aes(depth, do_est)) +
  geom_jitter(alpha = 0.01, shape = 20, colour = "black", size = 0.2) +
  geom_smooth(colour = "black", size = 0.5) + xlim(0, 450) + ylab("mean DO (ml/L)") +
  geom_hline(yintercept = 2, colour = "black", linetype = "dashed") +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.1, 0.2, 0, "cm"),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  )

# temp_depth <- ggplot(do_data, aes(depth, temp)) +
#   geom_jitter(alpha = 0.01, shape = 20, colour = "black", size=0.2) +
#   geom_smooth(colour = "black", size = 0.5) + xlim(0,450) + ylab("mean temperature (ºC)") +
#   gfplot::theme_pbs() + theme(plot.margin = margin(0, 0.1, 0, 0, "cm"),
#     axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())


do_slopes <- do_slopes %>% mutate(sort_var = slope_est)
do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth",
  col_group = "age", point_size = 3
) +
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  xlab("mean depth for species") +
  ylab("slope at lowest DO") + guides(colour = F) +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.1, 0, 0, "cm"),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  )

temp_slopes <- temp_slopes %>% mutate(sort_var = slope_est)
temp_high <- slope_scatterplot(
  filter(temp_slopes, chopstick == "high"), "depth",
  col_group = "age", point_size = 3
) +
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  # scale_y_continuous(trans = fourth_root_power) +
  # geom_smooth(method= "lm", size = 0.5) +
  xlab("mean depth of cell/species-maturity class") +
  ylab("slope at highest temperature") +
  theme(legend.position = c(.8, .25), legend.title = element_blank())

do_depth + do_low + temp_high + plot_layout(ncol = 1, heights = c(1, 1, 1))
ggsave(here::here("ms", "figs", "supp-slope-by-depth.png"), width = 4.5, height = 7)


