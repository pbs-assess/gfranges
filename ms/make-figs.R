setwd(here::here())

library(TMB)
library(tidyverse)
library(patchwork)
library(gfranges)

# load appropriate final models
model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")

# model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
# model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")

model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-12-vel-temp-1-200-temp.rds")
model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-12-vel-do-1-200-DO.rds")

stats <- readRDS(paste0("analysis/VOCC/data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "rockfish", "other fishes")
stats <- stats %>% separate(species_science_name, " ", into = c("genus", "specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
stats$group[stats$group == "HAKE"] <- "COD"
imm <- mutate(stats, age = "immature") %>%
  mutate(depth = depth_imm, age_mean = age_imm) %>%
  select(-depth_imm, -age_imm)
mat <- mutate(stats, age = "mature") %>% select(-depth_imm, -age_imm)
stats <- rbind(mat, imm)
stats$family <- gsub("\\(.*", "", stats$parent_taxonomic_unit)
alldata <- readRDS(paste0("analysis/VOCC/data/all-do-with-null-1-untrimmed-allvars.rds"))

#### CLIMATE MAPS ####
mean_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "mean_DO", fill_label = "ml/L ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F,
  legend_position = c(0.15, 0.25)
) + ggtitle("dissolved oxygen") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

mean_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "mean_temp", fill_label = "ºC ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "C",
  axis_lables = T,
  legend_position = c(0.15, 0.25)
) + ggtitle("temperature") +
  ylab("mean conditions") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

trend_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "DO_trend", fill_label = "ml/L ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = F,
  legend_position = c(0.15, 0.25)
) + #ggtitle("dissolved oxygen") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

trend_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "temp_trend", fill_label = "ºC ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE, 
  axis_lables = T,
  legend_position = c(0.15, 0.25)
) + #ggtitle("temperature") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  ylab("change per decade") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

vel_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_DO_vel", fill_label = "km",
  raster_cell_size = 4,
  na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = F,
  legend_position = c(0.15, 0.25)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) + 
  theme(
  plot.margin = margin(0, 0, 0, 0, "cm"),
  axis.text = element_blank(), axis.ticks = element_blank(),
  axis.title.x = element_blank(), axis.title.y = element_blank()
)

vel_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_temp_vel", fill_label = "km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = T,
  legend_position = c(0.15, 0.25)
) + ylab("velocities per decade") + 
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

mean_temp + mean_do +  trend_temp + trend_do + vel_temp + vel_do + plot_layout(ncol = 2)
ggsave(here::here("ms", "figs", "climate-maps-updated.png"), width = 6, height = 9)



#### SEPARATE WORM PLOTS ####

### WORM PLOTS OF SLOP ESTIMATES FROM TREND MODEL 
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

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.25, .95)
) + theme(axis.title.x = element_blank()) +
  ggtitle("temperature")
p_do_worm <- plot_chopstick_slopes(do_slopes,
  type = "DO",
  legend_position = c(.25, .95)
) + coord_flip(ylim = c(-3.1, 1.4)) +
  ggtitle("DO") +
  # ylab("slopes")
  theme(axis.title.x = element_blank())

(p_temp_worm | p_do_worm) / grid::textGrob("slope of biomass trend with a SD change in climate", just = 0.31) + plot_layout(height = c(10.5, 0.25))
ggsave(here::here("ms", "figs", "worm-plot-trend.pdf"), width = 8, height = 6)

# meta-analytical coefficients? ... all span zero, but could include as appendix?

### WORM PLOTS OF SLOP ESTIMATES FROM VELOCITY MODELS 

temp_vel_slopes <- chopstick_slopes(model_vel_t,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled",
  type = "temp"
)
# temp_vel_slopes <- left_join(temp_vel_slopes, stat)
temp_vel_slopes <- temp_vel_slopes %>% mutate(sort_var = slope_est)
temp_vel_slopes$species[temp_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_vel_slopes <- chopstick_slopes(model_vel_d,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
)
do_vel_slopes <- do_vel_slopes %>% mutate(sort_var = slope_est)
do_vel_slopes$species[do_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

# p_temp_worm2 <- plot_chopstick_slopes(temp_vel_slopes, type = "temp",
#   legend_position = c(.25,.95)) + coord_flip(ylim =c(-14,7.75)) +
#   theme(axis.title.x = element_blank()) +
#   ggtitle("temperature")
# p_do_worm2 <- plot_chopstick_slopes(do_vel_slopes, type = "DO",
#   legend_position = c(.25,.95)) + coord_flip(ylim =c(-4.5,3.25)) +
#   ggtitle("DO") +
#   # ylab("slopes")
#   theme(axis.title.x = element_blank())
#
# (p_temp_worm2 | p_do_worm2) / grid::textGrob("slope of biomass velocity relative to climate velocity", just = 0.31) +
#   plot_layout(height = c(10.5, 0.25))
# ggsave(here::here("ms", "figs", "worm-plot-vel.pdf"), width = 8, height = 6)

#### IF WE WANT PLOT OF BOTH TREND AND VELOCITY SLOPES TOGETHER ####
p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.25, .95)
) + theme(plot.margin = margin(0, 0, 0.2, 0.2, "cm"),
  axis.title.x = element_blank()) +
  ggtitle("temperature") + 
  xlab("% biomass change for a SD of climate change")
p_do_worm <- plot_chopstick_slopes(do_slopes,
  type = "DO",
  legend_position = c(.25, .95)
) + coord_flip(ylim = c(-3.1, 1.4)) +
  ggtitle("DO") +
  # ylab("slopes")
  theme(plot.margin = margin(0, 0, 0.2, 0, "cm"),
    axis.title.x = element_blank())

p_temp_worm3 <- plot_chopstick_slopes(temp_vel_slopes,
  type = "temp",
  legend_position = c(.25, .95)
) + coord_flip(ylim = c(-14, 7.95)) +
  theme(plot.margin = margin(0, 0, 0, 0.2, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none") +
  xlab("biotic velocity change for a SD increase in climate velocity")
p_do_worm3 <- plot_chopstick_slopes(do_vel_slopes,
  type = "DO",
  legend_position = c(.25, .95)
) + coord_flip(ylim = c(-5.75, 3.25)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none")

(p_temp_worm | p_do_worm)/(p_temp_worm3 | p_do_worm3) + plot_layout(height = c(1, 1))

# # with slope descriptions on bottom instead of left side
# ((p_temp_worm | p_do_worm) / grid::textGrob("", just = 0.31) +
#   plot_layout(height = c(10, 0.25))) /
#   ((p_temp_worm3 | p_do_worm3) / grid::textGrob("", just = 0.31) + plot_layout(height = c(10, 0.25))) +
#   plot_layout(height = c(5, 0.1, 5, 0.1))

ggsave(here::here("ms", "figs", "worm-plot-both.pdf"), width = 8, height = 10)

#### EXAMPLE SPECIES CHOPSTICK PLOTS AND MAPS FROM TREND MODEL ####
# TODO: for example species...
# temperature maps plus chopsticks
# O2 maps plus chopsticks

species_panels <- function(species, model, x_type, alpha_range = c(0.9, 0.9)) {
  biotic_map <- filter(model$data, species == !!species) %>% plot_vocc(
    vec_aes = NULL,
    fill_col = "biotic_trend", fill_label = "", raster_cell_size = 4,
    na_colour = "lightgrey", white_zero = TRUE,
    high_fill = "#276b95", #"Steel Blue 4", # "#5E4FA2", #
    low_fill = "Red 3", # "#FF8B09", #
    axis_lables = T, legend_position = c(0.15, 0.25), make_square = F
  ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    ylab("Predicted % change in biomass") +
    ggtitle(paste0(species)) + theme(
      plot.margin = margin(0, 0, 0, 0.5, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    )

  if (x_type == "temp") {
    temp_slopes <- chopstick_slopes(model,
      x_variable = "temp_trend_scaled",
      interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
    )

    single_chop <- plot_fuzzy_chopsticks(model,
      x_variable = "temp_trend_scaled", type = "temp", y_label = "",
      line_size = 1,
      alpha_range = alpha_range,
      colours = c("#cd0000", "#2971A0"), # "#3288BD"),
      choose_species = stringr::str_replace(species, ".*mature ", ""),
      choose_age = gsub(" .*", "", species),
      slopes = temp_slopes # if add, the global slope can be included for insig.
    ) + geom_hline(yintercept = 0, colour = "gray", linetype = "solid") +
      theme(
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.position = "none",
        axis.title.x = element_blank()
      )
    climate_map <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL,
        fill_col = "temp_trend", fill_label = "ºC ",
        raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE, low_fill = "#0072B2", 
        axis_lables = T,
        legend_position = c(0.15, 0.25), make_square = F
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      # ggtitle("temperature") +
      xlab("Temperature trend (scaled)") +
      theme(
        plot.margin = margin(0, 0, 0, 0.5, "cm"),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title.y = element_blank()
      )
  }

  if (x_type == "DO") {
    do_slopes <- chopstick_slopes(model,
      x_variable = "DO_trend_scaled",
      interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
    )

    single_chop <- plot_fuzzy_chopsticks(model,
      x_variable = "DO_trend_scaled", type = "DO", y_label = "",
      line_size = 1, alpha_range = alpha_range,
      # colours = c("#cd0000", "#2971A0"), #"#3288BD"),
      choose_species = stringr::str_replace(species, ".*mature ", ""),
      choose_age = gsub(" .*", "", species),
      slopes = do_slopes # if add, the global slope can be included for insig.
    ) + # coord_cartesian(ylim=c(-11,7)) +
      geom_hline(yintercept = 0, colour = "gray", linetype = "solid") +
      theme(
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.position = "none",
        axis.title.x = element_blank()
      )

    climate_map <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL,
        fill_col = "DO_trend", fill_label = "ml/L ",
        raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
        high_fill = "#5E4FA2", low_fill = "#FDAE61",
        axis_lables = T,
        legend_position = c(0.15, 0.25), make_square = F
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      xlab("DO trend (scaled)") +
      theme(
        plot.margin = margin(0, 0, 0, 0.5, "cm"),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title.y = element_blank()
      )
  }

  biotic_map + single_chop + climate_map + plot_layout(ncol = 1, heights = c(2, 0.5, 2))
  # cowplot::plot_grid(biotic_map, single_chop, climate_map, ncol = 1, rel_heights = c(2,0.5,2), rel_widths = c(2,2,2))
  ggsave(here::here("ms", "figs", paste0("panels-", x_type, "-", species, ".pdf")), width = 4, height = 10)
}

species_panels ("mature North Pacific Spiny Dogfish", model, "temp")
species_panels ("mature Sablefish", model, "temp", alpha_range = c(0.25,0.9))
species_panels ("mature Pacific Cod", model, "temp", alpha_range = c(0.25,0.9))

species_panels ("mature Widow Rockfish", model, "temp")
species_panels ("mature Bocaccio", model, "temp") 
species_panels ("mature Canary Rockfish", model, "temp", alpha_range = c(0.25,0.9))
species_panels ("mature Redbanded Rockfish", model, "temp")
species_panels ("mature Shortspine Thornyhead", model, "temp")

species_panels ("mature English Sole", model, "temp")
# species_panels ("mature Dover Sole", model, "temp")
species_panels ("mature Flathead Sole", model, "temp")
species_panels ("mature Arrowtooth Flounder", model, "temp")

species_panels ("mature North Pacific Spiny Dogfish", model, "DO")
species_panels ("mature Sablefish", model, "DO")
species_panels ("mature Pacific Cod", model, "DO")

species_panels ("mature Canary Rockfish", model, "DO", alpha_range = c(0.25,0.9))
species_panels ("mature Yelloweye Rockfish", model, "DO", alpha_range = c(0.25,0.9))
species_panels ("mature Bocaccio", model, "DO", alpha_range = c(0.25,0.9))
species_panels ("mature Redbanded Rockfish", model, "DO")
species_panels ("mature Widow Rockfish", model, "DO")
species_panels ("mature Shortspine Thornyhead", model, "DO")

species_panels ("mature Pacific Halibut", model, "DO")
species_panels ("mature English Sole", model, "DO")
species_panels ("mature Flathead Sole", model, "DO")
species_panels ("mature Arrowtooth Flounder", model, "DO")

#### SLOPE SCATTERPLOTS AGAINST DEPTH ####

# prep data 
do_data <- readRDS(paste0("analysis/VOCC/data/predicted-DO-new.rds")) %>%
  select(X, Y, year, depth, temp, do_est)
temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)

temp_slopes <- left_join(temp_slopes, stats)
temp_slopes <- temp_slopes %>% mutate(sort_var = slope_est)
temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_slopes <- left_join(do_slopes, stats)
do_slopes <- do_slopes %>% mutate(sort_var = slope_est)
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_slopes <- mutate(do_slopes, species_lab = if_else(slope_est < -0.75|depth >270, species, ""))
temp_slopes <- mutate(temp_slopes, species_lab = if_else(slope_est < -2.25|depth >270, species, ""))
do_slopes$species_lab <- gsub("Rockfish", "", do_slopes$species_lab)
temp_slopes$species_lab <- gsub("Rockfish", "", temp_slopes$species_lab)

# build plots
depth <- ggplot(do_data, aes(depth, do_est)) +
  geom_point(aes(depth, temp/2), alpha = 0.02, shape = 20, colour = "#3d95cc", size = 0.2) +
  geom_point(aes(depth, do_est), alpha = 0.02, shape = 20, colour = "darkorchid4", size = 0.2) +
  geom_smooth(colour = "darkorchid4", size = 0.5) + 
  xlim(15, 450) + 
  ylab("mean DO (ml/L)") +
  scale_y_continuous(sec.axis = sec_axis( ~ (./2), name = "temperature (ºC)")) +  #, expand = expand_scale(mult = c(0.05, .2)
  geom_smooth(aes(depth, temp/2), inherit.aes = F, colour = "#3d95cc", size = 0.5) + 
  geom_hline(yintercept = 1.4, colour = "black", linetype = "dashed") +
  xlab("mean depth") +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.3, 0.2, 0.2, "cm"),
    axis.text.y.left = element_text(color = "darkorchid4"),
    axis.text.y.right = element_text(color = "#3d95cc"),
    axis.title.y.left = element_text(color = "darkorchid4"),
    axis.title.y.right = element_text(color = "#3d95cc") #, vjust = -0.2
    # axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  )

temp_high <- slope_scatterplot(
  filter(temp_slopes, chopstick == "high"), "depth",
  col_group = "age",  
  point_alpha = 0.8,
  point_size = 1.5
) +
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label=species_lab), 
    size=2, force = 2, #nudge_y = -0.3, nudge_x = 20, 
    na.rm = T, min.segment.length = 1) +
  # scale_y_continuous(trans = fourth_root_power) +
  # geom_smooth(method= "lm", size = 0.5) +
  ylab("slope at highest temperature") +
  xlim(15, 450) + 
  theme(
    plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
    legend.position = c(.85, .2), 
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
    legend.title = element_blank())

do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth",
  col_group = "age", 
  point_alpha = 0.8,
  point_size = 1.5, 
) +
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  xlab("mean depth for species") +
  ylab("slope at lowest DO") + guides(colour = F) +
  xlim(15, 450) + 
  ggrepel::geom_text_repel(aes(label=species_lab), 
    size=2, force = 2, nudge_y = -0.35, nudge_x = 15,     
    na.rm = T, min.segment.length = 2) +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  )

temp_high + do_low + depth + plot_layout(ncol = 1, heights = c(1, 1, 1))
ggsave(here::here("ms", "figs", "slope-by-depth.png"), width = 4.5, height = 7)

#### COEFFICIENT SCATTERPLOTS AGAINST LIFE HISTORY ####
model2 <- add_colours(model$coefs, species_data = stats)
model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
model2 <- model2 %>%
  group_by(group) %>%
  mutate(spp_count = length(unique(species))) %>%
  ungroup()
model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc = F))
model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc = F))
trendeffects <- model2 %>%
  filter(coefficient %in% c("temp_trend_scaled", "DO_trend_scaled")) %>%
  transform(coefficient = factor(coefficient,
    levels = c("temp_trend_scaled", "DO_trend_scaled"),
    labels = c("temperature", "DO")
  ))
trendeffects <- trendeffects %>%
  mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc = F))


p_depth <- coef_scatterplot(trendeffects,
  coef = c("temperature", "DO"),
  x = "depth", group = "age", regression = F
  ) +
  ylab("trend coefficient") + xlab("mean depth") +
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
    aes_string("depth", "Estimate"), method = "lm",
    # colour = "darkgray", 
    fill = "lightgray"
  ) +
  scale_colour_viridis_d(begin = .8, end = .2) +
  guides(colour = F) + labs(subtitle = "all species") +
  facet_grid(rows = vars(coefficient), scales = "free_y") +
  # gfplot:::theme_pbs() %+replace%
  theme(
    plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4)
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank()
  )

p_age <- coef_scatterplot(trendeffects,
  coef = c("temperature", "DO"),
  x = "age_mean", group = "age", regression = F
  ) +
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
    aes_string("age_mean", "Estimate"), method = "lm",
    # colour = "darkgray", 
    fill = "lightgray"
  ) +
  xlab("mean age") +
  scale_colour_viridis_d(begin = .8, end = .2) +
  guides(colour = F) +
  theme(
    plot.margin = margin(0, 0.25, 0.1, 0.1, "cm"), strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()
  ) +
  facet_grid(rows = vars(coefficient), cols = vars(rockfish), scales = "free")

trendeffects <- mutate(trendeffects, growth_rate = length_50_mat_f/age_mat, growth_rate_m = length_50_mat_m/age_mat_m) 
# plot(data = trendeffects, growth_rate ~ growth_rate_m)
p_mat <- coef_scatterplot(trendeffects,
  coef = c("temperature", "DO"),
  x = "growth_rate",
  # x = "length_50_mat_f", 
  group = "age", regression = F
  ) +
  xlab("immature growth rate") +
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
    aes_string("growth_rate", "Estimate"), method = "lm",
    # colour = "darkgray", 
    fill = "lightgray"
  ) +
  scale_colour_viridis_d(begin = .8, end = .2) +
  theme(
    plot.margin = margin(0, 0.1, 0.1, 0, "cm"),
    strip.background = element_blank(),
    legend.position = c(.75, .15), legend.title = element_blank(),
    # strip.text = element_blank(),
    axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
  ) +
  facet_grid(coefficient ~ rockfish, scales = "free")

cowplot::plot_grid(p_depth, p_age, p_mat, nrow = 1, rel_widths = c(1.1, 1.75, 1.75))
ggsave(here::here("ms", "figs", "coef-scatterplots.pdf"), width = 8, height = 4)
