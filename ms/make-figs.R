setwd(here::here())
library(patchwork)

#### CLIMATE MAPS
alldata <- readRDS(paste0("analysis/VOCC/data/all-do-with-null-1-untrimmed-allvars.rds"))

trend_do <- plot_vocc(alldata, vec_aes = NULL,
  fill_col = "DO_trend", fill_label = "ml/L ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = F,
  legend_position = c(0.15, 0.25)
) + ggtitle("dissolved oxygen") + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank())

trend_temp <- plot_vocc(alldata, vec_aes = NULL,
  fill_col = "temp_trend", fill_label = "ºC ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = T,
  legend_position = c(0.15, 0.25)
) + ggtitle("temperature") + ylab("trends per decade") + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(), 
    axis.title.x = element_blank())


vel_do <- plot_vocc(alldata, vec_aes = NULL,
  fill_col = "squashed_DO_vel", fill_label = "km",
  raster_cell_size = 4,
  na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = F,
  legend_position = c(0.15, 0.25)
) + theme(plot.margin = margin(0, 0, 0, 0, "cm"), 
  axis.text = element_blank(), axis.ticks = element_blank(), 
  axis.title.x = element_blank(), axis.title.y = element_blank())

vel_temp <- plot_vocc(alldata, vec_aes = NULL,
  fill_col = "squashed_temp_vel", fill_label = "km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  axis_lables = T,
  legend_position = c(0.15, 0.25)
) + ylab("velocities per decade") + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(), 
    axis.title.x = element_blank())

trend_temp + trend_do + vel_temp + vel_do + plot_layout(ncol=2) 

ggsave(here::here("ms", "figs", "climate-maps.pdf"), width = 6, height = 6)




#### WORM PLOTS OF SLOP ESTIMATES FROM TREND MODEL
temp_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
temp_slopes$species[temp_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	


p_temp_worm <- plot_chopstick_slopes(temp_slopes, type = "temp", 
  legend_position = c(.25,.95)) + theme(axis.title.x = element_blank())+
  ggtitle("temperature") 
p_do_worm <- plot_chopstick_slopes(do_slopes, type = "DO", 
  legend_position = c(.25,.95)) + coord_flip(ylim =c(-3.1,1.4)) + 
  ggtitle("DO") +  
  # ylab("slopes") 
  theme(axis.title.x = element_blank())

(p_temp_worm | p_do_worm) / grid::textGrob("slope of biomass change with unit/decade of change in climate", just = 0.31) + plot_layout(height = c(10.5,0.25)) 
ggsave(here::here("ms", "figs", "worm-plot-trend.pdf"), width = 8, height = 6)

# meta-analytical coefficients? ... all span zero, but could include as appendix?


#### WORM PLOTS OF SLOP ESTIMATES FROM VELOCITY MODELs

model_vel_t <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
temp_vel_slopes <- chopstick_slopes(model_vel_t , x_variable = "squashed_temp_vel_scaled", 
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp") 
# temp_vel_slopes <- left_join(temp_vel_slopes, stat)
temp_vel_slopes <- temp_vel_slopes %>% mutate(sort_var = slope_est)
temp_vel_slopes$species[temp_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

model_vel_d <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")
do_vel_slopes <- chopstick_slopes(model_vel_d, x_variable = "squashed_DO_vel_scaled", 
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO") 
do_vel_slopes <- do_vel_slopes %>% mutate(sort_var = slope_est)
do_vel_slopes$species[do_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

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
# 
#### IF WE WANT PLOT OF BOTH TREND AND VELOCITY SLOPES TOGETHER
p_temp_worm3 <- plot_chopstick_slopes(temp_vel_slopes, type = "temp", 
  legend_position = c(.25,.95)) + coord_flip(ylim =c(-14,7.75)) + 
  theme(axis.title.x = element_blank()) 
p_do_worm3 <- plot_chopstick_slopes(do_vel_slopes, type = "DO", 
  legend_position = c(.25,.95)) + coord_flip(ylim =c(-4.5,3.25)) +
  theme(axis.title.x = element_blank())

((p_temp_worm | p_do_worm) / grid::textGrob("slope of biomass change with unit/decade of change in climate", just = 0.31) + 
    plot_layout(height = c(10, 0.25))) /
((p_temp_worm3 | p_do_worm3) / grid::textGrob("slope of biomass velocity relative to climate velocity", just = 0.31) + plot_layout(height = c(10, 0.25))) + 
    plot_layout(height = c(5, 0.1, 5, 0.1)) 
ggsave(here::here("ms", "figs", "worm-plot-both.pdf"), width = 8, height = 12)


#### EXAMPLE SPECIES CHOPSTICK PLOTS AND MAPS FROM TREND MODEL
# TODO: for example species...
# temperature maps plus chopsticks
# O2 maps plus chopsticks


#### SLOPE SCATTERPLOTS AGAINST DEPTH

stats <- readRDS(paste0("analysis/VOCC/data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "rockfish", "other")
stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
stats$group[stats$group == "HAKE"] <- "COD"
imm <- mutate(stats, age = "immature") %>% mutate(depth = depth_imm) %>% select(-depth_imm)
mat <- mutate(stats, age = "mature") %>% select(-depth_imm)
stats <- rbind(mat, imm)
stats$family <- gsub("\\(.*", "", stats$parent_taxonomic_unit) 



do_data <- readRDS(paste0("data/predicted-DO-new.rds")) %>% 
  select(X, Y, year, depth, temp, do_est)
do_depth <- ggplot(do_data, aes(depth, do_est)) + 
  geom_jitter(alpha = 0.01, shape = 20, colour = "black", size=0.2) + 
  geom_smooth(colour = "black", size = 0.5) + xlim(0,450) + ylab("mean DO (ml/L)") +
  geom_hline(yintercept = 2, colour = "black", linetype = "dashed") + 
  gfplot::theme_pbs() + theme(plot.margin = margin(0, 0.1, 0, 0, "cm"), 
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

# temp_depth <- ggplot(do_data, aes(depth, temp)) + 
#   geom_jitter(alpha = 0.01, shape = 20, colour = "black", size=0.2) + 
#   geom_smooth(colour = "black", size = 0.5) + xlim(0,450) + ylab("mean temperature (ºC)") +
#   gfplot::theme_pbs() + theme(plot.margin = margin(0, 0.1, 0, 0, "cm"), 
#     axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())


do_slopes <- left_join(do_slopes, stats)
do_slopes <- do_slopes %>% mutate(sort_var = slope_est)
do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth", col_group = "age") + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  xlab("mean depth for species") +
  ylab("slope at lowest DO") + guides(colour=F) + 
  gfplot::theme_pbs() + theme(plot.margin = margin(0, 0.1, 0, 0, "cm"), 
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

temp_slopes <- left_join(temp_slopes, stats)
temp_slopes <- temp_slopes %>% mutate(sort_var = slope_est)
temp_high <- slope_scatterplot(filter(temp_slopes, chopstick == "high"), "depth", col_group = "age") + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # scale_y_continuous(trans = fourth_root_power) +
  # geom_smooth(method= "lm", size = 0.5) +
  xlab("mean depth of cell/species-maturity class") +
  ylab("slope at highest temperature") + guides(colour=F)

do_depth + do_low + temp_high + plot_layout(ncol = 1, heights = c(1,1,1)) 
ggsave(here::here("ms", "figs", "slope-by-depth.pdf"), width = 5, height = 8)




#### COEFFICIENT SCATTERPLOTS

model2 <- add_colours(model$coefs, species_data = stats) 
model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
model2 <- model2 %>% group_by(group) %>% mutate(spp_count = length(unique(species))) %>% ungroup()
model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc=F))
model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc=F))
trendeffects <- model2 %>% filter(coefficient %in% c("temp_trend_scaled","DO_trend_scaled")) %>% transform(coefficient = factor(coefficient, levels = c("temp_trend_scaled","DO_trend_scaled"), labels = c("temperature", "DO")))
trendeffects <- trendeffects %>% mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc=F)) 

p_depth <- coef_scatterplot(trendeffects, 
  coef = c("temperature","DO"), 
  x = "depth", group = "age", regression = T) + 
  ylab("trend coefficient") + xlab("mean depth")+
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + labs(subtitle = "all species") +
  theme(plot.margin = margin(0.1, 0, 0.1, 0, "cm"), 
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    strip.background = element_blank(), 
    strip.text = element_blank(), 
    # axis.text.y = element_blank(), 
    axis.ticks = element_blank()) + 
  facet_grid(rows = vars(coefficient), scales = "free") 

p_age <- coef_scatterplot(trendeffects, coef = c("temperature","DO"), 
  x = "age_max", group = "age", regression = T) + 
  xlab("maximum age")+
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + 
  # ggtitle("") +
  theme(plot.margin = margin(0, 0.1, 0.1, 0.1, "cm"), strip.background = element_blank(), 
    strip.text.y = element_blank(),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_grid(rows = vars(coefficient), cols = vars(rockfish), scales = "free") 

p_mat <- coef_scatterplot(trendeffects, coef = c("temperature","DO"), 
  x = "length_50_mat_f", group = "age", regression = F) + 
  xlab("length at maturity") +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  # guides(colour=F) +
  theme(plot.margin = margin(0, 0.1, 0.1, 0, "cm"), 
    strip.background = element_blank(), 
    legend.position = c(.75,.1), legend.title = element_blank(),
    # strip.text = element_blank(),
    axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  facet_grid(coefficient~rockfish, scales = "free") 

cowplot::plot_grid(p_depth, p_age, p_mat, nrow = 1, rel_widths = c(1.1, 1.75 , 1.75))
ggsave(here::here("ms", "figs", "coef-scatterplots.pdf"), width = 8, height = 4)

# p_depth + p_age + p_mat + plot_layout(nrow = 1, widths = c(1.1, 1.75 , 1.75)) 

