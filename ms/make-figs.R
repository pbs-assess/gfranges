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




#### TREND MODEL

# coefficient plots temp + O2; add meta-analytical coefficients?


# some example temperature maps plus chopsticks
# some example O2 maps plus chopsticks




#### COEFFICIENT SCATTERPLOTS

stats <- readRDS(paste0("analysis/VOCC/data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "rockfish", "other")
stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
stats$group[stats$group == "HAKE"] <- "COD"
imm <- filter(stats, age == "immature") %>% mutate(depth == depth_imm) %>% select(-depth_imm)
mat <- filter(stats, age == "mature") %>% select(-depth_imm)
stats <- rbind(mat, imm)

model2 <- add_colours(model$coefs) 
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




#### SLOPE SCATTERPLOTS AGAINST DEPTH
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

do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
do_slopes <- left_join(do_slopes, stats)
do_slopes <- do_slopes %>% mutate(sort_var = slope_est)
do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth", col_group = "age") + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  xlab("mean depth for species") +
  ylab("slope at lowest DO") + guides(colour=F) + 
  gfplot::theme_pbs() + theme(plot.margin = margin(0, 0.1, 0, 0, "cm"), 
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

temp_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
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
