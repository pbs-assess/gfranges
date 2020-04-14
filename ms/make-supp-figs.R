#### SUPPLEMENTARY FIGURES
# # if make-figs not just run
# setwd(here::here())
# library(patchwork)
# # load appropriate final models
# model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")
# model_vel_t <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
# model_vel_d <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")

#### ALL TREND CHOPS
temp_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")

p_temp_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass", 
  slopes = temp_slopes  # if add, the global slope can be included for insig.
) + coord_cartesian(ylim=c(-11,7)) + 
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

p_do_chops <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = "Predicted % change in biomass",
  slopes = do_slopes
) + coord_cartesian(ylim=c(-4,5)) + 
  xlab("DO trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

p_temp_all_slopes <- plot_chopstick_slopes(temp_slopes, 
  type = "temp", add_global = F,
  legend_position = c(.25,.95)) + 
  ylab(" ")

p_do_all_slopes <- plot_chopstick_slopes(do_slopes, 
  type = "DO", add_global = F,
  legend_position = c(.25,.95)) + 
  coord_flip(ylim =c(-3.1,1.4)) + #coord_flip(ylim =c(-3,1)) +
  ylab("slopes") 

cowplot::plot_grid(p_temp_all_slopes, p_temp_chops, p_do_all_slopes, p_do_chops, ncol = 2, rel_widths = c(1, 2.5)) 
ggsave(here::here("ms", "figs", "supp-trend-chopsticks.pdf"), width = 14, height = 12)


#### ALL VELOCITY CHOPS
temp_vel_slopes <- chopstick_slopes(model_vel_t , 
  x_variable = "squashed_temp_vel_scaled", 
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp") %>% 
  mutate(sort_var = slope_est)

do_vel_slopes <- chopstick_slopes(model_vel_d, 
  x_variable = "squashed_DO_vel_scaled", 
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO") %>% 
  mutate(sort_var = slope_est) 

p_temp_vel_chops <- plot_fuzzy_chopsticks(model_vel_t ,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = "Predicted mature biomass vel", 
  slopes = temp_vel_slopes # if add, the global slope can be included for insig
) + coord_cartesian(ylim=c(-25,37)) +
  xlab("Temperature velocity (scaled)") + theme(legend.position = "none")
  
p_do_vel_chops <- plot_fuzzy_chopsticks(model_vel_d,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  y_label = "Predicted mature biomass vel", 
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim=c(-25,37)) +
  xlab("DO velocity (scaled)") + theme(legend.position = "none")

temp_vel_slopes$species[temp_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
do_vel_slopes$species[do_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

p_temp_all_vel_slopes <- plot_chopstick_slopes(temp_vel_slopes, 
  type = "temp", add_global = F, 
  legend_position = c(.25,.95)) + 
  ylab(" ") +  
  # coord_flip(ylim =c(-12,8)) 
  coord_flip(ylim =c(-14,7.85)) 

p_do_all_vel_slopes <- plot_chopstick_slopes(do_vel_slopes, 
  type = "DO", add_global = F,
  legend_position = c(.25,.95)) + 
  ylab("slopes") +  
  # coord_flip(ylim =c(-3,3.5))
  coord_flip(ylim =c(-5.75,3.25))
  

cowplot::plot_grid(p_temp_all_vel_slopes, p_temp_vel_chops, p_do_all_vel_slopes, p_do_vel_chops, ncol = 2, rel_widths = c(1, 2.5)) 
ggsave(here::here("ms", "figs", "supp-vel-chopsticks.pdf"), width = 14, height = 12)



#### COEFFICIENT SCATTERPLOTS
## If make-figs not run first...
# model2 <- add_colours(model$coefs, species_data = stats)
# model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
# model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
# model2 <- model2 %>% group_by(group) %>% mutate(spp_count = length(unique(species))) %>% ungroup()
# model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc=F))
# model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc=F))
# trendeffects <- model2 %>% filter(coefficient %in% c("temp_trend_scaled","DO_trend_scaled")) %>% transform(coefficient = factor(coefficient, levels = c("temp_trend_scaled","DO_trend_scaled"), labels = c("temperature", "DO")))
# trendeffects <- trendeffects %>% mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc=F))

p_depth <- coef_scatterplot(trendeffects, 
  coef = c("temperature","DO"), 
  x = "depth", group = "age", regression = T) + 
  ylab("trend coefficient") + xlab("mean depth") +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + labs(subtitle = "all species") +
  facet_grid(rows = vars(coefficient), scales = "free_y") + 
  # gfplot:::theme_pbs() %+replace% 
  theme(plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"), 
    strip.background = element_blank(), 
    strip.text.y = element_blank(), 
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    # axis.text.y = element_blank(), 
    axis.ticks = element_blank()) 

p_age <- coef_scatterplot(trendeffects, coef = c("temperature","DO"), 
  x = "age_max", group = "age", regression = T) + 
  xlab("maximum age")+
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + 
  # ggtitle("") +
  theme(plot.margin = margin(0, 0.25, 0.1, 0.1, "cm"), strip.background = element_blank(), 
    strip.text.y = element_blank(),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_grid(rows = vars(coefficient), cols = vars(rockfish), scales = "free_y") 

p_mat <- coef_scatterplot(trendeffects, coef = c("temperature","DO"), 
  x = "length_50_mat_f", group = "age", regression = T) + 
  xlab("length at maturity") +
  # geom_smooth(method = "lm", alpha = 0.15) +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  # guides(colour=F) +
  theme(plot.margin = margin(0, 0.1, 0.1, 0, "cm"), 
    strip.background = element_blank(), 
    legend.position = c(.75,.15), legend.title = element_blank(),
    # strip.text = element_blank(),
    axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  facet_grid(coefficient~rockfish, scales = "free_y") 

cowplot::plot_grid(p_depth, p_age, p_mat, nrow = 1, rel_widths = c(1.1, 1.75 , 1.75))
ggsave(here::here("ms", "figs", "supp-coef-scatterplots.pdf"), width = 6.5, height = 3)
# p_depth + p_age + p_mat + plot_layout(nrow = 1, widths = c(1.1, 1.75 , 1.75)) 


