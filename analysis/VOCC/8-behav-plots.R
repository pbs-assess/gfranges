


model <- readRDS("data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")

stats <- readRDS(paste0("data/life-history-behav.rds"))

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

long_slopes <- rbind(temp_slopes, do_slopes) %>% mutate(slope_type = paste(chopstick, type)) %>% ungroup()

long_slopes <- long_slopes %>% mutate(type = factor(type, levels = c("temp", "DO")),
  slope_type = factor(slope_type, levels = c("high temp", "low temp", "high DO", "low DO")),
  Diet = factor(Diet, levels = c("Generalist", "Zooplankton", "Polychaetes", "Crustaceans", "Fish")),
  Zone = factor(BenthoPelagicPelagicDemersal, levels = c("Demersal", "Benthopelagic", "Pelagic")),
  Latitude = factor(NorthMiddleSouth, levels = c("South", "Middle", "North"))
  )

# ggplot(long_slopes, aes(slope, Schooling, colour =slope_type, fill = slope_type )) + 
#   geom_violin( alpha = 0.1) + #scale = "width",
#   # geom_boxplot(alpha = 0.4) + 
#   scale_colour_brewer(palette = "Spectral", direction = -1) + scale_fill_brewer(palette = "Spectral", direction = -1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   gfplot:::theme_pbs() +
#   theme(
#     # plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"),
#     # legend.position = "none"
#     legend.position = c(.15, .15)
#   ) 



p1 <- ggplot(long_slopes, aes(Latitude, slope, colour =slope_type, fill = slope_type )) + 
  # geom_violin( alpha = 0.1) + #scale = "width",
  geom_hline(yintercept = 0, colour = "grey") +
  geom_boxplot(alpha = 0.6) +
  # scale_y_continuous(trans = fourth_root_power) +
  scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
  # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
  facet_grid(rows = vars(type), scales = "free") + 
  ggtitle("Range limits") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.y = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    # legend.position = "none"
    legend.title = element_blank(),
    legend.position = c(.25, .15)
  ) 

p2 <- ggplot(long_slopes, aes(Schooling, slope, colour =slope_type, fill = slope_type )) + 
  # geom_violin( alpha = 0.1) + #scale = "width",
  geom_hline(yintercept = 0, colour = "grey") +
  geom_boxplot(alpha = 0.6) +
  # scale_y_continuous(trans = fourth_root_power) +
  scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
  # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
  facet_grid(rows = vars(type), scales = "free") + 
  ggtitle("Sociality") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.y = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),

    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p3 <- ggplot(long_slopes, aes(Zone, slope, colour = slope_type, fill = slope_type )) + 
  # geom_violin( alpha = 0.1) + #scale = "width",
  geom_hline(yintercept = 0, colour = "grey") +
  geom_boxplot(alpha = 0.6) +
  # scale_y_continuous(trans = fourth_root_power) +
  scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
  # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
  facet_grid(rows = vars(type), scales = "free") + 
  ggtitle("Foraging zone") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.y = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p4 <- ggplot(long_slopes, aes(Diet, slope, colour =slope_type, fill = slope_type )) + 
  # geom_violin( alpha = 0.1) + #scale = "width",
  geom_hline(yintercept = 0, colour = "grey") +
  geom_boxplot(alpha = 0.6) +
  # scale_y_continuous(trans = fourth_root_power) +
  scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
  # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
  facet_grid(rows = vars(type), scales = "free") +
  ggtitle("Diet") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p1 + p2 + p3 + p4 + patchwork::plot_layout(nrow = 1, widths = c(1.1, 0.66, 1, 1.5))

ggsave(here::here("ms", "figs", "behav-slope-boxplots.pdf"), width = 12, height = 5)

