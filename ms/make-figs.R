setwd(here::here())

library(TMB)
library(tidyverse)
library(patchwork)
library(gfranges)

write_tex <- function(x, macro, ...) {
  paste0("\\newcommand{\\", macro, "}{", x, "}") %>%
    readr::write_lines("ms/values.tex", append = TRUE)
}

# load supplementary data
# stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav.rds"))
stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth.rds")) %>% mutate(age = firstup(age))
alldata <- readRDS(paste0("analysis/VOCC/data/all-newclim-untrimmed-dvocc-med.rds"))

# load appropriate final models
# model <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-06-25-trend-with-do-1-500.rds")
model <- readRDS("analysis/VOCC/data/trend-all-95-optimized2-08-01-trend-with-do-1-500.rds") # optimized
max(model$sdr$gradient.fixed)

# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-all-newclim-06-25-vel-both-1-350.rds") # un-optimized biomass
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-07-31-vel-both-1-350.rds") # not converged
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-01-vel-both-family-1-350.rds")
model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-01-vel-both-1-400.rds") # optimized and converges

max(model_vel$sdr$gradient.fixed)


## FILTER TO DEPTHS SAMPLED
# range(survey_sets$depth_m, na.rm = T) 
#   18 1308
# quantile(survey_sets$depth_m, c(0.005, 0.995), na.rm = T)
# 0.5% 99.5% 
#   23  1112 
# alldata2 <- alldata %>% filter(depth > 18) %>% filter(depth < 1308)
alldata <- alldata %>% filter(depth > 23) %>% filter(depth < 1112) # 99th
# alldata2 <- alldata %>% filter(depth > 31) %>% filter(depth < 523.8) # 95th

#### SAVE TEX VALUES FOR CLIMATE IQRs ####

paste0("% temperature range and change") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(quantile(alldata$mean_temp, 0.025), digits = 2), "lowTMean")
write_tex(signif(quantile(alldata$mean_temp, 0.975), digits = 2), "highTMean")
write_tex(signif(attributes(alldata$mean_temp_scaled)[[2]], digits = 2), "tempMeanSD")
write_tex(signif(mean(alldata$temp_trend), digits = 2), "meanTTrend")
write_tex(signif(quantile(alldata$temp_trend, 0.025), digits = 2), "lowTTrend")
write_tex(signif(quantile(alldata$temp_trend, 0.975), digits = 2), "highTTrend")
write_tex(signif(attributes(alldata$temp_trend_scaled)[[2]], digits = 2), "temptrendSD")

paste0("% temp change < 100 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
alldata100 <- alldata %>% filter(depth < 100)
write_tex(signif(mean(alldata100$temp_trend), digits = 2), "meanTTrendONE")

paste0("% DO range and change") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(quantile(alldata$mean_DO, 0.025), digits = 2), "lowDOMean")
write_tex(signif(quantile(alldata$mean_DO, 0.975), digits = 2), "highDOMean")
write_tex(signif(attributes(alldata$mean_DO_scaled)[[2]], digits = 2), "DOmeanSD")
write_tex(signif(mean(alldata$DO_trend), digits = 2), "meanDOTrend")
write_tex(signif(quantile(alldata$DO_trend, 0.025), digits = 2), "lowDOTrend")
write_tex(signif(quantile(alldata$DO_trend, 0.975), digits = 2), "highDOTrend")
write_tex(signif(attributes(alldata$DO_trend_scaled)[[2]], digits = 2), "DOtrendSD")

alldata50 <- alldata %>% filter(depth <= 50)
alldataDO <- alldata %>% filter(depth < 200) %>% filter(depth > 50)
alldata200 <- alldata %>% filter(depth >= 200) 

paste0("% temp change <= 50 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldata50$temp_trend), digits = 2), "meanTTrendONE")
write_tex(signif(quantile(alldata50$temp_trend, 0.025), digits = 2), "minTTrendONE")
write_tex(signif(quantile(alldata50$temp_trend, 0.975), digits = 2), "maxTTrendONE")
paste0("% temp change between 50 and 200 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldataDO$temp_trend), digits = 2), "meanTTrendTWO")
write_tex(signif(quantile(alldataDO$temp_trend, 0.025), digits = 2), "minTTrendTWO")
write_tex(signif(quantile(alldataDO$temp_trend, 0.975), digits = 2), "maxTTrendTWO")
paste0("% temp change >= 200 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldata200$temp_trend), digits = 2), "meanTTrendDEEP")
write_tex(signif(quantile(alldata200$temp_trend, 0.025), digits = 2), "minTTrendDEEP")
write_tex(signif(quantile(alldata200$temp_trend, 0.975), digits = 2), "maxTTrendDEEP")

paste0("% DO change <= 50 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldata50$DO_trend), digits = 2), "meanDOTrendONE")
write_tex(signif(quantile(alldata50$DO_trend, 0.025), digits = 2), "minDOTrendONE")
write_tex(signif(quantile(alldata50$DO_trend, 0.975), digits = 2), "maxDOTrendONE")
paste0("% DO change between 50 and 200 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldataDO$DO_trend), digits = 2), "meanDOTrendTWO")
write_tex(signif(quantile(alldataDO$DO_trend, 0.025), digits = 2), "minDOTrendTWO")
write_tex(signif(quantile(alldataDO$DO_trend, 0.975), digits = 2), "maxDOTrendTWO")
paste0("% DO change >= 200 m") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(mean(alldata200$DO_trend), digits = 2), "meanDOTrendDEEP")
write_tex(signif(quantile(alldata200$DO_trend, 0.025), digits = 2), "minTTrendDEEP")
write_tex(signif(quantile(alldata200$DO_trend, 0.975), digits = 2), "maxTTrendDEEP")

paste0("% temperature velocities") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(attributes(alldata$squashed_temp_vel_scaled)[[2]], digits = 2), "tempvelSD")
write_tex(signif(mean(alldata$squashed_temp_vel), digits = 2), "tempvelmean")
write_tex(signif(range(alldata$squashed_temp_vel)[[1]], digits = 2), "tempvelmin")
write_tex(signif(range(alldata$squashed_temp_vel)[[2]], digits = 2), "tempvelmax")
range_temp_vel <- range(alldata$squashed_temp_vel)[[2]]-range(alldata$squashed_temp_vel)[[1]]
midpoint_temp_vel <- range(alldata$squashed_temp_vel)[[2]]-range_temp_vel/2
write_tex(signif(midpoint_temp_vel, digits = 2), "tempvelmid")

paste0("% DO velocities") %>% readr::write_lines("ms/values.tex", append = TRUE)
write_tex(signif(attributes(alldata$squashed_DO_vel_scaled)[[2]], digits = 2), "DOvelSD")
write_tex(signif(
  mean(alldata$squashed_DO_vel_scaled)*attributes(alldata$squashed_DO_vel_scaled)[[2]],
  digits = 2), "DOvelmean")
write_tex(signif(
  range(alldata$squashed_DO_vel_scaled)[[1]]*attributes(alldata$squashed_DO_vel_scaled)[[2]],
  digits = 2), "DOvelmin")
write_tex(signif(
  range(alldata$squashed_DO_vel_scaled)[[2]]*attributes(alldata$squashed_DO_vel_scaled)[[2]],
  digits = 2), "DOvelmax")
range_DO_vel <- range(alldata$squashed_DO_vel_scaled)[[2]]*attributes(alldata$squashed_DO_vel_scaled)[[2]] -
  range(alldata$squashed_DO_vel_scaled)[[1]]*attributes(alldata$squashed_DO_vel_scaled)[[2]]
midpoint_DO_vel <- range(alldata$squashed_DO_vel_scaled)[[2]]*attributes(alldata$squashed_DO_vel_scaled)[[2]] -
  range_DO_vel/2
write_tex(signif(midpoint_DO_vel, digits = 2), "DOvelmid")

#########################
#########################
#### CLIMATE MAPS ####
# 
# # test filter of cells by observed DO and temp values 

# alldata <- alldata %>% filter(mean_DO > 0.23) %>% filter(mean_DO < 7.91) # full range
alldata <- alldata %>%
  filter(mean_DO > 0.28) %>%
  filter(mean_DO < 7.06) # 0.005 and 0.995
# alldata <- alldata2 %>% filter(mean_temp > 2.61) %>% filter(mean_temp < 14.31) # full range
alldata <- alldata %>%
  filter(mean_temp > 3.07) %>%
  filter(mean_temp < 11.3) # 0.005 and 0.995

(mean_do <- plot_vocc(alldata, # grey_water = T,
  vec_aes = NULL, grey_water = F,
  fill_col = "mean_DO", fill_label = "ml/L ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F, tag_text = "b.",
  viridis_begin = 0.2,
  # raster_limits = c(0.69, 5.24),
  legend_position = c(0.15, 0.3)
) + ggtitle("Dissolved oxygen (DO)") +
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  ))

(mean_temp <- plot_vocc(alldata,
  vec_aes = NULL, grey_water = F,
  fill_col = "mean_temp", fill_label = "ºC ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  viridis_option = "B",
  viridis_begin = 0.15,
  viridis_end = 0.7,
  axis_lables = T, tag_text = "a.",
  legend_position = c(0.15, 0.3)
) + ggtitle("Temperature") +
  ylab("Mean conditions") +
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  ))

(trend_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "temp_trend", fill_label = "ºC ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "ghostwhite", grey_water = F,
  mid_fill = "mistyrose1", grey_water = F,
  low_fill = "royalblue4", # low_fill = "#5E4FA2",
  high_fill = "Red 3",
  axis_lables = T, tag_text = "c.",
  legend_position = c(0.15, 0.3)
) + # ggtitle("temperature") +
  coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  ylab("Change per decade") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank()
  ))

(vel_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_temp_vel", fill_label = "km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "ghostwhite", grey_water = F,
  mid_fill = "mistyrose1", grey_water = F,
  # mid_fill = "lavenderblush1", grey_water = F,
  low_fill = "royalblue4", # low_fill = "#5E4FA2",
  high_fill = "Red 3",
  # transform_col = sqrt,
  axis_lables = T, tag_text = "e.",
  legend_position = c(0.15, 0.3)
) + ylab("Velocities per decade") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    ))

(trend_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "DO_trend", fill_label = "ml/L ",
  raster_cell_size = 4, white_zero = TRUE,
  # mid_fill = "lightcyan1", grey_water = F,
  # mid_fill = "aliceblue", grey_water = F,
  # mid_fill = "mintcream", grey_water = F,
  mid_fill = "honeydew", grey_water = F,
  # mid_fill = "lightyellow", grey_water = F,
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan", # "lightseagreen",
  # high_fill = "#3d95cc",
  # low_fill = "yellowgreen",
  # raster_limits = c(-3.5, 2),
  na_colour = "gold", 
  axis_lables = F, tag_text = "d.",
  legend_position = c(0.15, 0.3)
) + # ggtitle("dissolved oxygen") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))

(vel_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_DO_vel", fill_label = "km",
  raster_cell_size = 4,
  na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "lightcyan1", grey_water = F,
  mid_fill = "honeydew", grey_water = F,
  # mid_fill = "lightyellow", grey_water = F,
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan",
  # transform_col = sqrt,
  axis_lables = F, tag_text = "f.",
  legend_position = c(0.15, 0.3)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  ))

(depth_map <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "depth", fill_label = "m",
  raster_cell_size = 4, na_colour = "navyblue", #white_zero = TRUE,
  grey_water = F, 
  viridis_dir = -1, 
  raster_limits = c(20,800),
  transform_col = log10,
  axis_lables = T, tag_text = "e.",
  legend_position = c(0.15, 0.3)
) + ylab("Velocities per decade") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    ))

(dvocc_temp <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_temp_dvocc", fill_label = "km",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "ghostwhite", grey_water = F,
  mid_fill = "mistyrose1", grey_water = F,
  # mid_fill = "lavenderblush1", grey_water = F,
  low_fill = "royalblue4", # low_fill = "#5E4FA2",
  high_fill = "Red 3",
  axis_lables = T, tag_text = "g.",
  legend_position = c(0.15, 0.3)
) + ylab("Analog-distance velocities") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank()
    ))

(dvocc_do <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "squashed_DO_dvocc", fill_label = "km",
  raster_cell_size = 4,
  na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "lightcyan1", grey_water = F,
  mid_fill = "honeydew", grey_water = F,
  # mid_fill = "lightyellow", grey_water = F,
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan",
  axis_lables = F, tag_text = "h.",
  legend_position = c(0.15, 0.3)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))


(dvocc_both <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "dvocc_both", fill_label = "km",
  raster_cell_size = 4,
  na_colour = "lightgrey", white_zero = TRUE,
  # mid_fill = "lightcyan1", grey_water = F,
  mid_fill = "honeydew", grey_water = F,
  # mid_fill = "lightyellow", grey_water = F,
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan",
  axis_lables = F, tag_text = "h.",
  legend_position = c(0.15, 0.3)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))

mean_temp + mean_do + trend_temp + trend_do + vel_temp + vel_do + 
  plot_layout(ncol = 2)
# colorblindr::cvd_grid(trend_do)
# ggsave(here::here("ms", "figs", "climate-maps-newclim.png"), width = 5, height = 7.5)
ggsave(here::here("ms", "figs", "climate-maps-newclim-trimmed-99.png"), width = 5, height = 7.5)

mean_temp + mean_do + trend_temp + trend_do + vel_temp + vel_do + 
  dvocc_temp + dvocc_do +
  plot_layout(ncol = 2)
ggsave(here::here("ms", "figs", "climate-maps-w-dvocc.png"), width = 5, height = 9.5)



#########################
#########################
#### CLIMATE BY DEPTH
### TEMP

(p_depth_tf <- ggplot(alldata, aes(depth, mean_temp, colour = mean_temp)) +
    scale_color_viridis_c( option = "B", end = 0.8 ) +
    geom_point(alpha = 0.5, shape = 20#, size = 0.432
    ) +
    geom_smooth(colour = "black", size = 0.5, se = F) +
    ylab("Mean (ºC)") +
    scale_x_reverse() +
    xlim(450, 15) +
    ylim(4, 11) +
    coord_flip(expand = F) +  
    xlab("Mean cell depth") +
    ggtitle("Temperature") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), 
      axis.title.x = element_blank(), 
      legend.position = "none"
    ))

(p_depth_tt <- ggplot(alldata, aes(depth, temp_trend, colour = temp_trend)) +
    scale_color_viridis_c( option = "B", end = 0.8 ) +
    geom_point(alpha = 0.5, shape = 20) +
    # geom_smooth(colour = "black", size = 0.5, se = F) +
    # scale_y_continuous(position = "right") +
    geom_hline(yintercept = 0, colour = "grey60", linetype = "solid") + # 
    scale_x_reverse() +
    coord_flip(xlim = c(450, 15), expand = F) +
    # coord_cartesian(xlim = c(15, 450), ylim = c(0.2, 7.5), expand = F) + # ylim = c(0, 11.5),
    # ylab("Temperature trend (ºC per decade)") +
    ylab("Trend per decade") +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.y = element_blank(), axis.title.y = element_blank()
    ))

(p_depth_tv <- ggplot(alldata, aes(depth, squashed_temp_vel, colour = squashed_temp_vel)) +
    scale_color_viridis_c( option = "B", begin = 0.25, end = 0.9 
    ) +
    geom_point(alpha = 0.5, shape = 20) +
    geom_hline(yintercept = 0, colour = "grey60", linetype = "solid") + # 
    scale_x_reverse() +
    coord_flip(xlim = c(450, 15), expand = F) +
    # ylab("Temperature velocity (km per decade)") +
    ylab("Velocity (km per decade)") +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.y = element_blank(), axis.title.y = element_blank()
    ))

#### DO

(p_depth_dof <- ggplot(alldata, aes(depth, mean_DO, colour = mean_DO)) +
    scale_color_viridis_c(trans = sqrt, end = 1) +
    geom_point(alpha = 0.5, shape = 20#, size = 0.432
    ) +
    geom_smooth(colour = "black", size = 0.5, se = F) +
    ylab("Mean") +
    scale_x_reverse() +
    xlim(450, 15) +
    ylim(0, 6.4) +
    coord_flip(expand = F) +  
    geom_hline(yintercept = 6.4, colour = "grey", linetype = "dashed") + # 100% saturation at 1 bar, 10 degree C, ~35 salinity
    geom_hline(yintercept = 1.8, colour = "grey40", linetype = "dashed") + # 30% saturation onset for mild hypoxia
    # geom_hline(yintercept = 0.64, colour = "black", linetype = "dashed") + # 10% saturation onset for severe hypoxia
    xlab("Mean cell depth") +
    ggtitle("DO") +
    gfplot::theme_pbs() + theme(
      # axis.text.y = element_blank(), axis.title.y = element_blank(),
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none"
    ))

(p_depth_dot <- ggplot(alldata, aes(depth, DO_trend, colour = DO_trend)) +
    scale_color_viridis_c(trans = sqrt, end = 1) +
    geom_point(alpha = 0.5, shape = 20) +
    # geom_smooth(colour = "black", size = 0.5, se = F) +
    # scale_y_continuous(position = "right") +
    geom_hline(yintercept = 0, colour = "grey60", linetype = "solid") + # 
    scale_x_reverse() +
    coord_flip(xlim = c(450, 15), expand = F) +
    # coord_cartesian(xlim = c(15, 450), ylim = c(0.2, 7.5), expand = F) + # ylim = c(0, 11.5),
    ylab("Trend per decade") +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none",
      # axis.title.x = element_blank()
      axis.text.y = element_blank(), axis.title.y = element_blank()
    ))

(p_depth_dov <- ggplot(alldata, aes(depth, squashed_DO_vel, colour = squashed_DO_vel)) +
    scale_color_viridis_c(trans = sqrt, end = 1) +
    geom_point(alpha = 0.5, shape = 20) +
    geom_hline(yintercept = 0, colour = "grey60", linetype = "solid") + # 
    scale_x_reverse() +
    coord_flip(xlim = c(450, 15), expand = F) +
    # coord_cartesian(xlim = c(15, 450), ylim = c(0.2, 7.5), expand = F) + # ylim = c(0, 11.5),
    ylab("Velocity (km)") +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none",
      # axis.title.x = element_blank()
      axis.text.y = element_blank(), axis.title.y = element_blank()
    ))

(p_iqr <- temp_slopes %>% filter(chopstick == "high") %>% 
    mutate(labs = if_else(age == "Mature" & depth > 300 | depth_iqr < 25 | (age == "Mature" & depth_iqr > 100),  gsub(" Rockfish", "", species), NA_character_)) %>%
    ggplot(aes(depth, depth_iqr)) + 
    # geom_smooth(method = "lm", colour = "black", size =0.5) +
    geom_line(aes(group = species), colour = "grey60") +
    geom_point(aes(shape = age, colour = age), fill = "white", size = 1.5) +
    scale_colour_manual(values = c("deepskyblue3", "royalblue4")) +
    # scale_colour_manual(values = c("royalblue4","deepskyblue3" )) +
    # scale_fill_manual(values = c("cornflowerblue", "deepskyblue")) +
    # scale_colour_manual(values = c("royalblue4", "darkorchid4")) +
    # scale_fill_manual(values = c("royalblue4", "darkorchid4")) + 
    scale_shape_manual(values = c(21, 19)) +
    coord_cartesian(xlim = c(15, 450), ylim = c(2, 240), expand = F) +
    # scale_x_log10() +
    # scale_y_log10() +
    scale_x_reverse() +
    coord_flip() +
    ggrepel::geom_text_repel(
      aes(label = labs), colour = "royalblue4",
      point.padding = 0.3, segment.colour = "deepskyblue3", max.iter = 10000,
      size = 3, force = 20,
      nudge_y = 2, nudge_x = -2,
      na.rm = T, min.segment.length = 10, seed = 1000
    ) +
    ggtitle("Groundfish species") +
    ylab("Depth range occupied (IQR)") +
    xlab("Mean depth occupied") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0, 0, 0.3, "cm"),
      legend.position = c(0.8, 0.8),
      # axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      legend.title = element_blank()
    ))  


# p_depth_tf + p_depth_tt + p_depth_tv +
# p_depth_dof + p_depth_dot + p_depth_dov + plot_layout(nrow = 1)
# 
# ggsave(here::here("ms", "figs", "climate-depth-plots.png"), width = 11, height = 4)
layout1 <- "
      AAA
      AAA
      BCD
      EFG
      "

p_iqr +
  p_depth_tf + p_depth_tt + p_depth_tv +
  p_depth_dof + p_depth_dot + p_depth_dov + 
  plot_layout(design = layout1)

ggsave(here::here("ms", "figs", "climate-depth-plots-stacked.png"), width = 6, height = 8.5)

#######################
#######################
#### FISHING EFFORT MAPS ####
(mean_fish <- plot_vocc(alldata,
  vec_aes = NULL, grey_water = F,
  fill_col = "mean_effort", fill_label = "Hrs/yr",
  raster_cell_size = 4, white_zero = F,
  viridis_option = "B",
  # viridis_begin = 0.1,
  # viridis_end = 0.7,
  # # viridis_dir = -1,
  na_colour = "yellow",
  # raster_limits = c(0, 300),
  transform_col = log10,
  axis_lables = T, tag_text = "a.",
  legend_position = c(0.15, 0.3)
) + ggtitle("Fishing effort") +
    # ylab("Mean conditions") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ) #+ scale_fill_gradientn(
    #   colours = c("royalblue3", "royalblue3", "Red3", "Red2"),
    #   na.value = "red",
    #   trans = fourth_root_power,
    #   limits = c(0, 100)
    # )
  )

(trend_fish <- plot_vocc(alldata,
  vec_aes = NULL,
  fill_col = "fishing_trend", fill_label = "% ",
  raster_cell_size = 4, na_colour = "midnightblue", white_zero = TRUE,
  # mid_fill = "ghostwhite", grey_water = F,
  mid_fill = "ghostwhite", grey_water = F,
  low_fill = "midnightblue",#"royalblue4", # low_fill = "#5E4FA2",
  high_fill = "orangered",
  axis_lables = T, tag_text = "b.",
  raster_limits = c(-6,6.7),
  legend_position = c(0.15, 0.3)
) +  #ggtitle("Fishing") +
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    ylab("Change per decade") +
    theme(
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ))

mean_fish + trend_fish + plot_layout(nrow  = 2)
# ggsave(here::here("ms", "figs", "fishing-maps.png"), width = 3, height = 6)
ggsave(here::here("ms", "figs", "fishing-maps-trimmed-99.png"), width = 3, height = 6)


## attempt at fishing velocity but not very interesting
# fishing <- readRDS("fishig-velocity.rds")
# fishing$squashed <- collapse_outliers(fishing$velocity, c(0, 0.985))
# (vel_fishing <- plot_vocc(fishing,
#   vec_aes = NULL,
#   fill_col = "velocity", fill_label = "km",
#   raster_cell_size = 4, 
#   na_colour = "lightgrey", white_zero = TRUE,
#   # mid_fill = "ghostwhite", grey_water = F,
#   mid_fill = "mistyrose1", grey_water = F,
#   # mid_fill = "lavenderblush1", grey_water = F,
#   low_fill = "royalblue4", # low_fill = "#5E4FA2",
#   high_fill = "Red 3",
#   axis_lables = T, tag_text = "e.",
#   legend_position = c(0.15, 0.3)
# ) + ylab("Velocities per decade") +
#     # coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
#     theme(
#       plot.margin = margin(0, 0, 0, 0, "cm"),
#       axis.text = element_blank(), axis.ticks = element_blank(),
#       axis.title.x = element_blank(), axis.title.y = element_blank()
#     ))

#########################
#########################
#### GLOBAL COEFS ####
#########################

# check change in violins with age effect
# model <- readRDS("analysis/VOCC/data/trend-all-95-newclim-more2016-06-21-trend-1-500.rds") # without WCHG, no taxonic groups
# null01 <- readRDS("analysis/VOCC/data/trend-all-95-newclim-more2016-06-22-trend-w-age-1-500-DO.rds")

#### get trend model betas and save tex ####
coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_t$type <- "True trend"

# renamed version
coef_names <- c(
  "intercept", "change in T", "mean T", "change in DO", "mean DO",
  "biomass", "interaction (T)", "interaction (DO)"
)
overall_betas <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas$model <- "Trend"

# # ### SAVE TEX VALUES FOR TEMPERATURE TREND ESTIMATES ####
# paste0("% trend model betas") %>% readr::write_lines("ms/values.tex", append = TRUE)
# write_tex(signif(overall_betas$betas[overall_betas$coef_names == "change in T"], 2), "betaTtrend")
# write_tex(signif(abs(overall_betas$betas[overall_betas$coef_names == "change in T"]), 2), "ABSbetaTtrend")
# write_tex(signif(overall_betas$lowerCI[overall_betas$coef_names == "change in T"], 2), "lowerTTbeta")
# write_tex(signif(overall_betas$upperCI[overall_betas$coef_names == "change in T"], 2), "upperTTbeta")
# write_tex(signif(overall_betas$betas[overall_betas$coef_names == "interaction (T)"], 2), "betaTTinteract")
# write_tex(signif(abs(overall_betas$betas[overall_betas$coef_names == "interaction (T)"]), 2), "ABSbetaTTinteract")
# write_tex(signif(overall_betas$lowerCI[overall_betas$coef_names == "interaction (T)"], 2), "lowerTTinteract")
# write_tex(signif(overall_betas$upperCI[overall_betas$coef_names == "interaction (T)"], 2), "upperTTinteract")

# write_tex(signif(overall_betas$betas[overall_betas$coef_names == "change in DO"], 2), "betaDOtrend")
# write_tex(signif(overall_betas$lowerCI[overall_betas$coef_names == "change in DO"], 2), "lowerDTbeta")
# write_tex(signif(overall_betas$upperCI[overall_betas$coef_names == "change in DO"], 2), "upperDTbeta")
# write_tex(signif(overall_betas$betas[overall_betas$coef_names == "interaction (DO)"], 2), "betaDTinteract")
# write_tex(signif(overall_betas$lowerCI[overall_betas$coef_names == "interaction (DO)"], 2), "lowerDTinteract")
# write_tex(signif(overall_betas$upperCI[overall_betas$coef_names == "interaction (DO)"], 2), "upperDTinteract")


#### get velocity model betas and save tex ####
coef_names <- shortener(unique(model_vel$coefs$coefficient))
betas <- signif(as.list(model_vel$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_v <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_v$type <- "True velocity"

# renamed version
coef_names <- c(
  "intercept", "change in T", "change in DO", "mean T", "mean DO",
  "biomass", "interaction (T)", "interaction (DO)"
)
overall_betas_vel <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel$model <- "Velocity"
###
# # ### SAVE TEX VALUES FOR TEMPERATURE VELOCITY ESTIMATES ####
# paste0("% velocity model betas") %>% readr::write_lines("ms/values.tex", append = TRUE)
# write_tex(signif(overall_betas_vel$betas[overall_betas_vel$coef_names == "change in T"], 2), "betaTvel")
# write_tex(signif(abs(overall_betas_vel$betas[overall_betas_vel$coef_names == "change in T"]), 2), "ABSbetaTvel")
# write_tex(signif(overall_betas_vel$lowerCI[overall_betas_vel$coef_names == "change in T"], 2), "lowerTVbeta")
# write_tex(signif(overall_betas_vel$upperCI[overall_betas_vel$coef_names == "change in T"], 2), "upperTVbeta")
# 
# write_tex(signif(overall_betas_vel$betas[overall_betas_vel$coef_names == "interaction (T)"], 3), "betaTVinteract")
# write_tex(signif(abs(overall_betas_vel$betas[overall_betas_vel$coef_names == "interaction (T)"]), 3), "ABSbetaTVinteract")
# write_tex(signif(overall_betas_vel$lowerCI[overall_betas_vel$coef_names == "interaction (T)"], 3), "lowerTVinteract")
# write_tex(signif(overall_betas_vel$upperCI[overall_betas_vel$coef_names == "interaction (T)"], 3), "upperTVinteract")

write_tex(signif(overall_betas_vel$betas[overall_betas_vel$coef_names == "change in DO"], 2), "betaDOvel")
write_tex(signif(overall_betas_vel$lowerCI[overall_betas_vel$coef_names == "change in DO"], 2), "lowerDVbeta")
write_tex(signif(overall_betas_vel$upperCI[overall_betas_vel$coef_names == "change in DO"], 2), "upperDVbeta")
write_tex(signif(overall_betas_vel$betas[overall_betas_vel$coef_names == "interaction (DO)"], 3), "betaDVinteract")
write_tex(signif(overall_betas_vel$lowerCI[overall_betas_vel$coef_names == "interaction (DO)"], 3), "lowerDVinteract")
write_tex(signif(overall_betas_vel$upperCI[overall_betas_vel$coef_names == "interaction (DO)"], 3), "upperDVinteract")


#### ADD NULLS AND MAKE VIOLIN PLOT ####
# 95-all-newclim is full dataset while 95-newclim-more2016 is former with WCHG missing

# # currently partial dataset nulls are standin
null01 <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-06-30-trend-with-do-sim-1-500.rds") # not converged 0.004
null02 <- readRDS("analysis/VOCC/data/trend-all-95-newclim-more2016-06-23-trend-with-do-sim-2-500-DO.rds")
null03 <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-07-01-trend-with-do-sim-3-500-DO.rds") 
null04 <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-06-30-trend-with-do-sim-4-500-DO.rds") # not converged 0.002
null05 <- readRDS("analysis/VOCC/data/trend-all-95-all-newclim-07-01-trend-with-do-sim-5-500-DO.rds")
max(null04$sdr$gradient.fixed)

# # max(null01$sdr$gradient.fixed)
vnull01 <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-03-vel-both-sim-1-400.rds")
vnull02 <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-13-vel-both-sim-6-400-DO.rds")
vnull03 <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-13-vel-both-sim-3-400-DO.rds")
# vnull04 <- readRDS("analysis/VOCC/data/vel-all-95-all-newclim-06-30-vel-both-sim-4-350.rds")
vnull05 <- readRDS("analysis/VOCC/data/vel-all-95-optimized2-08-12-vel-both-sim-5-400-DO.rds")
# # 
max(vnull01$sdr$gradient.fixed)
max(vnull02$sdr$gradient.fixed)
max(vnull03$sdr$gradient.fixed)
# max(vnull04$sdr$gradient.fixed)
max(vnull05$sdr$gradient.fixed)
# 
# # coef_names <- shortener(unique(vnull02$coefs$coefficient))
# # betas <- signif(as.list(vnull02$sdr, "Estimate")$b_j, digits = 3)
# # SE <- signif(as.list(vnull02$sdr, "Std. Error")$b_j, digits = 3)
# # lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
# # upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
# # overall_vnull2 <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)

# # # old nulls with incorrect climate data too
# # vnull01 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-1-200-DO.rds")
# vnull02 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-2-200-DO.rds")
# vnull03 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-3-200-DO.rds")
# # vnull04 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-4-200-DO.rds")
# # vnull05 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-30-vel-both-sim-5-200-DO.rds")

model$coefs$model <- "trend"
null01$coefs$model <- "null01"
null02$coefs$model <- "null02"
null03$coefs$model <- "null03"
null04$coefs$model <- "null04"
null05$coefs$model <- "null05"

model_vel$coefs$model <- "velocity"
vnull01$coefs$model <- "vnull01"
vnull02$coefs$model <- "vnull02"
vnull03$coefs$model <- "vnull03"
vnull04$coefs$model <- "vnull04"
vnull05$coefs$model <- "vnull05"


# custom_order <- c(
#   "Intercept", "log_biomass",
#   "temp", "temp_trend", "temp_vel", "temp_trend:temp", "temp_vel:temp",
#   "DO", "DO_trend", "DO_vel", "DO_trend:DO", "DO_vel:DO"
# )
custom_order <- c(
  "Intercept", "log_biomass",
  "temp", "temp_trend", "temp_vel", "temp_dvocc", "temp_trend:temp", "temp_vel:temp", "temp_dvocc:temp",
  "DO", "DO_trend", "DO_vel", "DO_dvocc", "DO_trend:DO", "DO_vel:DO", "DO_dvocc:DO"
)

nulls <- rbind(model$coefs, 
  null02$coefs, null03$coefs, null04$coefs, null05$coefs, 
  null01$coefs) %>%
  mutate(
    term = factor(shortener(coefficient),
      levels = as.character(custom_order)
    ), 
    std.error = `Std. Error`, 
    change_var = "trend",
    type = if_else(model == "trend", "Trend model", "Null models")
  ) %>%
  rename(estimate = Estimate)


(null_coefs <- ggplot(nulls, aes(estimate, term, colour = type)) +
  xlab("Coefficient estimate with 95% CI") + ylab("") +
  # geom_violin(scale = "width") +
  geom_violin( # aes(estimate, term), inherit.aes = F ,
    scale = "width",
    alpha = 0.1,  fill= NA, #"#D53E4F",
    data = filter(nulls, model == "trend")
  ) +
  # geom_violin(#aes(estimate, term), inherit.aes = F ,
  #   scale = "width", # scale = "area", # scale = "count",
  #   data = filter(nulls, model != "trend")) +
  # geom_violin(
  #   scale = "width", fill= NA,
  #   alpha = 0.1, data = filter(nulls, model == "null01")
  # ) +
  geom_violin(
    scale = "width", fill= NA,
    alpha = 0.1, data = filter(nulls, model == "null02")
  ) + # this one is strange in it's Mean T coefs...
  geom_violin(
    scale = "width",fill= NA,
    alpha = 0.1, data = filter(nulls, model == "null03")
  ) +
  geom_violin(
    scale = "width",fill= NA,
    alpha = 0.1, data = filter(nulls, model == "null04")
  ) +
  geom_violin(
    scale = "width", fill= NA,
    alpha = 0.1, data = filter(nulls, model == "null05")
  ) +
  scale_y_discrete(
    limits = rev(unique(sort(nulls$term))),
    labels = c(
      "DO interaction", "DO trend", "Mean DO",
      "T interaction", "T trend", "Mean T", "Biomass", "Intercept"
    )
  ) +
    # scale_fill_manual(name = "Model type", values = c("white", "#D53E4F")) +
    # guides(fill = F) +
    scale_colour_manual(name = "Model type", values = c("grey80", "#D53E4F")) +
  # scale_fill_manual(name = "Model type", values = c("#FDAE61", "#D53E4F")) + # guides(fill = F) +
  # scale_colour_manual(name = "Model type", values = c("#FDAE61", "#D53E4F")) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(betas, coef_names, xmin = lowerCI, xmax = upperCI),
    # size = 1.15, shape = "|", fatten = 6,
    size = 0.5, fatten = 1,
    inherit.aes = F,
    data = overall_t
  ) + 
  coord_cartesian(xlim = c(-4, 2.7)) +
  ggtitle("a. Trend-based models") +
  guides(color = guide_legend(reverse = TRUE)) +
  gfplot::theme_pbs() + theme(
    axis.title = element_blank(), # element_text(size = 10),
    legend.title = element_blank(),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.025, 0.018)
  ))

vnulls <- rbind(
  vnull02$coefs,
  vnull03$coefs,
  # vnull04$coefs, 
  vnull05$coefs,
  vnull01$coefs,
  model_vel$coefs) %>%
  mutate(
    term = factor(shortener(coefficient),
      levels = as.character(custom_order)
    ), std.error = `Std. Error`, change_var = "velocity",
    type = if_else(model == "velocity", "Velocity model", "Time-null velocities")
  ) %>%
  rename(estimate = Estimate)

(vnull_coefs <- ggplot(vnulls, aes(estimate, term,
  fill = type,
  colour = type
)) +
  xlab("Coefficient estimate with 95% CI") + ylab("") +
  geom_violin( # aes(estimate, term), inherit.aes = F ,
    scale = "width",
    alpha = 0.1, fill = NA, #"#5E4FA2",
    data = filter(vnulls, model == "velocity")
  ) +
  # geom_violin(#aes(estimate, term), inherit.aes = F ,
  #   scale = "width", # scale = "count", # scale = "area",
  #   alpha = 0.1, data = filter(vnulls, model != "velocity")) +
  geom_violin(
    scale = "width", fill= NA,
    alpha = 0.1, data = filter(vnulls, model == "vnull01")
  ) +
  # geom_violin(
  #   scale = "width",
  #   alpha = 0.1, data = filter(vnulls, model == "vnull02")
  # ) +
  geom_violin(
    scale = "width", fill= NA,
    alpha = 0.1, data = filter(vnulls, model == "vnull03")
  ) +
  # geom_violin(
  #   scale = "width", fill= NA,
  #   alpha = 0.1, data = filter(vnulls, model == "vnull04")
  # ) +
  geom_violin(
    scale = "width", fill= NA,
    alpha = 0.1, data = filter(vnulls, model == "vnull05")
  ) +
  scale_y_discrete(
    limits = rev(unique(sort(vnulls$term))),
    labels = c(
      "DO interaction", "DO velocity", "Mean DO",
      "T interaction", "T velocity", "Mean T", "Biomass", "Intercept"
    )
  ) +
  scale_fill_manual(name = "Model type", values = c("gray90", "#5E4FA2"), guide = F) + #"#FDAE61" yellow guides(fill = F) +
  scale_colour_manual(name = "Model type", values = c("gray80", "#5E4FA2")) + # "#ABDDA4",  green
  geom_vline(xintercept = 0, colour = "gray60") +
  geom_pointrange(aes(betas, coef_names, xmin = lowerCI, xmax = upperCI),
    size = 0.5, fatten = 1,
    #size = 1.05, shape = "|", fatten = 6,
    inherit.aes = F,
    data = overall_v
  ) +
  coord_cartesian(xlim = c(-8.5, 5.5)) +
  ggtitle("b. Velocity-based models") +
  guides(color = guide_legend(reverse = TRUE)) +
  gfplot::theme_pbs() + theme(
    axis.title = element_blank(), # element_text(size = 10),
    legend.title = element_blank(),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.025, 0.018)
  ))

(null_coefs | vnull_coefs) / grid::textGrob("Species-specific coefficient estimates", 
  just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

ggsave(here::here("ms", "figs", "violin-optimized-w-nulls.pdf"), width = 9, height = 4)
# ggsave(here::here("ms", "figs", "null-spp-violin-w-dvocc.pdf"), width = 9, height = 4)

#########################
#########################
#### SEPARATE WORM PLOTS
#########################
### WORM PLOTS OF SLOP ESTIMATES FROM TREND MODEL ####
lowerT <- overall_betas %>%
  filter(coef_names == "change in T") %>%
  select(lowerCI)
upperT <- overall_betas %>%
  filter(coef_names == "change in T") %>%
  select(upperCI)
lowerD <- overall_betas %>%
  filter(coef_names == "change in DO") %>%
  select(lowerCI)
upperD <- overall_betas %>%
  filter(coef_names == "change in DO") %>%
  select(upperCI)
estT <- overall_betas %>%
  filter(coef_names == "change in T") %>%
  select(betas)
estD <- overall_betas %>%
  filter(coef_names == "change in DO") %>%
  select(betas)

temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
) %>%  mutate(sort_var = -(all_global_slope))
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
) %>%  mutate(sort_var = -(all_global_slope))

temp_slopes <- left_join(temp_slopes, stats) #%>% ungroup () %>% mutate(chopstick = factor(chopstick, levels = c("high", "low")))
do_slopes <- left_join(do_slopes, stats)

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

(p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.25, .85),
  name_chop_type = F,
  add_grey_bars = T
) +  coord_flip() + 
  coord_flip(ylim = c(-10, 3)) +
  # annotate("rect", ymin = lowerT[[1]], ymax = upperT[[1]], xmin = -Inf, xmax = Inf, alpha=0.1, fill="black") +
  geom_hline(yintercept = estT[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()) +
  ggtitle("a. Temperature"))

(p_do_worm <- plot_chopstick_slopes(do_slopes,
  type = "DO",
  legend_position = c(.25, .95),
  name_chop_type = F,
  add_grey_bars = T
) + 
  coord_flip() +
  coord_flip(ylim = c(-2, 2.8)) +
  # annotate("rect", ymin = lowerD[[1]], ymax = upperD[[1]], xmin = -Inf, xmax = Inf, alpha=0.1, fill="black") +
  geom_hline(yintercept = estD[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  ggtitle("b. DO") +
  # ylab("slopes")
  scale_x_discrete(position = "top") +
  theme(
    # legend.position = "none",
    axis.title.x = element_blank()))

# colorblindr::cvd_grid(p_temp_worm)
# colorblindr::cvd_grid(p_do_worm)

(p_temp_worm | p_do_worm) / grid::textGrob("Slope of biomass trend with a SD change in climate", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

ggsave(here::here("ms", "figs", "worm-plot-trend-newclim.pdf"), width = 9, height = 6)

# meta-analytical coefficients? ... all span zero, but could include as appendix?

### WORM PLOTS OF SLOP ESTIMATES FROM VELOCITY MODELS  ####

lowervT <- overall_betas_vel %>%
  filter(coef_names == "change in T") %>%
  select(lowerCI)
uppervT <- overall_betas_vel %>%
  filter(coef_names == "change in T") %>%
  select(upperCI)
lowervD <- overall_betas_vel %>%
  filter(coef_names == "change in DO") %>%
  select(lowerCI)
uppervD <- overall_betas_vel %>%
  filter(coef_names == "change in DO") %>%
  select(upperCI)
estvT <- overall_betas_vel %>%
  filter(coef_names == "change in T") %>%
  select(betas)
estvD <- overall_betas_vel %>%
  filter(coef_names == "change in DO") %>%
  select(betas)


temp_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled",
  type = "temp"
) 
# temp_vel_slopes <- chopstick_slopes(model_vel,
#   x_variable = "squashed_temp_dvocc_scaled",
#   interaction_column = "squashed_temp_dvocc_scaled:mean_temp_scaled",
#   type = "temp"
# )

temp_vel_slopes <- temp_vel_slopes %>% 
  # mutate(sort_var = slope_est)
  mutate(sort_var = (diff))
  # mutate(sort_var = abs(diff))

temp_vel_slopes$species[temp_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
) 
# do_vel_slopes <- chopstick_slopes(model_vel,
#   x_variable = "squashed_DO_dvocc_scaled",
#   interaction_column = "squashed_DO_dvocc_scaled:mean_DO_scaled", type = "DO"
# )

do_vel_slopes <- do_vel_slopes %>%  
  # mutate(sort_var = slope_est)
  mutate(sort_var = (diff))

do_vel_slopes$species[do_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_worm2 <- plot_chopstick_slopes(temp_vel_slopes, type = "temp",
  add_grey_bars = T,
  legend_position = c(.25,.95)) + #coord_flip(ylim =c(-14,7.75)) +
  theme(axis.title = element_blank(),
    plot.margin = margin(0, 0.2, 0, 0.2, "cm"),
    legend.position = "none") + 
  ggtitle("a. Temperature")
p_do_worm2 <- plot_chopstick_slopes(do_vel_slopes, type = "DO",
  add_grey_bars = T,
  legend_position = c(.25,.95)) + #coord_flip(ylim =c(-4.5,3.25)) +
  ggtitle("b. Dissolved oxygen") +
  scale_x_discrete(position = "top") +
  # ylab("slopes")
  theme(axis.title = element_blank(),
    plot.margin = margin(0, 0.2, 0, 0.2, "cm"),
    legend.position = "none")

(((p_temp_worm2 | p_do_worm2)/ 
    grid::textGrob(expression(~Delta~"biotic velocity with a SD change in climate velocity"), 
      vjust = -0.25, hjust = 0.5) +
  plot_layout(height = c(10, 0.1))) + plot_layout(guides = 'collect') & theme(
  legend.text = element_text(size = 9),
  legend.position = "bottom",
  # legend.justification='left',
  # legend.position = c(0, 1),
  # legend.justification = c(0, 0),
  legend.text.align = 1,
  legend.margin=margin(t = 0.4, l= 0, r = 0.1, b=0, unit='cm'),
  # legend.margin = unit(1.5, "cm"),
  # legend.spacing.x = unit(.1, "cm"),
    legend.direction = "horizontal",
    legend.box = "horizontal"
)) 

ggsave(here::here("ms", "figs", "worm-plot-vel.pdf"), width = 7, height = 7)
# ggsave(here::here("ms", "figs", "worm-plot-dvocc-med-by-est.pdf"), width = 8, height = 6)

#### IF WE WANT PLOT OF BOTH TREND AND VELOCITY SLOPES TOGETHER ####

temp_slopes <- temp_slopes %>% mutate(sort_var = -slope_est)
do_slopes <- do_slopes %>% mutate(sort_var = -slope_est)

p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.25, .95),
  order_by_chops = c("high"),
  add_grey_bars = T
) + coord_flip() + 
  coord_flip(ylim = c(-7.6, 3)) +
  annotate("rect", ymin = lowerT[[1]], ymax = upperT[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  geom_hline(yintercept = estT[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  theme(
    plot.margin = margin(0, 0.2, 0.2, 0.2, "cm"),
    axis.title.x = element_blank()
  ) +
  ggtitle("a. Temperature") +
  xlab("% biomass change for a SD of climate change")

p_do_worm <- plot_chopstick_slopes(do_slopes,
  type = "DO",
  legend_position = c(.25, .95),
  order_by_chops = c("low"),
  add_grey_bars = T
) + coord_flip(ylim = c(-3.1, 1.7)) +
  annotate("rect", ymin = lowerD[[1]], ymax = upperD[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  geom_hline(yintercept = estD[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  ggtitle("b. DO") +
  scale_x_discrete(position = "top") + # ylab("slopes")
  theme(
    plot.margin = margin(0, 0, 0.2, 0, "cm"),
    axis.title.x = element_blank()
  )

p_temp_worm3 <- plot_chopstick_slopes(temp_vel_slopes,
  type = "temp",
  legend_position = c(.25, .95),
  add_grey_bars = T
) + #coord_flip(ylim = c(-10, 7.95)) +
  annotate("rect", ymin = lowervT[[1]], ymax = uppervT[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  geom_hline(yintercept = estvT[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  ggtitle("c.") +
  theme(
    plot.margin = margin(0.2, 0.2, 0, 0.2, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  xlab("Biotic velocity change for a SD change in climate velocity")
p_do_worm3 <- plot_chopstick_slopes(do_vel_slopes,
  type = "DO",
  legend_position = c(.25, .95),
  add_grey_bars = T
) + #coord_flip(ylim = c(-5.75, 3.25)) +
  geom_hline(yintercept = estvD[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  annotate("rect", ymin = lowervD[[1]], ymax = uppervD[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  scale_x_discrete(position = "top") +
  ggtitle("d.") +
  theme(
    plot.margin = margin(0.2, 0, 0, 0, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none"
  )


p_temp_worm2 <- p_temp_worm + xlab("Biomass trend with a SD change in climate")

(p_temp_worm2 | p_do_worm) / (p_temp_worm3 | p_do_worm3) + plot_layout(height = c(1, 1)) #+ plot_annotation(tag_levels = "a")

ggsave(here::here("ms", "figs", "worm-plot-both.pdf"), width = 8, height = 10)

#########################
#########################
#### SPECIES CHOPSTICK PLOTS AND MAPS FROM TREND MODEL ####

species_panels <- function(species, model, x_type,
                           trends = T,
                           biotic_lim = c(-20, 25), # currently only applied to
                           # biotic_lim = c(-40, 40), # currently only applied to 
                           chop_label = F,
                           leftmost = F,
                           alpha_range = c(0.9, 0.9)) {
  age <- unique(model$data[model$data$species == species, ]$age_class)

  if (trends) {
  biotic_map <- filter(model$data, species == !!species) %>% plot_vocc(
    fill_col = "biotic_trend", 
    fill_label = "%", 
    raster_limits = c(-6, 6),
    
    vec_aes = NULL,
    raster_cell_size = 4,
    na_colour = "red 3", white_zero = TRUE,
    high_fill = "darkcyan",
    # mid_fill = "honeydew", grey_water = F,
    # mid_fill = "lightyellow", grey_water = F,
    mid_fill = "lightcyan1", grey_water = F,
    # mid_fill = "azure", grey_water = F,
    low_fill = "Red 3", 
    axis_lables = T,
    legend_position = c(0.15, 0.25),
    make_square = F
  ) 
  } else {
    biotic_map <- filter(model$data, species == !!species) %>% plot_vocc(
      fill_col = "squashed_biotic_vel", 
      fill_label = "km per \ndecade", 
      # raster_limits = c(-6, 6),
      
      vec_aes = NULL,
      raster_cell_size = 4,
      na_colour = "red 3", white_zero = TRUE,
      high_fill = "darkcyan",
      # mid_fill = "honeydew", grey_water = F,
      # mid_fill = "lightyellow", grey_water = F,
      mid_fill = "lightcyan1", grey_water = F,
      # mid_fill = "azure", grey_water = F,
      low_fill = "Red 3", 
      axis_lables = T,
      legend_position = c(0.15, 0.3),
      make_square = F
    ) 
    
  }
  
  biotic_map <- biotic_map + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme(
      plot.title = element_text(vjust = 1),
      plot.margin = margin(1, 0, 0, 0, "cm"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

  if (!leftmost) {
    biotic_map <- biotic_map + theme(legend.position = "none")
  }

  if (age == "immature") {
    # biotic_map <- biotic_map + ggtitle(paste0(" ", age, "\n", shortener(species)))
    biotic_map <- biotic_map + ggtitle(paste0(" \n", shortener(species), " (", age, ")"))
  } else {
    biotic_map <- biotic_map + ggtitle(paste0(" \n", shortener(species)))
    # biotic_map <- biotic_map + ggtitle(paste0(" \n", shortener(species)))
  }

  if (x_type == "temp") {
    if(trends){
    temp_slopes <- chopstick_slopes(model,
      x_variable = "temp_trend_scaled",
      interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
    )

    single_chop <- plot_fuzzy_chopsticks(model,
      x_variable = "temp_trend_scaled", type = "temp", #y_label = "",
      line_size = 1,
      alpha_range = alpha_range,
      choose_species = stringr::str_replace(species, ".*mature ", ""),
      choose_age = gsub(" .*", "", species),
      slopes = temp_slopes # if add, the global slope can be included for insig.
    ) + 
      scale_x_continuous(labels = function(x) paste0(round(x * 0.6328, 1))) + # TODO: need to make SD always correct...
      coord_cartesian(xlim = c(-0.2 , 2.6), ylim = c(-3.5, 5.8)) +
      theme(
        plot.margin = margin(0, 0.2, 0.1, 0.1, "cm"),
        legend.position = "none",
        axis.title = element_blank()
      )

    climate_map <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL,
        fill_col = "temp_trend", fill_label = "ºC per \ndecade",
        raster_cell_size = 4, na_colour = "red 3", white_zero = TRUE,
        low_fill = "royalblue3", 
        mid_fill = "mistyrose1", grey_water = F,
        # mid_fill = "ghostwhite", grey_water = F,
        high_fill = "Red 3", 
        axis_lables = T,
        # raster_limits = c(-0.75, 1.8),
        raster_limits = c(-1, 2.3),
        legend_position = c(0.15, 0.3), make_square = F
      ) 

    climate_map2 <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL, grey_water = F,
        fill_col = "mean_temp", fill_label = "mean \nºC",
        raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
        viridis_option = "B",
        viridis_begin = 0.15,
        viridis_end = 0.7,
        raster_limits = c( 3.79, 9.85),
        axis_lables = T, make_square = F,
        legend_position = c(0.15, 0.3)
      ) 
    
    } else { # repeat above for velocities
      temp_slopes <- chopstick_slopes(model_vel,
        x_variable = "squashed_temp_vel_scaled",
        interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp"
      )
      
      single_chop <- plot_fuzzy_chopsticks(model_vel,
        x_variable = "squashed_temp_vel_scaled",
        type = "temp", #y_label = "",
        line_size = 1,
        alpha_range = alpha_range,
        choose_species = stringr::str_replace(species, ".*mature ", ""),
        choose_age = gsub(" .*", "", species),
        slopes = temp_slopes # if add, the global slope can be included for insig.
      ) + 
        # scale_x_continuous(labels = function(x) paste0(round(x * 0.6328, 1))) + # TODO: need to make SD always correct...
        # coord_cartesian(xlim = c(-0.2 , 2.6), ylim = c(-3.5, 5.8)) +
        coord_cartesian(ylim = biotic_lim) +
        theme(
          plot.margin = margin(0, 0.2, 0.1, 0.1, "cm"),
          legend.position = "none",
          axis.title = element_blank()
        )
      
      climate_map <- filter(model$data, species == !!species) %>%
        plot_vocc(
          fill_col = "squashed_temp_vel", fill_label = "km per \ndecade",
          raster_limits = c(-10, 85),
          vec_aes = NULL,
          raster_cell_size = 4, na_colour = "red 3", white_zero = TRUE,
          low_fill = "royalblue3", 
          mid_fill = "mistyrose1", grey_water = F,
          high_fill = "Red 3", 
          axis_lables = T,
          legend_position = c(0.15, 0.3), make_square = F
        ) 
      
      climate_map2 <- filter(model$data, species == !!species) %>%
        plot_vocc(
          vec_aes = NULL, grey_water = F,
          fill_col = "mean_temp", fill_label = "mean \nºC",
          raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
          viridis_option = "B",
          viridis_begin = 0.15,
          viridis_end = 0.7,
          # raster_limits = c(3.79, 9.85),
          axis_lables = T, make_square = F,
          legend_position = c(0.15, 0.3)
        ) 
    }
    
    if (chop_label) {
      climate_map <- climate_map + ggtitle("Temperature") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"),
          axis.title.y = element_blank())
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.y = element_blank())
    } else {
      climate_map <- climate_map + ggtitle("()") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"), 
          plot.title = element_text(colour = "white"),
          axis.title.y = element_blank(), 
          legend.position = "none")
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.y = element_blank(), 
          legend.position = "none")
    }
  }

  if (x_type == "DO") {
    if(trends){
    do_slopes <- chopstick_slopes(model,
      x_variable = "DO_trend_scaled",
      interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
    )

    single_chop <- plot_fuzzy_chopsticks(model,
      x_variable = "DO_trend_scaled", type = "DO", 
      line_size = 1, alpha_range = alpha_range,
      choose_species = stringr::str_replace(species, ".*mature ", ""),
      choose_age = gsub(" .*", "", species),
      slopes = do_slopes # if add, the global slope can be included for insig.
    ) + 
      scale_x_continuous(labels = function(x) paste0(round(x * 0.4093, 1))) +
      coord_cartesian(xlim = c(-4, 4), ylim = c(-3.5, 3.8)) + 
      theme(
        plot.margin = margin(0, 0.2, 0.1, 0.1, "cm"),
        legend.position = "none",
        axis.title = element_blank()
      )

    climate_map <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL,
        fill_col = "DO_trend", fill_label = "ml/L per \ndecade",
        raster_cell_size = 4, na_colour = "red 3", white_zero = TRUE,
        high_fill = "gold",
        # mid_fill = "lightyellow", grey_water = F,
        # mid_fill = "lightcyan1", grey_water = F,
        mid_fill = "honeydew", grey_water = F,
        # mid_fill = "azure", grey_water = F,
        low_fill = "darkcyan",
        axis_lables = T,
        raster_limits = c(-3.5, 2),
        # raster_limits = c(-1.6, 1.6),
        legend_position = c(0.15, 0.3), make_square = F
      ) 
    
    climate_map2 <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL, grey_water = F,
        fill_col = "mean_DO", fill_label = "mean \nDO",
        raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
        viridis_option = "D",
        viridis_begin = 0.2,
        axis_lables = T, 
        # raster_limits = c(0.69, 5.24),
        legend_position = c(0.15, 0.3), make_square = F
      ) 
    } else { # for velocities
      do_slopes <- chopstick_slopes(model_vel,
        x_variable = "squashed_DO_vel_scaled",
        interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
      )
      
      single_chop <- plot_fuzzy_chopsticks(model_vel,
        x_variable = "squashed_DO_vel_scaled", type = "DO", 
        line_size = 1, alpha_range = alpha_range,
        choose_species = stringr::str_replace(species, ".*mature ", ""),
        choose_age = gsub(" .*", "", species),
        slopes = do_slopes # if add, the global slope can be included for insig.
      ) + 
        # scale_x_continuous(labels = function(x) paste0(round(x * 0.4093, 1))) +
        # coord_cartesian(xlim = c(-4, 4), ylim = c(-3.5, 3.8)) + 
        coord_cartesian(ylim = biotic_lim) + 
        theme(
          plot.margin = margin(0, 0.2, 0.1, 0.1, "cm"),
          legend.position = "none",
          axis.title = element_blank()
        )
      
      climate_map <- filter(model$data, species == !!species) %>%
        plot_vocc(
          fill_col = "squashed_DO_vel", fill_label = "km per \ndecade",
          # raster_limits = c(-3.5, 2),
          vec_aes = NULL, grey_water = F,
          raster_cell_size = 4, na_colour = "red 3", white_zero = TRUE,
          high_fill = "gold",
          mid_fill = "honeydew", 
          low_fill = "darkcyan",
          axis_lables = T, 
          legend_position = c(0.15, 0.3), make_square = F
        ) 
      
      climate_map2 <- filter(model$data, species == !!species) %>%
        plot_vocc(
          vec_aes = NULL, grey_water = F,
          fill_col = "mean_DO", fill_label = "mean \nDO",
          raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
          viridis_option = "D",
          viridis_begin = 0.2,
          axis_lables = T, 
          # raster_limits = c(0, 5.24),
          legend_position = c(0.15, 0.3), make_square = F
        ) 
    }
    if (chop_label) {
      climate_map <- climate_map + ggtitle("Dissolved oxygen") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"),
          axis.title.y = element_blank())
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.y = element_blank())
    } else {
      climate_map <- climate_map + ggtitle("()") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"), 
          plot.title = element_text(colour = "white"),
          axis.title.y = element_blank(), 
          legend.position = "none")
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.y = element_blank(), 
        legend.position = "none")
    }
  }

  climate_map <- climate_map + 
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme( 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank()
    )
  
  climate_map2 <- climate_map2 + 
    coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
    theme( 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank()
    )
  
  if (!leftmost) {
    single_chop <- single_chop + theme(axis.text.y = element_blank())
  }
  
  (biotic_map / single_chop / climate_map/ climate_map2 + plot_layout(heights = c(2, 0.5, 2, 2)))
  # ggsave(here::here("ms", "figs", paste0("panels-", x_type, "-", species, ".pdf")), width = 4, height = 10)
}

# velocity-based example chops
(p1 <- species_panels("mature Redbanded Rockfish", 
  trends = F,
  chop_label = T, 
  leftmost = T,
  model, "temp"
))

(p2 <- species_panels("mature Sablefish", 
  trends = F,
  # chop_label = T, 
  # leftmost = T, alpha_range = c(0.1, 0.9),
  model, "temp"
))

(p3 <- species_panels("mature Lingcod", 
  trends = F,
  chop_label = T, #leftmost = T, 
  model, "DO"))

(p4 <- species_panels("mature Flathead Sole", 
  trends = F,
  # chop_label = T, 
  model, "DO"))

layout <- "
      ADEFG
      BDEFG
      BDEFG
      CDEFG
      "

ygrob1 <- grid::textGrob((expression("Biotic velocity ("~italic("Y")~")")),
  gp = grid::gpar(fontsize = 12),
  hjust = 1, 
  vjust = 0.85,
  rot = 90
)

ygrob2 <- grid::textGrob((expression("Climate velocity ("~italic("x")~")")),
  gp = grid::gpar(fontsize = 12), hjust = 1, 
  vjust = 0.85, 
  rot = 90
)

ygrob3 <- grid::textGrob(("Mean climate"),
  gp = grid::gpar(fontsize = 12), hjust = 0.25, 
  vjust = 0.85, 
  rot = 90
)

wrap_plots(ygrob1, ygrob2, ygrob3, p1, p2, p3, p4) + plot_layout(design = layout, widths = c(0.05, 1, 1, 1, 1))

# ggsave(here::here("ms", "figs", "species-map-panels-vel.pdf"), width = 11, height = 11)
ggsave(here::here("ms", "figs", "species-map-panels-vel.png"), width = 11, height = 11)


# two panels only

(p1b <- species_panels("mature Redbanded Rockfish", 
  trends = F,
  chop_label = T, 
  leftmost = T,
  # biotic_lim = c(-20, 10),
  model, "temp"
))

(p2b <- species_panels("mature Flathead Sole", 
  trends = F,
  chop_label = T, #leftmost = T, 
  # biotic_lim = c(-20, 10),
  model, "DO"))

(p1c <- species_panels("mature Lingcod", 
  trends = F,
  chop_label = T, leftmost = T, 
  biotic_lim = c(-20, 10),
  model, "temp"))

(p2c <- species_panels("mature Lingcod", 
  trends = F,
  chop_label = T, #leftmost = T, 
  biotic_lim = c(-20, 10),
  model, "DO"))



layout <- "
      ADE
      BDE
      BDE
      CDE
      "
wrap_plots(ygrob1, ygrob2, ygrob3, p1b, p2b) + plot_layout(design = layout, widths = c(0.03, 1, 1))

# ggsave(here::here("ms", "figs", "species-map-panels-vel.pdf"), width = 11, height = 11)
ggsave(here::here("ms", "figs", "species-map-panels-vel2.png"), width = 6, height = 11)

wrap_plots(ygrob1, ygrob2, ygrob3, p1c, p2c) + plot_layout(design = layout, widths = c(0.03, 1, 1))

ggsave(here::here("ms", "figs", "species-map-panels-vel-lingcod.png"), width = 6, height = 11)

### trend-based example chops ####
# 
# (p1 <- species_panels("mature Canary Rockfish", model, "temp"))
# (p2b <- species_panels("immature Shortspine Thornyhead", model, "temp"))
# (p3 <- species_panels("mature Flathead Sole", model, "temp" #, alpha_range = c(0.1, 0.9)))
# 
# ygrob1 <- grid::textGrob(("Biotic trend (Y)"),
#   gp = grid::gpar(fontsize = 12),
#   hjust = 0.4, vjust = 0.85,
#   rot = 90
# )
# 
# wrap_plots(ygrob1, ygrob2, ygrob3, p1, p2b, p3, ...) + plot_layout(design = layout, widths = c(0.05, 1, 1, 1, 1))
# 
# ggsave(here::here("ms", "figs", "species-map-panels.pdf"), width = 17, height = 11)
# 
# ###
#### OTHER SPECIES PANEL OPTIONS ####
# species_panels("mature Arrowtooth Flounder", model, "temp")
# species_panels("immature Arrowtooth Flounder", model, "temp")
# species_panels("mature Curlfin Sole", model, "temp")
# species_panels("mature Flathead Sole", model, "temp")
# species_panels("immature Flathead Sole", model, "temp")
# species_panels ("mature Dover Sole", model, "temp")
# species_panels ("immature Dover Sole", model, "temp")
# species_panels("mature English Sole", model, "temp")
# species_panels("mature Pacific Halibut", model, "temp")
# species_panels("immature English Sole", model, "temp", alpha_range = c(0.25, 0.9))
# species_panels("mature Pacific Cod", model, "temp")
# species_panels("mature Walleye Pollock", model, "temp")
# species_panels("mature Canary Rockfish", model, "temp", alpha_range = c(0.25, 0.9))
# species_panels("mature Widow Rockfish", model, "temp")
# species_panels("mature Bocaccio", model, "temp")
# species_panels("mature Shortspine Thornyhead", model, "temp")

# species_panels("mature Sablefish", model, "DO")
# species_panels("mature Pacific Cod", model, "DO")
# species_panels("mature Canary Rockfish", model, "DO")
# species_panels("mature Yelloweye Rockfish", model, "DO")
# species_panels("mature Bocaccio", model, "DO")
# species_panels("mature Pacific Ocean Perch", model, "DO")
# species_panels("immature Quillback Rockfish", model, "DO")
# species_panels("mature Dover Sole", model, "DO")
# species_panels("immature Lingcod", model, "DO")
# species_panels("mature Redbanded Rockfish", model, "DO")
# species_panels("mature Widow Rockfish", model, "DO")
# species_panels("mature Petrale Sole", model, "DO")
# species_panels("mature Pacific Halibut", model, "DO")
# species_panels("mature English Sole", model, "DO")
# species_panels("immature Flathead Sole", model, "DO")
# species_panels("immature Arrowtooth Flounder", model, "DO")


#########################
#########################
#### DEPTH SCATTERPLOTS ####
### prep data ####

# do_data1 <- readRDS(paste0("analysis/VOCC/data/predicted-DO-june-2020.rds")) %>%
temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)


temp_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled",
  type = "temp"
) 

do_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
) 

temp_slopes <- left_join(temp_slopes, stats)
temp_vel_slopes2 <- left_join(temp_vel_slopes, stats)
temp_slopes <- temp_slopes %>% mutate(sort_var = slope_est)
temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_slopes <- left_join(do_slopes, stats)
do_vel_slopes2 <- left_join(do_vel_slopes, stats)
do_slopes <- do_slopes %>% mutate(sort_var = slope_est)
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"


# # apply labels to deep species and outliers
# do_slopes <- mutate(do_slopes, species_lab = if_else(slope_est < -0.75|depth >270, species, ""))
# temp_slopes <- mutate(temp_slopes, species_lab = if_else(slope_est < -2.25|depth >270, species, ""))

# or just to outliers
do_slopes <- mutate(do_slopes, species_lab = if_else(slope_est > 3|slope_est < -3, paste(age,species), ""))
temp_slopes <- mutate(temp_slopes, species_lab = if_else(slope_est < -6, paste(age,species), ""))


do_slopes$species_lab <- gsub("Rockfish", "", do_slopes$species_lab)
temp_slopes$species_lab <- gsub("Rockfish", "", temp_slopes$species_lab)

do_slopes$species_lab <- gsub("Rockfish", "", do_slopes$species_lab)
temp_slopes$species_lab <- gsub("Rockfish", "", temp_slopes$species_lab)

##### combined temp-DO panel 
# depth <- ggplot(do_data, aes(depth, do_est)) +
#   geom_point(aes(depth, temp*(2/3)), alpha = 0.02, shape = 20, colour = "#3d95cc", size = 0.432) +
#   geom_point(aes(depth, do_est), alpha = 0.02, shape = 20, colour = "darkorchid4", size = 0.432) +
#   geom_smooth(colour = "darkorchid4", size = 0.5) +
#   coord_cartesian(ylim = c(0, 8.2), xlim = c(15, 410), expand =F) +
#   ylab("Mean DO (ml/L)") +
#   scale_y_continuous(sec.axis = sec_axis( ~ (.*3/2), name = "Temperature (ºC)")) +  #, expand = expand_scale(mult = c(0.05, .2)
#   geom_smooth(aes(depth, temp*(2/3)), inherit.aes = F, colour = "#3d95cc", size = 0.5) +
#   geom_hline(yintercept = 1.4, colour = "black", linetype = "dashed") +
#   xlab("Mean depth") +
#   gfplot::theme_pbs() + theme(
#     plot.margin = margin(0, 0.3, 0.2, 0.2, "cm"),
#     axis.text.y.left = element_text(color = "darkorchid4"),
#     axis.text.y.right = element_text(color = "#3d95cc"),
#     axis.title.y.left = element_text(color = "darkorchid4", vjust = 0.2),
#     axis.title.y.right = element_text(color = "#3d95cc") #, vjust = -0.2
#     # axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
#   )

#### SLOPES AGAINST DEPTH ####
(temp_high <- slope_scatterplot(
  filter(temp_slopes, chopstick == "high"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5
) +
    # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
    geom_hline(yintercept = 0, colour = "gray") +
    geom_point(size = 1, fill = "white") +
    # ggrepel::geom_text_repel(aes(label = species_lab),
    #   size = 2,
    #   force = 3,
    #   nudge_y = 1.5,
    #   # nudge_x = 15,
    #   na.rm = T, min.segment.length = 4
    # ) +
    ylab(expression(~Delta~"% change in biomass with SD change in T")) +
    scale_y_continuous(breaks = c(3, 0, -3, -6, -9)) +
    # coord_cartesian( xlim = c(15, 450)) + #ylim = c(-11, 5.5),
    coord_cartesian(expand = F,
      # ylim = c(-6.5, 3),
      xlim = c(15, 450)) +
    scale_colour_manual(values = c("Red 3", "Red 3")) +
    # scale_colour_manual(values = c("#D53E4F","#D53E4F")) +
    theme(
      plot.margin = margin(0, 0.1, 0, 0.2, "cm"),
      # legend.position = c(.85, .2), legend.title = element_blank(),
      legend.position = "none",
      # axis.text.y = element_text(color = "#FDAE61"), # yellow
      # axis.text.y = element_text(color = "#cd0000"), # dark red
      # axis.text.y = element_text(color = "#ff4d00"), # redish orange
      axis.title.y = element_text(vjust = 0.2),
      axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
    ) )

(do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5,
) + geom_hline(yintercept = 0, colour = "gray") +
    # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
    geom_point(size = 1, fill = "white") +
    xlab("Mean depth for species") +
    ylab(expression(~Delta~"% change in biomass with SD change in DO")) + # guides(colour = F) +
    # coord_cartesian(xlim = c(15, 450)) +
    coord_cartesian(expand = F, 
      # ylim = c(-2.5, 2.5),
      xlim = c(15, 450)) +
    # ggrepel::geom_text_repel(aes(label = species_lab),
    #   size = 2, force = 3, 
    #   # nudge_y = -1, nudge_x = -10,
    #   na.rm = T, min.segment.length = 2
    # ) +
    scale_colour_manual(values = c("darkcyan", "darkcyan")) +
    gfplot::theme_pbs() + theme(
      # legend.position = c(.85, .2), legend.title = element_blank(),
      legend.position = "none",
      plot.margin = margin(0, 0.1, 0, 0.2, "cm"),
      axis.title.y.left = element_text(vjust = 0.2),
      axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
    ))

(temp_high2 <- slope_scatterplot(
  filter(temp_vel_slopes2, chopstick == "high"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5
) +
  # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point(size = 1, fill = "white") +
  # ggrepel::geom_text_repel(aes(label = species_lab),
  #   size = 2,
  #   force = 3,
  #   nudge_y = 1.5,
  #   # nudge_x = 15,
  #   na.rm = T, min.segment.length = 4
  # ) +
    ylab(expression(~Delta~"biotic velocity with SD change in T")) +
    # ylab( expression(~Delta~"velocity at high temp")) +
  scale_y_continuous(breaks = c(3, 0, -3, -6, -9)) +
  # coord_cartesian( xlim = c(15, 450)) + #ylim = c(-11, 5.5),
  coord_cartesian(expand = F, 
    # ylim = c(-6.5, 3), 
    xlim = c(15, 450)) +
  scale_colour_manual(values = c("Red 3", "Red 3")) +
  # scale_colour_manual(values = c("#D53E4F","#D53E4F")) +
  theme(
    plot.margin = margin(0, 0.1, 0, 0.2, "cm"),
    # legend.position = c(.85, .2), legend.title = element_blank(),
    legend.position = "none",
    # axis.text.y = element_text(color = "#FDAE61"), # yellow
    # axis.text.y = element_text(color = "#cd0000"), # dark red
    # axis.text.y = element_text(color = "#ff4d00"), # redish orange
    axis.title.y = element_text(vjust = 0.2),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  ) )

(do_low2 <- slope_scatterplot(filter(do_vel_slopes2, chopstick == "low"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5,
) + geom_hline(yintercept = 0, colour = "gray") +
  # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
  geom_point(size = 1, fill = "white") +
  xlab("Mean depth for species") +
  ylab(expression(~Delta~"biotic velocity for SD change in DO")) +
  # ylab(expression(~Delta~"velocity at low DO")) + # guides(colour = F) +
  # coord_cartesian(xlim = c(15, 450)) +
  coord_cartesian(expand = F, 
    # ylim = c(-2.5, 2.5),
    xlim = c(15, 450)) +
  # ggrepel::geom_text_repel(aes(label = species_lab),
  #   size = 2, force = 3, 
  #   # nudge_y = -1, nudge_x = -10,
  #   na.rm = T, min.segment.length = 2
  # ) +
  scale_colour_manual(values = c("darkcyan", "darkcyan")) +
  gfplot::theme_pbs() + theme(
    # legend.position = c(.85, .2), legend.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.1, 0, 0.2, "cm"),
    axis.title.y.left = element_text(vjust = 0.2),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  ))


#### CLIMATE BY DEPTH ####
### CODE OPTIONS FROM VOCC FOR EXCLUDING BEYOND RANGE SAMPLED
# # d <- d %>% filter(do_est > 0.23) %>% filter(do_est < 7.91) # full range
# d <- d %>%
#   filter(do_est > 0.28) %>%
#   filter(do_est < 7.06) # 0.005 and 0.995
# # d <- d %>% filter(temp > 2.61) %>% filter(temp < 14.31) # full range
# d <- d %>%
#   filter(temp > 3.07) %>%
#   filter(temp < 11.3) # 0.005 and 0.995
do_data <- readRDS(paste0("analysis/VOCC/data/predicted-DO-2020-06-20-more2016.rds")) %>%
  select(X, Y, year, depth, temp, do_est)

(p_depth_t <- ggplot(do_data, aes(depth, temp, colour = temp)) +
    scale_color_viridis_c(option = "B", end = 0.8) +
    geom_point(
      alpha = 0.02, size = 0.432,
      # alpha = 0.2, #size = 0.432, 
      shape = 20) +
    coord_cartesian(xlim = c(15, 450), ylim = c(4, 13.5), expand = F) +
    ylab("Temperature (ºC)") + # , expand = expand_scale(mult = c(0.05, .2)
    geom_smooth(colour = "black", size = 0.5, se = F) +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0.1, 0.1, 0.2, "cm"), legend.position = "none",
      axis.title.x = element_blank()
    ))

(p_depth_do <- ggplot(do_data, aes(depth, do_est, colour = do_est)) +
  scale_color_viridis_c(trans = sqrt, end = 1) +
  geom_point(alpha = 0.02, shape = 20#, size = 0.432
    ) +
  geom_smooth(colour = "black", size = 0.5, se = F) +
  scale_y_continuous(position = "right") +
  coord_cartesian(xlim = c(15, 450), ylim = c(0.2, 7.5), expand = F) + # ylim = c(0, 11.5),
  ylab("Mean DO (ml/L)") +
    # scale_x_reverse() +
    # xlim(450, 15) +
    # ylim(0.2, 7.5) +
    # coord_flip(expand = F) +  
  geom_hline(yintercept = 6.4, colour = "grey", linetype = "dashed") + # 100% saturation at 1 bar, 10 degree C, ~35 salinity
  geom_hline(yintercept = 1.8, colour = "grey40", linetype = "dashed") + # 30% saturation onset for mild hypoxia
  # geom_hline(yintercept = 0.64, colour = "black", linetype = "dashed") + # 10% saturation onset for severe hypoxia
  xlab("Mean depth") +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.3, 0.1, 0, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none"
  ))


d <- temp_slopes %>% filter(chopstick == "high" & type == "Temp" & age == "Immature") 

# if adding to tex
# cor(d$log_age_scaled, d$growth_rate_scaled, use = "pairwise.complete.obs", method = "spearman")

(growth <- ggplot(d, aes(y = age_mean, x=(length_50_mat_f / age_mat))) +
  geom_point(aes(shape = age, colour = age), size = 2) +
  scale_colour_manual(values = c("deepskyblue3")) +
  scale_shape_manual(values = c(21)) +
  ylim(1,21) +
  scale_x_log10(
    # position = "top",
    expand= c(0,0.1)
    ) +
  scale_y_log10(
    # position = "right",
    expand= c(0.01,0.1)
    ) +
  ggrepel::geom_text_repel(aes(label = gsub(" Rockfish", "", species)),
    point.padding = 0.3, segment.colour = "deepskyblue3", max.iter = 5000,
    size = 3, force = 20, #nudge_y = -0.1, #nudge_x = 35,
    na.rm = T, min.segment.length = 0.5, seed = 1000
  ) +
  xlab("Immature growth rate (cm per year)") +
  ylab("Mean age of immature population (years)") +
  gfplot::theme_pbs() + theme(
    # plot.margin = margin(0.2, 0.2, 0.4, 0.4, "cm"),
    # axis.text.y.right = element_text(
    # margin = margin(l = -10, r = 10)
    # ),
    # axis.text.x.top  = element_text(
    # margin = margin(b = -10, t = 10)
    # ),
    # axis.ticks.length=unit(-0.2, "cm"),
    # axis.title.y.right = element_text(),
    legend.position = "none"
   )
  )

# (p_growth <- growth %>% egg::tag_facet(
#   open = "", close = ".", tag_pool = c("b"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
# ))

# ggsave(here::here("ms", "figs", "supp-age-growth.pdf"), width = 4, height = 4)

(p_iqr <- temp_slopes %>% filter(chopstick == "high") %>% 
    mutate(labs = if_else(age == "Mature" & (depth > 250 | depth_iqr >100),  gsub(" Rockfish", "", species), NA_character_)) %>%
    ggplot(aes(depth, depth_iqr)) + 
    # geom_smooth(method = "lm", colour = "black", size =0.5) +
    geom_line(aes(group = species), colour = "grey60") +
    geom_point(aes(shape = age, colour = age), fill = "white", size = 1.5) +
    scale_colour_manual(values = c("deepskyblue3", "royalblue4")) +
    # scale_colour_manual(values = c("royalblue4","deepskyblue3" )) +
    # scale_fill_manual(values = c("cornflowerblue", "deepskyblue")) +
    # scale_colour_manual(values = c("royalblue4", "darkorchid4")) +
    # scale_fill_manual(values = c("royalblue4", "darkorchid4")) + 
    scale_shape_manual(values = c(21, 19)) +
    coord_cartesian(xlim = c(15, 450), ylim = c(2, 240), expand = F) +
    # scale_x_log10() +
    # scale_y_log10() +
    ggrepel::geom_text_repel(
      aes(label = labs), colour = "royalblue4",
      point.padding = 0.3, segment.colour = "deepskyblue3", max.iter = 10000,
      size = 2, #force = 10,
      nudge_y = -5, nudge_x = 5,
      na.rm = T, min.segment.length = 10, seed = 1000
    ) +
    ylab("Depth range (IQR)") +
    xlab("Mean depth") +
    gfplot::theme_pbs() + theme(
      plot.margin = margin(0, 0, 0, 0.3, "cm"),
      legend.position = c(0.2, 0.8),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    ))  
# 
# (p_iqr2 <- temp_slopes %>% filter(chopstick == "high") %>% 
#     mutate(labs = if_else(age == "Mature",  gsub(" Rockfish", "", species), NA_character_)) %>%
#     ggplot(aes(depth, depth_iqr)) + 
#     # geom_smooth(method = "lm", colour = "black", size =0.5) +
#     # geom_line(aes(group = species), colour = "grey60") +
#     geom_point(aes(shape = age, colour = age, alpha = age), size = 1.5) +
#     scale_colour_manual(values = c("white", "royalblue4")) +
#     scale_alpha_manual(values = c(0, 0.2)) +
#     # scale_colour_manual(values = c("royalblue4","deepskyblue3" )) +
#     # scale_fill_manual(values = c("cornflowerblue", "deepskyblue")) +
#     # scale_colour_manual(values = c("royalblue4", "darkorchid4")) +
#     # scale_fill_manual(values = c("royalblue4", "darkorchid4")) + 
#     scale_shape_manual(values = c(21, 19)) +
#     coord_cartesian(xlim = c(15, 450), ylim = c(2, 240), expand = F) +
#     # scale_x_log10() +
#     # scale_y_log10() +
#     ggrepel::geom_text_repel(
#       aes(label = labs), colour = "royalblue4",
#       point.padding = 0,
#       # segment.colour = "deepskyblue3", 
#       max.iter = 5000,
#       size = 2, 
#       # force = 0.3, 
#       nudge_y = 0, nudge_x = 0,
#       na.rm = T, min.segment.length = 10, seed = 1000
#     ) +
#     ylab("Depth range (IQR)") +
#     xlab("Mean depth") +
#     gfplot::theme_pbs() + theme(
#       plot.margin = margin(0, 0, 0, 0, "cm"),
#       legend.position = "none",
#       axis.title = element_blank(),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       legend.title = element_blank()
#     ))  
# 
# #### add facet tags and save ####
# p_iqr <- p_iqr %>% egg::tag_facet(
#   tag_pool = c("a"),
#   open = "", close = ".", 
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
# )

temp_high3 <- temp_high %>% egg::tag_facet(
  tag_pool = c("a"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

do_low <- do_low + scale_y_continuous(position = "right") + theme(
  # axis.text.y = element_text(colour = "darkcyan"), # "#5E4FA2"),
  axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  plot.margin = margin(0, 0.1, 0, 0, "cm")
)

do_low3 <- do_low %>% egg::tag_facet( 
  tag_pool = c("b"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

temp_high4 <- temp_high2 %>% egg::tag_facet(
  tag_pool = c("c"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)


do_low2 <- do_low2 + scale_y_continuous(position = "right") + theme(
  # axis.text.y = element_text(colour = "darkcyan"), # "#5E4FA2"),
  axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  plot.margin = margin(0, 0.1, 0, 0, "cm")
)

do_low4 <- do_low2 %>% egg::tag_facet( 
  tag_pool = c("d"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

depth_t <- p_depth_t %>% egg::tag_facet(
  tag_pool = c("e"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
depth_do <- p_depth_do %>% egg::tag_facet(
  tag_pool = c("f"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
# p_growth <- growth %>% egg::tag_facet(
#   open = "", close = ".", tag_pool = c("b"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
# )

# ggsave(here::here("ms", "figs", "supp-depth-iqr.pdf"), width = 5, height = 3.5)

layout <- "
      AB
      CD
      EF
      "

(temp_high3 + do_low3 +  
    temp_high4 + do_low4 +
    depth_t + depth_do + 
  plot_layout(design = layout, heights = c(0.5, 0.5, 0.25))) / grid::textGrob("Mean depth", just = 0.5, gp = grid::gpar(fontsize = 11)) + 
  plot_layout(nrow = 2, heights = c(1, 0.02))

# ggsave(here::here("ms", "figs", "slope-by-depth-quad-iqr.png"), width = 8, height = 5)
ggsave(here::here("ms", "figs", "slope-by-depth-w-newclim-vel.png"), width = 9, height = 9)

#########################

# REMOVE TREND SLOPES
#### add facet tags and save ####

p_iqr <- p_iqr %>% egg::tag_facet(
  tag_pool = c("a"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

temp_high5 <- temp_high2 %>% egg::tag_facet(
  tag_pool = c("b"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

do_low5 <- do_low2 %>% egg::tag_facet( 
  tag_pool = c("c"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

depth_t2 <- p_depth_t %>% egg::tag_facet(
  tag_pool = c("d"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
depth_do2 <- p_depth_do %>% egg::tag_facet(
  tag_pool = c("e"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
# p_growth <- growth %>% egg::tag_facet(
#   open = "", close = ".", tag_pool = c("b"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
# )

# ggsave(here::here("ms", "figs", "supp-depth-iqr.pdf"), width = 5, height = 3.5)

layout <- "
      AB
      CD
      EF
      "

(p_iqr + p_iqr2 + temp_high5 + do_low5 + depth_t2 + depth_do2 + plot_layout(design = layout, heights = c(0.6, 0.7, 0.5))) / grid::textGrob("Mean depth", just = 0.5, gp = grid::gpar(fontsize = 11)) + 
  plot_layout(nrow = 2, heights = c(1, 0.02))

# ggsave(here::here("ms", "figs", "slope-by-depth-quad-iqr.png"), width = 8, height = 5)
ggsave(here::here("ms", "figs", "slope-by-depth-vel-only.png"), width = 9, height = 7)


### remove slope plots
layout <- "
      AB
      CD
      "


depth_t_B <- p_depth_t %>% egg::tag_facet(
  tag_pool = c("b"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
depth_do_C <- p_depth_do %>% egg::tag_facet(
  tag_pool = c("c"),
  open = "", close = ".", 
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

(p_iqr + p_iqr2 + depth_t_B + depth_do_C + plot_layout(design = layout, heights = c(0.6, 0.6))) / grid::textGrob("Mean depth", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))

# ggsave(here::here("ms", "figs", "slope-by-depth-quad-iqr.png"), width = 8, height = 5)
ggsave(here::here("ms", "figs", "depth-only.png"), width = 9, height = 5)



#########################
#### COEFFICIENT SCATTERPLOTS AGAINST LIFE HISTORY ####

# model_vel$coefs <- model_vel$coefs %>% mutate(
#   age = if_else(gsub(" .*", "", species) == "immature", "Immature", "Mature"),
#   species = stringr::str_replace(species, ".*mature ", ""))
# 
stats2 <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth.rds"))

### if we want trend coefs ####
# model2 <- add_colours(model$coefs, species_data = stats2)
# model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
# model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
# model2$rockfish[model2$rockfish == "rockfish"] <- "Rockfish"
# model2$rockfish[model2$rockfish == "other fishes"] <- "Other fishes"
# 
# model2 <- model2 %>%
#   group_by(group) %>%
#   mutate(spp_count = length(unique(species))) %>%
#   ungroup()
# model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc = F))
# model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc = F))
# trendeffects <- model2 %>%
#   filter(coefficient %in% c("temp_trend_scaled", "DO_trend_scaled")) %>%
#   # transform(coefficient = factor(coefficient,
#   mutate(coefficient = factor(coefficient,
#     levels = c("temp_trend_scaled", "DO_trend_scaled"),
#     labels = c("Temperature", "DO")
#   ), Std..Error = `Std. Error`)

### if we want vel coefs ####
model2 <- add_colours(model_vel$coefs, species_data = stats2)
model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
model2$rockfish[model2$rockfish == "rockfish"] <- "Rockfish"
model2$rockfish[model2$rockfish == "other fishes"] <- "Other fishes"

model2 <- model2 %>%
  group_by(group) %>%
  mutate(spp_count = length(unique(species))) %>%
  ungroup()
model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc = F))
model2 <- model2 %>% mutate(rockfish = forcats::fct_reorder(rockfish, Estimate, .desc = F))
trendeffects <- model2 %>%
  filter(coefficient %in% c("squashed_temp_vel_scaled", "squashed_DO_vel_scaled")) %>%
  # transform(coefficient = factor(coefficient,
  mutate(coefficient = factor(coefficient,
    levels = c("squashed_temp_vel_scaled", "squashed_DO_vel_scaled"),
    labels = c("Temperature", "DO")
  ), Std..Error = `Std. Error`)



trendeffects <- trendeffects %>%
  mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc = F))
# trendeffects <- mutate(trendeffects, rockfish = firstup(rockfish) # didn't work
trendeffects$allspp <- "All species"


### changed to IQR for depth
# cordat <- stats %>% select(species, age, depth, depth_iqr) %>% na.omit()
# cor(cordat$depth_iqr,cordat$depth)

(p_depth <- coef_scatterplot(trendeffects,
  coef = c("Temperature", "DO"),
  x = "depth_iqr", group = "age", regression = F
) +
  ylab("Trend coefficient") + xlab("Depth range (IQR)") + # (25th to 75th quartile)
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"),
    inherit.aes = F,
    aes_string("depth_iqr", "Estimate"),
    method = "lm", size = 0.5,
    colour = "darkgray",
    fill = "lightgray"
  ) + geom_point(size = 1, fill = "white") +
  # scale_colour_manual(values = c("gold", "darkorchid4")) + #"royalblue4" #chartreuse4
  scale_colour_viridis_d(option = "D", begin = 0.8, end = 0.2) +
  scale_x_log10() +
  guides(colour = F) +
  facet_grid(rows = vars(coefficient), cols = vars(allspp), scales = "free_y") +
  # gfplot:::theme_pbs() %+replace%
  theme(
    legend.position = "none",
    plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(), strip.text.x = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4)
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank()
  ))
# colorblindr::cvd_grid(p_depth)

# # with rockfish split out
# p_age <- coef_scatterplot(trendeffects,
#   coef = c("Temperature", "DO"),
#   x = "age_mean", group = "age", regression = F
#   ) +
#   geom_smooth(
#     # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
#     aes_string("age_mean", "Estimate"), method = "lm",
#     # colour = "darkgray",
#     fill = "lightgray"
#   ) +
#   xlab("Mean age") +
#   scale_colour_viridis_d(begin = .8, end = .2) +
#   guides(colour = F) +
#   theme(
#     plot.margin = margin(0.1, 0.25, 0.1, 0.1, "cm"), strip.background = element_blank(),
#     strip.text.y = element_blank(),
#     axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()
#   ) +
#   facet_grid(rows = vars(coefficient), cols = vars(rockfish), scales = "free")

(p_age <- coef_scatterplot(trendeffects,
  coef = c("Temperature", "DO"),
  x = "age_mean", group = "age", regression = F
) +
  geom_smooth(
    # data = filter(trendeffects, coefficient != "DO" & age == "mature"),
    inherit.aes = F,
    aes_string("age_mean", "Estimate"),
    method = "lm", size = 0.5,
    colour = "darkgray",
    fill = "lightgray"
  ) + geom_point(size = 1, fill = "white") +
  xlab("Mean age") +
  # scale_colour_manual(values = c("darkorchid4", "royalblue4")) + #"darkcyan"
  scale_colour_viridis_d(begin = 0.8, end = 0.2) +
  scale_x_log10() +
  guides(colour = F) +
  theme(
    legend.position = "none",
    plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(), strip.text.x = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  facet_grid(rows = vars(coefficient), cols = vars(allspp), scales = "free_y")
)

trendeffects <- mutate(trendeffects, growth_rate = length_50_mat_f / age_mat, growth_rate_m = length_50_mat_m / age_mat_m)
# plot(data = trendeffects, growth_rate ~ growth_rate_m)


# # with rockfish split out
# p_mat <- coef_scatterplot(trendeffects,
#   coef = c("Temperature", "DO"),
#   x = "growth_rate",
#   # x = "length_50_mat_f",
#   group = "age", regression = F
#   ) +
#   xlab("Immature growth rate") +
#   geom_smooth(
#     # data = filter(trendeffects, coefficient != "DO" & age == "mature"), inherit.aes = F,
#     aes_string("growth_rate", "Estimate"), method = "lm",
#     # colour = "darkgray",
#     fill = "lightgray"
#   ) +
#   scale_colour_viridis_d(begin = .8, end = .2) +
#   theme(
#     plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
#     strip.background = element_blank(),
#     legend.position = c(.75, .15), legend.title = element_blank(),
#     # strip.text = element_blank(),
#     axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
#   ) +
#   facet_grid(coefficient ~ rockfish, scales = "free")


## left side tags
# p_depth2 <- p_depth %>% egg::tag_facet(open = "", close = ".", vjust = 1.7, hjust = -1, tag_pool = c("a","f"))
# p_age2 <- p_age %>% egg::tag_facet(open = "", close = ".", vjust = 1.7, hjust = -1, tag_pool = c("b", "c", "g", "h"))
# p_mat2 <- p_mat %>% egg::tag_facet(open = "", close = ".", vjust = 1.7, hjust = -1, tag_pool = c("d", "e", "i", "j"))

# ### with rockfish split out for age
# ## right side tags
# p_depth2 <- p_depth %>% egg::tag_facet(open = "", close = ".", tag_pool = c("a","f"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1)
# p_age2 <- p_age %>% egg::tag_facet(open = "", close = ".", tag_pool = c("b", "c", "g", "h"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1)
# p_mat2 <- p_mat %>% egg::tag_facet(open = "", close = ".", tag_pool = c("d", "e", "i", "j"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1)
# cowplot::plot_grid(p_depth2, p_age2, p_mat2, nrow = 1, rel_widths = c(1.1, 1.75, 1.75))

p_mat <- coef_scatterplot(trendeffects,
  coef = c("Temperature", "DO"),
  x = "growth_rate",
  # x = "length_50_mat_f",
  group = "age", regression = F
) +
  xlab("Immature growth rate") +
  geom_smooth(
    data = filter(trendeffects, age != "mature"), # inherit.aes = F,
    aes_string("growth_rate", "Estimate"), method = "lm", size = 0.5,
    # colour = "darkgray",
    fill = "lightgray"
  ) + geom_point(size = 1, fill = "white") +
  # scale_colour_manual(values = c("darkorchid4", "darkcyan")) +
  scale_colour_viridis_d(begin = 0.8, end = 0.2) +
  scale_x_log10() +
  theme(
    plot.margin = margin(0.1, 0.15, 0.1, 0, "cm"),
    strip.background = element_blank(),
    strip.text.y = element_blank(), strip.text.x = element_blank(),
    legend.position = c(.75, .15), legend.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  facet_grid(rows = vars(coefficient), cols = vars(allspp), scales = "free_y")


p_depth2 <- p_depth %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("a", "d"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
p_age2 <- p_age %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("b", "e"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
p_mat2 <- p_mat %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("c", "f"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

cowplot::plot_grid(p_depth2, p_age2, p_mat2, nrow = 1, rel_widths = c(1.1, 0.9, 0.9))

# ggsave(here::here("ms", "figs", "coef-scatterplots-allspp2.pdf"), width = 7, height = 4)


#########################
#########################
#### WORM OF BIOTIC ESTIMATES ####
### TEMPERATURE: 2 or 3 levels of change ####
(p_temp_est_min <- plot_chop_est(model_vel, type = "temp",  x_variable = "squashed_temp_vel_scaled", 
  # where_on_x = "middle",
  where_on_x = "min",
  add_grey_bars = T,
  # sort_var = "min",
  alt_order = T, 
  legend_position = "none",
  # legend_position = c(.75, .93),
  alpha_range = c(0.99, 0.99)) +
    # alpha_range = c(0.5, 0.99)) +
    # alpha_range = c(0.2, 0.99)) +
    coord_cartesian(xlim = c(-30,60)) +
    ggtitle("a. Minimum warming") +
  # xlab("Biotic velocity at midpoint of temperature velocities experienced")
  # xlab("Biotic velocity at max velocities")
  theme(
    # legend.position = "top",
    # legend.margin = unit(0, "cm"),
    # legend.spacing.y = unit(0.2, "cm"),
    axis.title = element_blank())
)

(p_temp_est_mean <- plot_chop_est(model_vel, type = "temp",  x_variable = "squashed_temp_vel_scaled", 
  where_on_x = "middle",
  add_grey_bars = T,
  # sort_var = "min",
  alt_order = T, 
  legend_position = "none",
  alpha_range = c(0.99, 0.99)) +
  # alpha_range = c(0.5, 0.99)) +
  # alpha_range = c(0.2, 0.99)) +
    coord_cartesian(xlim = c(-30,60)) +
    ggtitle("b. Midpoint warming") +
    # xlab("Biotic velocity at midpoint of temperature velocities experienced")
    # xlab("Biotic velocity at max velocities")
    theme(    
      # legend.position = "top",
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())
)


(p_temp_est_worm <- plot_chop_est(model_vel, type = "temp",  x_variable = "squashed_temp_vel_scaled", 
  # where_on_x = "middle",
  # where_on_x = "min",
  add_grey_bars = T,
  # sort_var = "min",
  legend_position = "none",
  alpha_range = c(0.99, 0.99)) +
  # alpha_range = c(0.5, 0.99)) +
    # alpha_range = c(0.2, 0.99)) +
    coord_cartesian(xlim = c(-30,60)) +
    scale_y_discrete(expand = expansion(mult = .02), position = "right") +
    # xlab("Biotic velocity at midpoint of temperature velocities experienced")
    # xlab("Biotic velocity at max velocities")
    ggtitle("c. Maximum warming") +
    # coord_flip() +
  theme(
    # legend.position = "top",
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
)
# ggsave(here::here("ms", "figs", "worm-temp-ests-vel-max.pdf"), width = 4.5, height = 6)
  
(# with midpoint plot
  (p_temp_est_min |p_temp_est_mean| p_temp_est_worm ) + plot_layout(guides = 'collect')& theme(
  legend.position = "top",  
    legend.justification='left',
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin=margin(t = 0.2, l= 0, r = 1.3, unit='cm'),
    # legend.margin = unit(1.5, "cm"),
    legend.spacing.x = unit(.1, "cm")
  )
  ) / grid::textGrob("Estimated biotic velocity (km per decade)", just = 0.3, gp = grid::gpar(fontsize = 11))  + plot_layout(height = c(10, 0.02))

# ggsave(here::here("ms", "figs", "worm-temp-ests-min-mean-max.pdf"), width = 8, height = 8)
# ggsave(here::here("ms", "figs", "worm-temp-ests-min-mean-max-faded.pdf"), width = 8, height = 8)


p_temp_est_min <- p_temp_est_min + ggtitle("a. Minimum warming experienced")
p_temp_est_worm <- p_temp_est_worm + ggtitle("b. Maximum warming experienced")

# ( # top legend
#   (p_temp_est_min | p_temp_est_worm ) + plot_layout(guides = 'collect')& theme(
#     legend.position = "top",  
#     legend.justification='left',
#     legend.direction = "horizontal",
#     legend.box = "horizontal",
#     legend.margin=margin(t = 0.2, l= 0, r = 1.4, unit='cm'),
#     # legend.margin = unit(1.5, "cm"),
#     legend.spacing.x = unit(.1, "cm")
#   )
# ) / grid::textGrob("Estimated biotic velocity (km per decade)",  hjust = 0.3, gp = grid::gpar(fontsize = 11))  + plot_layout(height = c(10, 0.02))

((p_temp_est_min | p_temp_est_worm ) 
  / grid::textGrob("Estimated biotic velocity (km per decade)", vjust = -0.8, just = 0.3, gp = grid::gpar(fontsize = 11)) + 
    plot_layout(height = c(10, 0.02), guides = 'collect')& theme(
      legend.text = element_text(size = 9),
  legend.position = "bottom",  
  legend.justification='left',
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.margin=margin(t = 0.5, l= 1, r = 0, unit='cm'),
  # legend.margin = unit(1.5, "cm"),
  legend.spacing.x = unit(.1, "cm")
))  

ggsave(here::here("ms", "figs", "worm-temp-ests-min-max-b.pdf"), width = 7, height = 7)
# ggsave(here::here("ms", "figs", "worm-temp-ests-min-max-faded.pdf"), width = 8, height = 8)


### TEMPERATURE vs DO at max level of change ####
(p_do_est_worm <- plot_chop_est(model_vel, type = "DO",  x_variable = "squashed_DO_vel_scaled", 
  # where_on_x = "middle",
  # where_on_x = "min",
  add_grey_bars = T,
  # sort_var = "min",
  alpha_range = c(0.4, 0.99),
  legend_position = "none") + 
  scale_y_discrete(expand = expansion(mult = .02), position = "right") +
  # xlab("Biotic velocity at middle of DO velocities experienced")
  theme(axis.title = element_blank()) 
)

(p_temp_est_worm | p_do_est_worm ) / grid::textGrob("Biotic velocity at min of climate velocities experienced", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

# ggsave(here::here("ms", "figs", "worm-plot-ests-vel-max.pdf"), width = 9, height = 6)


### TEMPERATURE vs DO TRENDS at max level of change ####
p_temp_est_worm <- plot_chop_est(model, type = "temp",  x_variable = "temp_trend_scaled", 
  # where_on_x = "middle",
  add_grey_bars = T,
  sort_var = "min",
  legend_position = "none") +
  #xlab("Biotic velocity at midpoint of temperature velocities experienced")
  theme(axis.title = element_blank()) 

(p_do_est_worm <- plot_chop_est(model, type = "DO",  x_variable = "DO_trend_scaled", 
  # where_on_x = "middle",
  add_grey_bars = T,
  sort_var = "min",
  legend_position = "none") + 
    scale_y_discrete(expand = expansion(mult = .02), position = "right") +
    # xlab("Biotic velocity at middle of DO velocities experienced")
    theme(axis.title = element_blank()) 
)

(p_temp_est_worm | p_do_est_worm ) / grid::textGrob("Biomass trend at midpoint of climate trend experienced", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

# ggsave(here::here("ms", "figs", "worm-plot-ests-trend.pdf"), width = 9, height = 6)


### exploratory boxplots ####
model_vel$pred_dat %>%
  filter(type == "temp") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(squashed_temp_vel_scaled == max(squashed_temp_vel_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()

model_vel$pred_dat %>%
  filter(type == "temp") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(squashed_temp_vel_scaled == min(squashed_temp_vel_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()

model_vel$pred_dat %>%
  filter(type == "temp") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(squashed_temp_vel_scaled == mean(squashed_temp_vel_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()

model_vel$pred_dat %>%
  filter(type == "DO") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(squashed_DO_vel_scaled == max(squashed_DO_vel_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()

model_vel$pred_dat %>%
  filter(type == "DO") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(squashed_DO_vel_scaled == min(squashed_DO_vel_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()


model$pred_dat %>%
  filter(type == "temp") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(temp_trend_scaled == max(temp_trend_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) + 
  geom_boxplot()

model$pred_dat %>%
  filter(type == "DO") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(DO_trend_scaled == max(DO_trend_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) +
  geom_violin() + 
  geom_boxplot() 

model$pred_dat %>%
  filter(type == "DO") %>% 
  group_by(genus, species, chopstick) %>% 
  filter(DO_trend_scaled == min(DO_trend_scaled)) %>% 
  summarise(est = est_p, se = se_p) %>% 
  ggplot(aes(chopstick, est, colour = chopstick)) +
  geom_violin() + 
  geom_boxplot() 

