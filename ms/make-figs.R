setwd(here::here())

library(TMB)
library(tidyverse)
library(patchwork)
library(gfranges)

write_tex <- function(x, macro, ...) {
  paste0("\\newcommand{\\", macro, "}{", x, "}") %>%
    readr::write_lines("ms/values.tex", append = TRUE)
}

# load appropriate final models
model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")
model_vel <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-27-vel-both-1-200.rds")

## Maybe just for supplementary
# # model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
# # model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")
# model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-12-vel-temp-1-200-temp.rds")
# model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-12-vel-do-1-200-DO.rds")
#
# model_temp <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-22-trend-1-500-temp.rds")
# model_grad <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-23-trend-w-grad-1-500-temp.rds")
# # model_do <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-22-trend-do-only-1-500.rds")

stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav.rds"))
alldata <- readRDS(paste0("analysis/VOCC/data/all-do-with-null-1-untrimmed-allvars.rds"))

#### SAVE TEX VALUES FOR CLIMATE IQRs ####

# # temperature
# write_tex(signif(quantile(alldata$temp_trend, 0.025), digits = 2), "lowTquantile")
# write_tex(signif(quantile(alldata$temp_trend, 0.975), digits = 2), "highTquantile")
# # DO
# write_tex(signif(quantile(alldata$DO_trend, 0.025), digits = 2), "lowDOquantile")
# write_tex(signif(quantile(alldata$DO_trend, 0.975), digits = 2), "highDOquantile")


#########################
#########################
#### CLIMATE MAPS ####
(mean_do <- plot_vocc(alldata, # grey_water = T,
  vec_aes = NULL, grey_water = F,
  fill_col = "mean_DO", fill_label = "ml/L ",
  raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
  axis_lables = F, tag_text = "b.",
  viridis_begin = 0.2,
  raster_limits = c(0.69, 5.24),
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
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan", # "lightseagreen",
  # high_fill = "#3d95cc",
  # low_fill = "yellowgreen",
  raster_limits = c(-1.7, 2.3), na_colour = "gold", 
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
  # mid_fill = "azure", grey_water = F,
  high_fill = "gold",
  low_fill = "darkcyan",
  axis_lables = F, tag_text = "f.",
  legend_position = c(0.15, 0.3)
) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  ))


mean_temp + mean_do + trend_temp + trend_do + vel_temp + vel_do + plot_layout(ncol = 2)
# colorblindr::cvd_grid(trend_do)
# ggsave(here::here("ms", "figs", "climate-maps-updated.png"), width = 6, height = 9)
ggsave(here::here("ms", "figs", "climate-maps-witwat2.png"), width = 5, height = 7.5)

#########################
#########################
#### GLOBAL COEFS ####
#########################
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

### SAVE TEX VALUES FOR TEMPERATURE TREND ESTIMATES ###
# write_tex(signif(overall_betas$betas[overall_betas$coef_names == "change in T"], 2), "betaTtrend")
# write_tex(signif(abs(overall_betas$betas[overall_betas$coef_names == "change in T"]), 2), "ABSbetaTtrend")
# write_tex(signif(overall_betas$lowerCI[overall_betas$coef_names == "change in T"], 2), "lowerTtrend")
# write_tex(signif(overall_betas$upperCI[overall_betas$coef_names == "change in T"], 2), "upperTtrend")


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

# ### SAVE TEX VALUES FOR TEMPERATURE VELOCITY ESTIMATES ###
# write_tex(signif(overall_betas_vel$betas[overall_betas_vel$coef_names == "change in T"], 2), "betaTvel")
# write_tex(signif(abs(overall_betas_vel$betas[overall_betas_vel$coef_names == "change in T"]), 2), "ABSbetaTvel")
# write_tex(signif(overall_betas_vel$lowerCI[overall_betas_vel$coef_names == "change in T"], 2), "lowerTvel")
# write_tex(signif(overall_betas_vel$upperCI[overall_betas_vel$coef_names == "change in T"], 2), "upperTvel")

#### ADD NULLS AND MAKE VIOLIN PLOT ####
null01 <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-29-trend-with-do-sim-1-500-DO.rds")
null02 <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-29-trend-with-do-sim-2-500-DO.rds")
null03 <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-29-trend-with-do-sim-3-500-DO.rds")
null04 <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-29-trend-with-do-sim-4-500-DO.rds")
null05 <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-29-trend-with-do-sim-5-500-DO.rds")

vnull01 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-1-200-DO.rds")
vnull02 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-2-200-DO.rds")
vnull03 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-3-200-DO.rds")
vnull04 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-29-vel-both-sim-4-200-DO.rds")
vnull05 <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-30-vel-both-sim-5-200-DO.rds")

model$coefs$model <- "trend"
null01$coefs$model <- "null01"
null02$coefs$model <- "null02"
null03$coefs$model <- "null03"
null04$coefs$model <- "null04"
null05$coefs$model <- "null05"
# null06$coefs$model <- "null06"
# null07$coefs$model <- "null07"
# null08$coefs$model <- "null08"
# null09$coefs$model <- "null09"
# null10$coefs$model <- "null10"

model_vel$coefs$model <- "velocity"
vnull01$coefs$model <- "vnull01"
vnull02$coefs$model <- "vnull02"
vnull03$coefs$model <- "vnull03"
vnull04$coefs$model <- "vnull04"
vnull05$coefs$model <- "vnull05"

custom_order <- c(
  "Intercept", "log_biomass",
  "temp", "temp_trend", "temp_vel", "temp_trend:temp", "temp_vel:temp",
  "DO", "DO_trend", "DO_vel", "DO_trend:DO", "DO_vel:DO"
)

nulls <- rbind(model$coefs, null01$coefs, null02$coefs, null03$coefs, null04$coefs, null05$coefs) %>%
  mutate(
    term = factor(shortener(coefficient),
      levels = as.character(custom_order)
    ), std.error = `Std. Error`, change_var = "trend",
    type = if_else(model == "trend", "Trend model", "Null models")
  ) %>%
  rename(estimate = Estimate)

vnulls <- rbind(model_vel$coefs, vnull01$coefs, vnull02$coefs, vnull03$coefs, vnull04$coefs, vnull05$coefs) %>%
  mutate(
    term = factor(shortener(coefficient),
      levels = as.character(custom_order)
    ), std.error = `Std. Error`, change_var = "velocity",
    type = if_else(model == "velocity", "Velocity model", "Time-null velocities")
  ) %>%
  rename(estimate = Estimate)

null_coefs <- ggplot(nulls, aes(estimate, term, fill = type, colour = type)) +
  xlab("Coefficient estimate with 95% CI") + ylab("") +
  # geom_violin(scale = "width") +
  geom_violin( # aes(estimate, term), inherit.aes = F ,
    scale = "width",
    alpha = 0.1, # fill= "white",
    data = filter(nulls, model == "trend")
  ) +
  # geom_violin(#aes(estimate, term), inherit.aes = F ,
  #   scale = "width", # scale = "area", # scale = "count",
  #   data = filter(nulls, model != "trend")) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(nulls, model == "null01")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(nulls, model == "null02")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(nulls, model == "null03")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(nulls, model == "null04")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(nulls, model == "null05")
  ) +
  scale_y_discrete(
    limits = rev(unique(sort(nulls$term))),
    labels = c(
      "DO interaction", "DO trend", "Mean DO",
      "T interaction", "T trend", "Mean T", "Biomass", "Intercept"
    )
  ) +
  scale_fill_manual(name = "Model type", values = c("#D53E4F", "#FDAE61")) + # guides(fill = F) +
  scale_colour_manual(name = "Model type", values = c("#D53E4F", "#FDAE61", "#FDAE61")) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  geom_pointrange(aes(betas, coef_names, xmin = lowerCI, xmax = upperCI),
    size = 0.75, shape = "|", fatten = 6,
    inherit.aes = F,
    data = overall_t
  ) +
  coord_cartesian(xlim = c(-3, 2.75)) +
  ggtitle("a. Trend-based models") +
  gfplot::theme_pbs() + theme(
    axis.title = element_blank(), # element_text(size = 10),
    legend.title = element_blank(),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.025, 0.018)
  )

vnull_coefs <- ggplot(vnulls, aes(estimate, term,
  fill = type,
  colour = type
)) +
  xlab("Coefficient estimate with 95% CI") + ylab("") +
  geom_violin( # aes(estimate, term), inherit.aes = F ,
    scale = "width",
    alpha = 0.1, # fill= "white",
    data = filter(vnulls, model == "velocity")
  ) +
  # geom_violin(#aes(estimate, term), inherit.aes = F ,
  #   scale = "width", # scale = "count", # scale = "area",
  #   alpha = 0.1, data = filter(vnulls, model != "velocity")) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(vnulls, model == "vnull01")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(vnulls, model == "vnull02")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(vnulls, model == "vnull03")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(vnulls, model == "vnull04")
  ) +
  geom_violin(
    scale = "width",
    alpha = 0.1, data = filter(vnulls, model == "vnull05")
  ) +
  scale_y_discrete(
    limits = rev(unique(sort(vnulls$term))),
    labels = c(
      "DO interaction", "DO velocity", "Mean DO",
      "T interaction", "T velocity", "Mean T", "Biomass", "Intercept"
    )
  ) +
  scale_fill_manual(name = "Model type", values = c("#5E4FA2", "#ABDDA4")) + # guides(fill = F) +
  scale_colour_manual(name = "Model type", values = c("#5E4FA2", "#ABDDA4")) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  geom_pointrange(aes(betas, coef_names, xmin = lowerCI, xmax = upperCI),
    size = 0.75, shape = "|", fatten = 6,
    inherit.aes = F,
    data = overall_v
  ) +
  coord_cartesian(xlim = c(-15, 13)) +
  ggtitle("b. Velocity-based models") +
  gfplot::theme_pbs() + theme(
    axis.title = element_blank(), # element_text(size = 10),
    legend.title = element_blank(),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.025, 0.018)
  )

(null_coefs | vnull_coefs) / grid::textGrob("Species-specific coefficient estimates", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

ggsave(here::here("ms", "figs", "null-spp-violin2.pdf"), width = 8, height = 3.5)

#########################
#### compare just true trend and velocity global coefs ####
# custom_order <- c("intercept", "immature", "biomass", "gradient",
# "mean T", "mean T:immature", "mean T:gradient",
# "change in T",  "change in T:immature",  "change in T:gradient",
# "interaction (T)", "interaction (T):immature",  "interaction (T):gradient",
# "mean DO",  "mean DO:immature",
# "change in DO", "change in DO:immature",
# "interaction (DO)", "interaction (DO):immature"
# )
# overall <- rbind.data.frame(overall_betas, overall_betas_vel)
# overall <- mutate(overall, term = firstup(as.character(coef_names)))
# overall2 <- overall %>% rename(
#   estimate = betas, std.error = SE) #%>% filter(term != "intercept")
# overall2 <- overall2 %>% mutate(term = factor(term, #model = firstup(as.character(model)),
#   levels = firstup(as.character(custom_order))))
#
# global_coefs <- dotwhisker::dwplot(overall2, dot_args = list(size=2)
#   # order_vars = custom_order
# ) + #xlim(-10,10) +
#   geom_vline(xintercept = 0, colour = "darkgray") +
#   xlab("Coefficient estimate with 95% CI") +
#   # geom_point(aes(term, estimate,  colour = model), alpha= 0.1,
#   #   position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) +
#   scale_colour_manual(name = "Model",
#     values = c(
#       "#D53E4F",
#       # "#F46D43",
#        # "#FDAE61"
#       # "#ABDDA4"
#       "#5E4FA2"
#     )) + # ggtitle("a. Trend versus velocity models") +
#   gfplot::theme_pbs() + theme (legend.title = element_blank(),
#     legend.position = c(0.75, 0.275))
# global_coefs
# ggsave(here::here("ms", "figs", "global-coefs.pdf"), width = 4, height = 2.5)
#### experiment with species level coefs as boxplots ####
# trendcoefs <-add_colours(model$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c("(Intercept)", "temp_trend_scaled", "mean_temp_scaled", "DO_trend_scaled", "mean_DO_scaled",
#     "log_biomass_scaled", "temp_trend_scaled:mean_temp_scaled", "DO_trend_scaled:mean_DO_scaled" ),
#   labels = c("intercept", "change in T", "mean T", "change in DO", "mean DO",
#     "biomass", "interaction (T)", "interaction (DO)")))
#
# velcoefs1 <- add_colours(model_vel_t$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c("(Intercept)", "squashed_temp_vel_scaled", "mean_temp_scaled",
#     "log_biomass_scaled", "squashed_temp_vel_scaled:mean_temp_scaled"),
#   labels = c("intercept", "change in T", "mean T",
#     "biomass", "interaction (T)")))
#
# velcoefs2 <- add_colours(model_vel_d$coefs, species_data = stats) %>% transform(coefficient = factor(coefficient,
#   levels = c("(Intercept)", "squashed_DO_vel_scaled", "mean_DO_scaled",
#     "log_biomass_scaled", "squashed_DO_vel_scaled:mean_DO_scaled"),
#   labels = c("intercept", "change in DO", "mean DO",
#     "biomass", "interaction (DO)")))
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
#   ggplot(aes(coef_names, betas)) + #forcats::fct_reorder(coef_names, ) #, colour = model
#   geom_point(aes(forcats::fct_reorder(coefficient, Estimate), Estimate,  colour = age), alpha= 0.3, position = position_jitter(width = 0.15), inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")) +
#   geom_boxplot(aes(forcats::fct_reorder(coefficient, Estimate), Estimate,  colour = age),
#     notch =T,
#     inherit.aes = F, data = filter(allcoefs, coefficient != "intercept")) +
#   geom_pointrange(aes(ymin = lowerCI, ymax = upperCI), size = 1, fatten = 3, alpha = .9) + #position = position_jitter(width = 0.2), shape = "|"
#   geom_hline(yintercept = 0, colour = "darkgray") +
#   scale_colour_viridis_d(begin = .8, end = .2) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   # scale_colour_manual(values = c("#D53E4F", "#5E4FA2", "#3288BD")) + #
#   xlab("") +
#   facet_wrap(~model_type) +
#   coord_flip(ylim = c(-2,2)) + # ylim = c(-10,10)
#   gfplot::theme_pbs()


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
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)
temp_slopes <- left_join(temp_slopes, stats) %>%
  do_slopes() <- left_join(do_slopes, stats)

temp_slopes$species[temp_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
do_slopes$species[do_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

p_temp_worm <- plot_chopstick_slopes(temp_slopes,
  type = "temp",
  legend_position = c(.25, .95),
  name_chop_type = F,
  add_grey_bars = T
) + coord_flip(ylim = c(-6, 5)) +
  # annotate("rect", ymin = lowerT[[1]], ymax = upperT[[1]], xmin = -Inf, xmax = Inf, alpha=0.1, fill="black") +
  geom_hline(yintercept = estT[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  theme(axis.title.x = element_blank()) +
  ggtitle("a. Temperature")

p_do_worm <- plot_chopstick_slopes(do_slopes,
  type = "DO",
  legend_position = c(.25, .95),
  name_chop_type = F,
  add_grey_bars = T
) + coord_flip(ylim = c(-3, 1.4)) +
  # annotate("rect", ymin = lowerD[[1]], ymax = upperD[[1]], xmin = -Inf, xmax = Inf, alpha=0.1, fill="black") +
  geom_hline(yintercept = estD[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  ggtitle("b. DO") +
  # ylab("slopes")
  scale_x_discrete(position = "top") +
  theme(axis.title.x = element_blank())

# colorblindr::cvd_grid(p_temp_worm)
# colorblindr::cvd_grid(p_do_worm)

(p_temp_worm | p_do_worm) / grid::textGrob("Slope of biomass trend with a SD change in climate", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(height = c(10, 0.02))

ggsave(here::here("ms", "figs", "worm-plot-trend-newcol2.pdf"), width = 7.5, height = 6)

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
# temp_vel_slopes <- left_join(temp_vel_slopes, stat)
temp_vel_slopes <- temp_vel_slopes %>% mutate(sort_var = slope_est)
temp_vel_slopes$species[temp_vel_slopes$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

do_vel_slopes <- chopstick_slopes(model_vel,
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
  legend_position = c(.25, .95),
  add_grey_bars = T
) + coord_flip(ylim = c(-10, 4.5)) +
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
  add_grey_bars = T
) + coord_flip(ylim = c(-3.1, 1.4)) +
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
) + coord_flip(ylim = c(-10, 7.95)) +
  annotate("rect", ymin = lowervT[[1]], ymax = uppervT[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  geom_hline(yintercept = estvT[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  ggtitle("c.") +
  theme(
    plot.margin = margin(0.2, 0.2, 0, 0.2, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  xlab("Biotic velocity change for a SD increase in climate velocity")
p_do_worm3 <- plot_chopstick_slopes(do_vel_slopes,
  type = "DO",
  legend_position = c(.25, .95),
  add_grey_bars = T
) + coord_flip(ylim = c(-5.75, 3.25)) +
  geom_hline(yintercept = estvD[[1]], colour = "black", alpha = 0.5, linetype = "dashed") +
  annotate("rect", ymin = lowervD[[1]], ymax = uppervD[[1]], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") +
  scale_x_discrete(position = "top") +
  ggtitle("d.") +
  theme(
    plot.margin = margin(0.2, 0, 0, 0, "cm"),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

(p_temp_worm | p_do_worm) / (p_temp_worm3 | p_do_worm3) + plot_layout(height = c(1, 1)) #+ plot_annotation(tag_levels = "a")

ggsave(here::here("ms", "figs", "worm-plot-both.pdf"), width = 8, height = 10)

#########################
#########################
#### SPECIES CHOPSTICK PLOTS AND MAPS FROM TREND MODEL ####

species_panels <- function(species, model, x_type,
                           chop_label = F,
                           leftmost = F,
                           alpha_range = c(0.9, 0.9)) {
  age <- unique(model$data[model$data$species == species, ]$age)

  biotic_map <- filter(model$data, species == !!species) %>% plot_vocc(
    vec_aes = NULL,
    fill_col = "biotic_trend", fill_label = "%", raster_cell_size = 4,
    na_colour = "red 3", white_zero = TRUE,
    high_fill = "darkcyan",
    # mid_fill = "honeydew", grey_water = F,
    # mid_fill = "lightyellow", grey_water = F,
    mid_fill = "lightcyan1", grey_water = F,
    # mid_fill = "azure", grey_water = F,
    low_fill = "Red 3", 
    raster_limits = c(-6, 6),
    axis_lables = T,
    legend_position = c(0.15, 0.25),
    make_square = F
  ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
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
    biotic_map <- biotic_map + ggtitle(paste0(" ", age, "\n", shortener(species)))
    # biotic_map <- biotic_map + ggtitle(paste0(shortener(species), " (", age, ")"))
  } else {
    biotic_map <- biotic_map + ggtitle(paste0(" \n", shortener(species)))
    # biotic_map <- biotic_map + ggtitle(paste0(" \n", shortener(species)))
  }

  if (x_type == "temp") {
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
      coord_cartesian(xlim = c(-0.2, 2.6), ylim = c(-3.5, 3.8)) +
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
        raster_limits = c(-0.75, 1.8),
        legend_position = c(0.15, 0.3), make_square = F
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      theme( 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank()
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
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      theme( 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank()
      )

    if (chop_label) {
      climate_map <- climate_map + ggtitle("Temperature trend (scaled)") + 
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
    do_slopes <- chopstick_slopes(model,
      x_variable = "DO_trend_scaled",
      interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
    )

    single_chop <- plot_fuzzy_chopsticks(model,
      x_variable = "DO_trend_scaled", type = "DO", #y_label = "",
      line_size = 1, alpha_range = alpha_range,
      choose_species = stringr::str_replace(species, ".*mature ", ""),
      choose_age = gsub(" .*", "", species),
      slopes = do_slopes # if add, the global slope can be included for insig.
    ) + 
      coord_cartesian(xlim = c(-3, 3), ylim = c(-3.5, 3.8)) + 
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
        mid_fill = "lightyellow", grey_water = F,
        # mid_fill = "lightcyan1", grey_water = F,
        # mid_fill = "honeydew", grey_water = F,
        # mid_fill = "azure", grey_water = F,
        low_fill = "darkcyan",
        axis_lables = T,
        raster_limits = c(-1.6, 1.6),
        legend_position = c(0.15, 0.3), make_square = F
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      theme(
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
    
    climate_map2 <- filter(model$data, species == !!species) %>%
      plot_vocc(
        vec_aes = NULL, grey_water = F,
        fill_col = "mean_DO", fill_label = "mean \nDO",
        raster_cell_size = 4, na_colour = "lightgrey", white_zero = F,
        viridis_option = "D",
        viridis_begin = 0.2,
        axis_lables = T, 
        raster_limits = c(0.69, 5.24),
        legend_position = c(0.15, 0.3), make_square = F
      ) + coord_fixed(xlim = c(180, 790), ylim = c(5370, 6040)) +
      theme( 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
    
    if (chop_label) {
      climate_map <- climate_map + ggtitle("DO trend (scaled)") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"))
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.y = element_blank())
    } else {
      climate_map <- climate_map + ggtitle("()") + 
        theme(plot.margin = margin(0, 0, 0.1, 0, "cm"), 
          plot.title = element_text(colour = "white"), 
          legend.position = "none")
      climate_map2 <- climate_map2 + 
        theme(plot.margin = margin(0, 0, 0, 0, "cm"), 
        legend.position = "none")
    }
  }

  if (!leftmost) {
    single_chop <- single_chop + theme(axis.text.y = element_blank())
  }
  
  (biotic_map / single_chop / climate_map/ climate_map2 + plot_layout(heights = c(2, 0.5, 2, 2)))
  # ggsave(here::here("ms", "figs", paste0("panels-", x_type, "-", species, ".pdf")), width = 4, height = 10)
}

(p1 <- species_panels("mature Sablefish", model, "temp",
  chop_label = T, 
  leftmost = T,
  alpha_range = c(0.1, 0.9)
))

# (p2 <- species_panels("mature Redbanded Rockfish", model, "temp"))

(p3 <- species_panels("immature Canary Rockfish", model, "temp",
  alpha_range = c(0.1, 0.9)
))
(p4 <- species_panels("mature Arrowtooth Flounder", model, "temp"))
# (p4 <- species_panels("mature Pacific Halibut", model, "temp", alpha_range = c(0.25, 0.9)))

(p5 <- species_panels("mature North Pacific Spiny Dogfish", chop_label = T, model, "DO"))

(p6 <- species_panels("mature Bocaccio", model, "DO", alpha_range = c(0.25, 0.9)))
# (p7 <- species_panels("mature Shortspine Thornyhead", model, "DO"))
(p7 <- species_panels("immature Shortspine Thornyhead", model, "DO"))

# wrap_plots(p1, p3, p4, p5, p6) + plot_layout(nrow = 1, ncol = 7, widths = c(1.05, 1, 1, 1, 1), design = layout)

# ygrob <- grid::textGrob(("Predicted % change in biomass per decade"),
#   gp = grid::gpar(fontsize = 12), rot = 90,
#   hjust = -1
# )
# wrap_plots(wrap_elements(ygrob), p1, p3, p4, p5, p6) + plot_layout(nrow = 1, ncol = 7, widths = c(0.05, 1, 1, 1, 1, 1))

layout <- "
      ADEFGHI
      ADEFGHI
      BDEFGHI
      CDEFGHI
      "

ygrob1 <- grid::textGrob(("Predicted % change in biomass per decade"),
  gp = grid::gpar(fontsize = 12),
  hjust = 0.4,
  rot = 90
)

ygrob2 <- grid::textGrob(("Climate trend"),
  gp = grid::gpar(fontsize = 12), hjust = 0, rot = 90
)

ygrob3 <- grid::textGrob(("Mean climate"),
  gp = grid::gpar(fontsize = 12), hjust = 0.25, rot = 90
)

wrap_plots(ygrob1, ygrob2, ygrob3, p1, p3, p4, p5, p6, p7) + plot_layout(design = layout, widths = c(0.05, 1, 1, 1, 1, 1, 1))

ggsave(here::here("ms", "figs", "all-chop-panels-w-means7.pdf"), width = 17, height = 11)


#### OTHER SPECIES PANEL OPTIONS ####
species_panels("mature Arrowtooth Flounder", model, "temp")
species_panels("immature Arrowtooth Flounder", model, "temp")
species_panels("mature Curlfin Sole", model, "temp")
species_panels("mature Flathead Sole", model, "temp")
species_panels("immature Flathead Sole", model, "temp")
species_panels ("mature Dover Sole", model, "temp")
species_panels ("immature Dover Sole", model, "temp")
species_panels("mature English Sole", model, "temp")
species_panels("mature Pacific Halibut", model, "temp")

species_panels("immature English Sole", model, "temp", alpha_range = c(0.25, 0.9))
#
species_panels("mature Pacific Cod", model, "temp", alpha_range = c(0.25, 0.9))
species_panels("mature Walleye Pollock", model, "temp")

# species_panels("mature Canary Rockfish", model, "temp", alpha_range = c(0.25, 0.9))
# species_panels("mature Widow Rockfish", model, "temp")
# species_panels("mature Bocaccio", model, "temp")
# species_panels("mature Shortspine Thornyhead", model, "temp")

# species_panels("mature Sablefish", model, "DO")
# species_panels("mature Pacific Cod", model, "DO")
#
# species_panels("mature Canary Rockfish", model, "DO", alpha_range = c(0.25, 0.9))
# species_panels("mature Yelloweye Rockfish", model, "DO", alpha_range = c(0.25, 0.9))
species_panels("mature Bocaccio", model, "DO", alpha_range = c(0.25, 0.9))
species_panels("mature Pacific Ocean Perch", model, "DO", alpha_range = c(0.25, 0.9))
# species_panels("immature Quillback Rockfish", model, "DO", alpha_range = c(0.25, 0.9))

species_panels("mature Dover Sole", model, "DO", alpha_range = c(0.25, 0.9))

# species_panels("mature Redbanded Rockfish", model, "DO")
# species_panels("mature Widow Rockfish", model, "DO")
species_panels("mature Petrale Sole", model, "DO")
#
#
# species_panels("mature Pacific Halibut", model, "DO")
# species_panels("mature English Sole", model, "DO")
# species_panels("mature Flathead Sole", model, "DO")
# species_panels("mature Arrowtooth Flounder", model, "DO")


#########################
#########################
#### SLOPE SCATTERPLOTS AGAINST DEPTH ####
### prep data ####
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


# # apply labels to deep species and outliers
# do_slopes <- mutate(do_slopes, species_lab = if_else(slope_est < -0.75|depth >270, species, ""))
# temp_slopes <- mutate(temp_slopes, species_lab = if_else(slope_est < -2.25|depth >270, species, ""))

# or just to outliers
do_slopes <- mutate(do_slopes, species_lab = if_else(slope_est < -3, species, ""))
temp_slopes <- mutate(temp_slopes, species_lab = if_else(slope_est < -5, species, ""))


do_slopes$species_lab <- gsub("Rockfish", "", do_slopes$species_lab)
temp_slopes$species_lab <- gsub("Rockfish", "", temp_slopes$species_lab)

##### combined temp-DO panel ####
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
##### quad verion of depth figure ####
temp_high <- slope_scatterplot(
  filter(temp_slopes, chopstick == "high"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5
) +
  # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  geom_point(size = 1, fill = "white") +
  ggrepel::geom_text_repel(aes(label = species_lab),
    size = 3,
    force = 3,
    nudge_y = 0.75,
    nudge_x = 35,
    na.rm = T, min.segment.length = 1
  ) +
  ylab("Slope at highest temperature") +
  scale_y_continuous(breaks = c(3, 0, -3, -6, -9)) +
  coord_cartesian(ylim = c(-11, 4.2), xlim = c(15, 410)) +
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
  )

do_low <- slope_scatterplot(filter(do_slopes, chopstick == "low"), "depth",
  col_group = "age",
  point_alpha = 0.8,
  point_size = 1.5,
) + geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") +
  # geom_linerange(aes(xmin=depth25, xmax = depth75), alpha = 0.2, size = 2) +
  geom_point(size = 1, fill = "white") +
  xlab("Mean depth for species") +
  ylab("Slope at lowest DO") + # guides(colour = F) +
  coord_cartesian(xlim = c(15, 410)) +
  ggrepel::geom_text_repel(aes(label = species_lab),
    size = 3, force = 3, nudge_y = 0.35, nudge_x = 35,
    na.rm = T, min.segment.length = 2
  ) +
  scale_colour_manual(values = c("darkcyan", "darkcyan")) +
  gfplot::theme_pbs() + theme(
    # legend.position = c(.85, .2), legend.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.1, 0, 0.2, "cm"),
    axis.title.y.left = element_text(vjust = 0.2),
    axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
  )

# # add plot tags for tri panel version
temp_high2 <- temp_high %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("a"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
# do_low2 <- do_low %>% egg::tag_facet(open = "", close = ".", tag_pool = c("b"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1)
# depth2 <- depth %>% egg::tag_facet(open = "", close = ".", tag_pool = c("c"),
#   x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1)
#
# # combine and save
# temp_high2 + do_low2 + depth2 + plot_layout(ncol = 1, heights = c(1, 1, 1))
# # + plot_annotation(tag_levels = "a", tag_suffix = ". ")
# # ggsave(here::here("ms", "figs", "slope-by-depth.png"), width = 4.5, height = 7)
# ggsave(here::here("ms", "figs", "slope-by-depth4.png"), width = 5.5, height = 8)


#### QUAD VERSION
### splits temp and do into separate panels
(p_depth_t <- ggplot(do_data, aes(depth, temp)) +
  scale_color_viridis_c(option = "B", end = 0.8) +
  geom_point(aes(depth, temp, colour = temp), alpha = 0.02, shape = 20, size = 0.432) +
  coord_cartesian(xlim = c(15, 410), ylim = c(3.4, 14.4), expand = F) +
  ylab("Mean DO (ml/L)") +
  ylab("Temperature (ºC)") + # , expand = expand_scale(mult = c(0.05, .2)
  geom_smooth(aes(depth, temp), inherit.aes = F, colour = "black", size = 0.5) +
  xlab("Mean depth") +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.1, 0.1, 0.2, "cm"), legend.position = "none",
    axis.title.x = element_blank()
  ))

p_depth_do <- ggplot(do_data, aes(depth, do_est)) +
  scale_color_viridis_c(trans = sqrt, end = 1) +
  geom_point(aes(depth, do_est, colour = do_est), alpha = 0.02, shape = 20, size = 0.432) +
  geom_smooth(colour = "black", size = 0.5) +
  scale_y_continuous(position = "right") +
  coord_cartesian(xlim = c(15, 410), ylim = c(0, 11.5), expand = F) + # ylim = c(0, 8.2),
  ylab("Mean DO (ml/L)") +
  geom_hline(yintercept = 1.8, colour = "black", linetype = "dashed") +
  xlab("Mean depth") +
  gfplot::theme_pbs() + theme(
    plot.margin = margin(0, 0.3, 0.1, 0, "cm"), legend.position = "none",
    axis.title.x = element_blank()
  )

depth_t <- p_depth_t %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("c"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)
depth_do <- p_depth_do %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("d"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

do_low <- do_low + scale_y_continuous(position = "right") + theme(
  # axis.text.y = element_text(colour = "darkcyan"), # "#5E4FA2"),
  axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
  plot.margin = margin(0, 0.1, 0, 0, "cm")
)

do_low3 <- do_low %>% egg::tag_facet(
  open = "", close = ".", tag_pool = c("b"),
  x = Inf, vjust = 1.7, hjust = 1.7, fontface = 1
)

(temp_high2 + do_low3 + depth_t + depth_do + plot_layout(ncol = 2, heights = c(1, 0.5))) / grid::textGrob("Mean depth", just = 0.5, gp = grid::gpar(fontsize = 11)) + plot_layout(nrow = 2, heights = c(1, 0.02))

# ggsave(here::here("ms", "figs", "slope-by-depth-quad-iqr.png"), width = 8, height = 5)
ggsave(here::here("ms", "figs", "slope-by-depth-quad2.png"), width = 8, height = 5)

#########################
#########################
#### COEFFICIENT SCATTERPLOTS AGAINST LIFE HISTORY ####

model2 <- add_colours(model$coefs, species_data = stats)
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
  filter(coefficient %in% c("temp_trend_scaled", "DO_trend_scaled")) %>%
  # transform(coefficient = factor(coefficient,
  mutate(coefficient = factor(coefficient,
    levels = c("temp_trend_scaled", "DO_trend_scaled"),
    labels = c("Temperature", "DO")
  ), Std..Error = `Std. Error`)

trendeffects <- trendeffects %>%
  mutate(coefficient = forcats::fct_reorder(coefficient, Estimate, .desc = F))
# trendeffects <- mutate(trendeffects, rockfish = firstup(rockfish) # didn't work
trendeffects$allspp <- "All species"


### changed to IQR for depth
# cordat <- stats %>% select(species, age, depth, depth_iqr) %>% na.omit()
# cor(cordat$depth_iqr,cordat$depth)

p_depth <- coef_scatterplot(trendeffects,
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
  )
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

ggsave(here::here("ms", "figs", "coef-scatterplots-allspp2.pdf"), width = 7, height = 4)


#########################
#########################
