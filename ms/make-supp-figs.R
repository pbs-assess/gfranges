#########################
#########################
#### SUPPLEMENTARY FIGURES
#########################
#########################
# # if make-figs not just run
# setwd(here::here())
# library(TMB)
# library(tidyverse)
# library(patchwork)
# library(gfranges)
# library(dotwhisker)

#### load appropriate final models and other data
# model <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-27-vel-both-1-200.rds")

model_vel_t <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
model_vel_d <- readRDS("analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")

model_temp <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-22-trend-1-500-temp.rds")
model_grad <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-23-trend-w-grad-1-500-temp.rds")
model_do <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-27-trend-do-only-1-500-DO.rds")
model_age <- readRDS("analysis/VOCC/data/trend-all-95-all-do-04-22-trend-w-age-1-400-temp.rds")

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
#########################
#########################
#### EXPLORE GRADIENTS 
###
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



### Scatterplots of coorelation btw biotic & temperature gradients ####

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
#########################
#########################
#### GLOBAL COEFS 
###
## trend model ####
coef_names <- shortener(unique(model$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", "change in DO", "mean DO", 
  "biomass", "interaction (T)", "interaction (DO)")
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas$model <- "trend"

## trend model with temp only ####
coef_names <- shortener(unique(model_temp$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", "biomass", "interaction (T)")
betas <- signif(as.list(model_temp$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_temp$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_t$model <- "trend (T only)"

## trend model with DO only ####
coef_names <- shortener(unique(model_do$coefs$coefficient))
coef_names <- c("intercept", "change in DO", "mean DO", "biomass", "interaction (DO)")
betas <- signif(as.list(model_do$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_do$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_d <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_d$model <- "trend (DO only)"

## trend model with temp and gradient ####
# no interaction
# model_grad <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-23-trend-grad-1-500-temp.rds")
# coef_names <- shortener(unique(model_grad$coefs$coefficient))
# coef_names <- c("intercept", "change in T", "mean T", "gradient", "biomass", "interaction (T)")
# # version with 2-way interactions
# model_grad <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-23-trend-w-grad-1-500-temp-2-way.rds")
# coef_names <- shortener(unique(model_grad$coefs$coefficient))
# coef_names <- c("intercept", "change in T", "mean T", "gradient", "biomass", "interaction (T)",  "mean T:gradient",  "change in T:gradient") 

# # version with 3-way interaction
coef_names <- shortener(unique(model_grad$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", 
  "gradient", "biomass", "interaction (T)",
  "mean T:gradient",  "change in T:gradient", "interaction (T):gradient")

betas <- signif(as.list(model_grad$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_grad$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_g <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_g$model <- "trend (T w gradient)"


## velocity model ####
coef_names <- shortener(unique(model_vel$coefs$coefficient))
coef_names <- c("intercept", "change in T", "change in DO", "mean T", "mean DO", 
  "biomass", "interaction (T)", "interaction (DO)")
betas <- signif(as.list(model_vel$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_vel <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel$model <- "velocity"

## temperature velocity model ####
coef_names <- shortener(unique(model_vel_t$coefs$coefficient))
coef_names <- c("intercept", "change in T", "mean T", 
  "biomass", "interaction (T)")
betas <- signif(as.list(model_vel_t$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel_t$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_vel_t <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_t$model <- "velocity (T only)"

## DO velocity model ####
coef_names <- shortener(unique(model_vel_d$coefs$coefficient))
coef_names <- c("intercept", "change in DO", "mean DO", 
  "biomass", "interaction (DO)")
betas <- signif(as.list(model_vel_d$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_vel_d$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_vel_d <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_vel_d$model <- "velocity (DO only)"

## trend model with age effect ####

# version with 3-way interactions
coef_names <- shortener(unique(model_age$coefs$coefficient))
coef_names <- c("intercept", "immature", "change in T", "mean T", "change in DO", "mean DO", 
  "biomass", "interaction (T)", "interaction (DO)", "change in T:immature", "mean T:immature", 
  "change in DO:immature", "mean DO:immature", "interaction (T):immature", "interaction (DO):immature")

betas <- signif(as.list(model_age$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_age$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_age <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_age$model <- "trend (3-way interactions)"

# # version with only 2-way interactions
model_age2 <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-22-trend-w-age-1-500-DO.rds")
coef_names <- shortener(unique(model_age2$coefs$coefficient))
coef_names <- c("intercept", "immature", "change in T", "mean T", "change in DO", "mean DO",
"biomass", "interaction (T)", "interaction (DO)", "change in T:immature", "mean T:immature", "change in DO:immature", "mean DO:immature")
betas <- signif(as.list(model_age2$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model_age2$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- as.double(signif(betas + SE * qnorm(0.025), digits = 3))
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas_age2 <- cbind.data.frame(coef_names, betas, SE, lowerCI, upperCI)
overall_betas_age2$model <- "trend (w maturity effect)"

### plot global coefs for multiple models #### 
overall_betas$model_type <- "trend"
overall_betas_t$model_type <- "trend"
overall_betas_d$model_type <- "trend"
overall_betas_g$model_type <- "trend"
overall_betas_vel$model_type <- "velocity"
overall_betas_vel_t$model_type <- "velocity"
overall_betas_vel_d$model_type <- "velocity"
overall_betas_age$model_type <- "trend"
overall_betas_age2$model_type <- "trend"

custom_order <- c("intercept", "immature", "biomass", 
  "gradient",
  "mean T", "mean T:immature", "mean T:gradient",
  "change in T",  "change in T:immature",  "change in T:gradient",
  "interaction (T)", "interaction (T):immature",  "interaction (T):gradient",
  "mean DO",  "mean DO:immature",
  "change in DO", "change in DO:immature", 
  "interaction (DO)", "interaction (DO):immature"
  )

# allcoefs2 <- allcoefs %>% rename(term = coefficient, estimate = Estimate, std.error = Std..Error) %>% filter(term != "intercept")

### compare trends and velocities
overall <- rbind.data.frame(overall_betas, overall_betas_t, overall_betas_g, overall_betas_d, 
  overall_betas_vel, overall_betas_vel_t, overall_betas_vel_d)
overall <- mutate(overall, term = firstup(as.character(coef_names)))
overall2 <- overall %>% rename(
  estimate = betas, std.error = SE) #%>% filter(term != "intercept")
overall2 <- overall2 %>% mutate(term = factor(term, 
  levels = firstup(as.character(custom_order))), 
  model = firstup(as.character(model)))

# overall2[is.na(overall2)] <- 0 
global_vel <- dotwhisker::dwplot(overall2#, 
  # order_vars = custom_order
  ) + #xlim(-10,10) +
  geom_vline(xintercept = 0, colour = "darkgray", alpha=0.5) +
  # geom_point(aes(term, estimate,  colour = model), alpha= 0.1, position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) + 
  scale_colour_manual(name = "Models", 
    values = c("#D53E4F", 
    "#F46D43", 
    "#FDAE61",
    "#FEE08B",
    "#ABDDA4",  "#3288BD", "#5E4FA2"
    )) + ggtitle("a. Trend versus velocity models") +
  gfplot::theme_pbs() + theme (#legend.title = element_blank(),
    legend.position = c(0.75, 0.25))
global_vel
ggsave(here::here("ms", "figs", "supp-global-coefs-vel.pdf"), width = 5, height = 4)

# look for sig age effects 
overall3 <- rbind.data.frame(overall_betas, overall_betas_age2, overall_betas_age)
overall3 <- overall3 %>% mutate(term = firstup(as.character(coef_names))) %>% rename(
  estimate = betas, std.error = SE) %>% mutate(term = factor(term, 
  levels = firstup(as.character(custom_order))), 
  model = firstup(as.character(model)))

global_age <- dotwhisker::dwplot(overall3#, 
  # order_vars = custom_order
) + #xlim(-10,10) +
  geom_vline(xintercept = 0, colour = "darkgray") +
  # geom_point(aes(term, estimate,  colour = model), alpha= 0.1, position = position_jitter(width = 0.25), inherit.aes = F, data = allcoefs2) + 
  scale_colour_manual(name = "Models", 
    values = c("#D53E4F", "#F46D43", 
    "#FDAE61"#, "#FEE08B", "#3288BD", "#5E4FA2"
    )) + ggtitle("b. Global maturity effects") +
  scale_y_discrete(position = "right") +
  gfplot::theme_pbs() + theme (legend.title = element_blank(),
    legend.position = c(0.25, 0.145))
# globel_age
# ggsave(here::here("ms", "figs", "supp-global-coefs-w-age.pdf"), width = 5, height = 4)

global_vel + global_age + plot_layout()
ggsave(here::here("ms", "figs", "supp-global-coefs.pdf"), width = 10, height = 5)


#########################
#### experiment with species level coefs as boxplots ####
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




#########################
#########################
#########################
#### ALL CHOPSTICKS AND SLOPE WORM PLOTS 
###
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


### if just temp model... no change in chopsticks ####
temp_slopes <- chopstick_slopes(model_temp,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
temp_slopes <- left_join(temp_slopes, stats)
p2 <- plot_fuzzy_chopsticks(model_temp,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass", 
  # choose_age = "mature",
  slopes = temp_slopes  
) + coord_cartesian(ylim=c(-11,7)) + 
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(temp_slopes, type = "temp", 
  legend_position = c(.25,.95)) + 
  ylab("Slopes")

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 
ggsave(here::here("ms", "figs", "supp-trend-chopsticks-temp-only.pdf"), width = 14, height = 5.5)

### if just temp with gradient model... no change in chopsticks ####
temp_slopes <- chopstick_slopes(model_grad,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
temp_slopes <- left_join(temp_slopes, stats)
p2 <- plot_fuzzy_chopsticks(model_grad,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass", 
  # choose_age = "mature",
  slopes = temp_slopes  
) + coord_cartesian(ylim=c(-11,7)) + 
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(temp_slopes, type = "temp", 
  legend_position = c(.25,.95)) + 
  ylab("Slopes")

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 
ggsave(here::here("ms", "figs", "supp-trend-chopsticks-temp-grad.pdf"), width = 14, height = 5.5)

### ALL VELOCITY CHOPS ####
temp_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_temp_vel_scaled",
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp"
) %>%
  mutate(sort_var = slope_est)

do_vel_slopes <- chopstick_slopes(model_vel,
  x_variable = "squashed_DO_vel_scaled",
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO"
) %>%
  mutate(sort_var = slope_est)

p_temp_vel_chops <- plot_fuzzy_chopsticks(model_vel,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = "Predicted mature biomass vel",
  slopes = temp_vel_slopes # if add, the global slope can be included for insig
) + coord_cartesian(xlim = c(-0.25, 4), ylim = c(-30, 37)) +
  xlab("Temperature velocity (scaled)") + theme(legend.position = "none")

p_do_vel_chops <- plot_fuzzy_chopsticks(model_vel,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  y_label = "Predicted mature biomass vel",
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim = c(-30, 37)) +
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
#########################
#########################
#### SLOPE SCATTERPLOTS 
###
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
  xlab("Mean age") +
  ylab("Slope") + 
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
  # xlab("Mean age") +
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
#########################
#########################
#### COEFFICIENT SCATTERPLOTS
###
### investigate mean age ####
## when mean age is less, than negative temperature effects are more likely?
p_age_alone <- coef_scatterplot(
  trendeffects,
  # filter(trendeffects, age == "immature"),
  coef = c("temperature", "DO"),
  x = "age_mean", group = "age", regression = F
) +
  xlab("Mean age") +
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
  ylab("Trend coefficient")
p_age_alone
ggsave(here::here("ms", "figs", "supp-coef-by-mean-age.pdf"), width = 3.5, height = 2.7)



#########################
#########################
#########################
#### SPATIAL RESIDUALS
###
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


### for velocity model ####
data2 <- model_vel$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual))

data2$species_only[data2$species_only == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"

data2 %>% filter(age_class == "mature") %>% 
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() + 
  theme(legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
  facet_wrap(~species_only, strip.position = "bottom") +
  ggtitle("Mature fish biomass velocity residuals")
ggsave(here::here("ms", "figs", "supp-mat-vel-residuals.pdf"), width = 7, height = 7)

data %>% filter(age_class == "immature") %>% 
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() + 
  theme(legend.position = "bottom", panel.spacing = unit(0, "lines"), strip.text = element_text(size = 5), 
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
  facet_wrap(~species_only, strip.position = "bottom") + 
  ggtitle("Immature fish biomass trend residuals")
ggsave(here::here("ms", "figs", "supp-imm-vel-residuals.pdf"), width = 7, height = 6)
