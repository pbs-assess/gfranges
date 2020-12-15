library(TMB)
library(dplyr)
library(ggplot2)
library(gfranges)
setwd(here::here())
compile("analysis/VOCC/vocc_regression.cpp")
dyn.load(dynlib("analysis/VOCC/vocc_regression"))
source("analysis/VOCC/vocc-regression-functions.R")

# stats <- readRDS(paste0("data/life-history-behav.rds"))
stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth2.rds"))

##############################
#### ONE JUST BUILT
model <- new_model

#### LOAD MODELS ####
model <- readRDS(here::here("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-1-600.rds"))
model <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-1-600.rds"))
model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-both-group-1-600-DO.rds")
# model2 <- add_colours(model$coefs, col_var = "family") 
# model2 <- add_colours(model$coefs, col_var = "higher_taxa") 
model2 <- add_colours(model$coefs, col_var = "mean_group") 
model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

manipulate::manipulate({
  plot_coefs(model2, grid_facets = T, fixed_scales = F, grouping_taxa = "species_id", #add_grey_bars = T,
    order_by = order_by) #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker(
  as.list(sort(unique(shortener(model2$coefficient))), decreasing=F))
)


model <- readRDS(here::here("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-1-600.rds"))
model <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-1-600.rds"))

## nulls that are currently illustrated
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-sim-2-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-4-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-5-600.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-29-vel-both-sim-6-600.rds")
# model2 <- add_colours(model$coefs, col_var = "mean_group") 
# model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
# plot_coefs(model2, grouping_taxa = "species_id", order_by = "temp_vel", grid_facets = T, fixed_scales = F)
# # ggsave(here::here("ms", "figs", "supp-all-vel-coefs.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null4nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null3nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null2nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-vel-coefs-null1nb.pdf"), width = 12.5, height = 8.5)
# 
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-2-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-4-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-5-600.rds")
# model <- readRDS("analysis/VOCC/data/trend-all-95-optimized4-11-30-trend-with-do-sim-6-600.rds")
# model2 <- add_colours(model$coefs, col_var = "mean_group") 
# model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"
# plot_coefs(model2, grouping_taxa = "species_id", order_by = "temp_trend", grid_facets = T, fixed_scales = F)
# # ggsave(here::here("ms", "figs", "supp-all-trend-coefs.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null4nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null3nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null2nb.pdf"), width = 12.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-all-trend-coefs-null1nb.pdf"), width = 12.5, height = 8.5)

#### fishing model ####
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-19-vel-w-catch-main-only-1-400.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-20-vel-both-family-1-350.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-20-vel-both-1-350.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-21-vel-both-1-400.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-19-vel-w-fishing-1-400.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-19-vel-w-catch-1-400.rds")
# model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-w-catch-1-400.rds")
model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-w-fishing-1-600.rds") #best grad so far
model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-w-fishing-1-600.rds")
model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-w-catch-1-500.rds")
max(model$sdr$gradient.fixed)


model <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-11-vel-fishing-only-1-600.rds")

model2 <- add_colours(model$coefs, col_var = "mean_group") %>%   
  # filter(coefficient %in% c("log_biomass_scaled", "log_effort_scaled", "fishing_trend_scaled","log_effort_scaled:fishing_trend_scaled"))
  # filter(coefficient %in% c("log_biomass_scaled", "log_effort_scaled", "fishing_vel_scaled","log_effort_scaled:fishing_vel_scaled"))
filter(coefficient %in% c(
  # "log_biomass_scaled",
  "log_catch_scaled", "catch_trend_scaled",
  "catch_vel_scaled", "fishing_vel_scaled",
"log_effort_scaled", "fishing_vel_scaled","log_effort_scaled:fishing_vel_scaled",
  "log_catch_scaled:fishing_vel_scaled",
  "log_catch_scaled:catch_vel_scaled"))

model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

manipulate::manipulate({
  plot_coefs(model2, grid_facets = T, fixed_scales = F, grouping_taxa = "species_id", #add_grey_bars = T, 
    order_by = order_by) #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model2[model2$age=="mature",]$coefficient))), decreasing=F))
)

ggsave(here::here("ms", "figs", "supp-fishing-interation-coefs-600.pdf"), width = 8.5, height = 8.5)
# ggsave(here::here("ms", "figs", "supp-catch-interaction-coefs-600.pdf"), width = 8.5, height = 9)
# ggsave(here::here("ms", "figs", "supp-catch-only-coefs-400.pdf"), width = 5.5, height = 8.5)
ggsave(here::here("ms", "figs", "supp-catch-main-coefs-500.pdf"), width = 8.5, height = 9)


#### sort by traits ####
model2 <- add_colours(model$coefs) %>%   
  filter(coefficient %in% c("temp_trend_scaled", "DO_trend_scaled", 
    "temp_trend_scaled:mean_temp_scaled", "DO_trend_scaled:mean_DO_scaled"))

model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

plot_coefs(model2, grid_facets = T, fixed_scales = F, 
  #add_grey_bars = T, 
  order_by = "depth") 

manipulate::manipulate({
  plot_coefs(model2, grid_facets = T, fixed_scales = F, 
    #add_grey_bars = T, 
    order_by_trait = T,
    increasing = T,
    order_by = order_by) #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker(as.list(names(model2)[8:20])))



#### GLOBAL COEFS ####
coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)

#### RESIDUALS ####
r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i
model$data$eta <- r$eta_i

ggplot(model$data, aes(eta, residual)) + geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") + facet_wrap(~species, scales = "free")

model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + gfplot::theme_pbs() +
  facet_wrap(~species)

#### WITH AGE EFFECT ####
# Just mature

# model_age <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-23-vel-w-age-1-400.rds")
model2 <- add_colours(model_age$coefs) 

model2$species[model2$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

unique(model2$coefficient)
model3 <- model2 %>% filter(age == "mature") %>%
  filter(coefficient != "age") %>%
  filter(coefficient != "age:temp_trend_scaled") %>%
  filter(coefficient != "age:mean_temp_scaled" ) %>%
  filter(coefficient != "age:DO_trend_scaled") %>%
  filter(coefficient != "age:mean_DO_scaled"  ) %>%
  filter(coefficient != "age:temp_trend_scaled:mean_temp_scaled" ) %>%
  filter(coefficient != "age:DO_trend_scaled:mean_DO_scaled") %>%   
  
  filter(coefficient != "age:squashed_temp_vel_scaled") %>%
  filter(coefficient != "age:squashed_DO_vel_scaled") %>%
  filter(coefficient != "age:squashed_temp_vel_scaled:mean_temp_scaled" ) %>%
  filter(coefficient != "age:squashed_DO_vel_scaled:mean_DO_scaled") %>%   
  filter(coefficient != "(Intercept)") %>%
  filter(coefficient != "log_biomass_scaled" )
                
manipulate::manipulate({
  plot_coefs(model3, fixed_scales = F, order_by = order_by) 
  #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model2$coefficient))), decreasing=F))
)

plot_coefs(model3, order_by = "temp_vel", fixed_scales = F)

ggsave(here::here("ms", "figs", "supp-coefs-mat.pdf"), width = 8, height = 10)


# Just immature

model4 <- model2 %>% filter(coefficient != "log_biomass_scaled" )

model4 <- model2 %>% filter(age == "immature") %>%
  filter(coefficient != "temp_trend_scaled") %>%
  filter(coefficient != "mean_temp_scaled" ) %>%
  filter(coefficient != "DO_trend_scaled") %>%
  filter(coefficient != "mean_DO_scaled"  ) %>%
  filter(coefficient != "temp_trend_scaled:mean_temp_scaled" ) %>%
  filter(coefficient != "DO_trend_scaled:mean_DO_scaled") %>%
  filter(coefficient != "squashed_temp_vel_scaled") %>%
  filter(coefficient != "squashed_DO_vel_scaled") %>%
  filter(coefficient != "squashed_temp_vel_scaled:mean_temp_scaled" ) %>%
  filter(coefficient != "squashed_DO_vel_scaled:mean_DO_scaled") %>%
  filter(coefficient != "(Intercept)") %>%
  filter(coefficient != "log_biomass_scaled" )

manipulate::manipulate({
  plot_coefs(filter(model4, age == "immature"), grid_facets = T, fixed_scales = F, order_by = order_by) 
  #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model2$coefficient))), decreasing=F))
)

plot_coefs(model4, order_by = "age", fixed_scales = F)

ggsave(here::here("ms", "figs", "supp-coefs-imm.pdf"), width = 8, height = 10)



#### GENUS COEF PLOTS ####

model3 <- add_colours(model$coefs_genus, col_var = "genus", add_spp_data = F) 
colour_list <- unique(model3$colours)
manipulate::manipulate({
  plot_coefs(model3, grouping_taxa = "genus", fixed_scales = F, order_by = order_by) 
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model3$coefficient))), decreasing=F))
)

#### FAMILY COEF PLOTS ####

# classes and orders
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-18-vel-both-order-1-300.rds")

# families 
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-20-vel-both-family-1-350.rds")
model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-family-1-600.rds") # not quite converged

# ecological/taxanomic groups
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-19-vel-both-group-1-400.rds")
# model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized3-11-23-vel-both-group-1-400.rds")

model_vel <- readRDS("analysis/VOCC/data/vel-all-95-optimized4-12-01-vel-both-group-1-600.rds")


model_vel$coefs_genus$genus <-  stringr::str_to_title(model_vel$coefs_genus$genus)
model3 <- add_colours(model_vel$coefs_genus, col_var = "genus", add_spp_data = F) %>% filter(coefficient != "log_biomass_scaled") %>% filter(coefficient != "(Intercept)")
colour_list <- unique(model3$colours)
manipulate::manipulate({
  plot_coefs(model3, grouping_taxa = "genus", fixed_scales = F, order_by = order_by) 
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model3$coefficient))), decreasing=F))
)
plot_coefs(model3, grouping_taxa = "genus", order_by = "temp_vel", fixed_scales = F) + theme(legend.position = "none")

# ggsave(here::here("ms", "figs", "supp-coefs-family-350.pdf"), width = 8, height = 6)
# ggsave(here::here("ms", "figs", "supp-coefs-family-600.pdf"), width = 7, height = 5)
ggsave(here::here("ms", "figs", "supp-coefs-group-600.pdf"), width = 8, height = 5)


#### CONTRAST COEFFICIENTS WITH LIFE HISTORY ####

manipulate::manipulate({filter(model2, coefficient == x) %>%
    ggplot(aes(depth, Estimate)) + geom_point() + 
    geom_smooth(method = "lm") + ylab(paste(x)) + facet_wrap(~age)}, 
  x = manipulate::picker(as.list(sort(unique(model2$coefficient)), decreasing=F))
)

model2 %>%
  ggplot(aes(depth, Estimate, colour = group)) + 
  geom_point() + 
  geom_smooth(data = filter(model2, 
    group == "FLATFISH" | group == "ROCKFISH"), 
    aes(depth, Estimate, colour = group), 
    method = "lm", inherit.aes = F) + 
  ylim(-3, 3) +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(age~coefficient) + gfplot:::theme_pbs()

model2 %>%
  ggplot(aes(length_50_mat_m, Estimate, colour = group)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ylim(-3, 3) +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(age~coefficient) + gfplot:::theme_pbs()

model2 %>%
  ggplot(aes(length_50_mat_m, Estimate, colour = group)) + 
  geom_point() + 
  geom_smooth(data = filter(model2, 
    group == "FLATFISH" | group == "ROCKFISH"), 
    aes(length_50_mat_m, Estimate, colour = group), 
    method = "lm", inherit.aes = F) + 
  ylim(-3, 3) +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(age~coefficient) + gfplot:::theme_pbs()


model2 %>% filter(
    coefficient == "temp_trend_scaled"|
    coefficient == "mean_temp_scaled"|
    coefficient == "temp_trend_scaled:mean_temp_scaled" ) %>% 
  mutate(coefficient= shortener(coefficient)) %>%
  ggplot(aes(group, Estimate, colour = group)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, colour = "darkgray") + 
  xlab("") + guides(colour = F) +
  coord_flip(ylim= c(-2.5, 1.5)) + #
  # scale_y_continuous(trans = fourth_root_power) +
  # scale_colour_manual(values = colour_list) +
  facet_grid(coefficient~age, scales = "free") + 
  gfplot:::theme_pbs()

colour_list <- unique(model2$colours)
model2 %>% filter(
    coefficient == "DO_trend_scaled"|
    coefficient == "mean_DO_scaled"|
    coefficient == "DO_trend_scaled:mean_DO_scaled" ) %>% 
  mutate(coefficient= shortener(coefficient)) %>%
  # ggplot(aes(forcats::fct_reorder(group, Estimate, length, .desc=F), Estimate, colour = forcats::fct_reorder(group, Estimate, length, .desc=F))) + 
  ggplot(aes(group, Estimate, colour = group)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, colour = "darkgray") + 
  xlab("") + guides(colour = F) +
  # geom_smooth(data = filter(model2, 
  # group == "FLATFISH" | group == "ROCKFISH"), 
  # aes(length_50_mat_m, Estimate, colour = group), 
  # method = "lm", inherit.aes = F) + 
  # scale_color_brewer(palette = "Spectral") +
  scale_color_viridis_d(direction = -1) +
  # scale_colour_manual(values = unique(model2$colours)) +
  coord_flip(ylim= c(-1, 0.7)) + #
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(coefficient~age, scales = "free") + 
  gfplot:::theme_pbs() 

model2 %>%
  ggplot(aes(age_max, Estimate)) + geom_point() +
  geom_smooth(method = "lm") + ylim(-3, 3) +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(age~coefficient)

model2 %>%
  ggplot(aes(log(weight_99th), Estimate)) + geom_point() +
  geom_smooth(method = "lm") +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(age~coefficient)


#### DEPTH

model2$group[model2$group == "DOGFISH"] <- "SHARKS & SKATES"
model2$group[model2$group == "SKATE"] <- "SHARKS & SKATES"
model2 <- model2 %>% group_by(group) %>% mutate(spp_count = length(unique(species))) %>% ungroup()
model2 <- model2 %>% mutate(group = forcats::fct_reorder(group, Estimate, .desc=F))




#### Length at maturity
#### MALE
# coef_scatterplot(model2, coef = "temp_trend_scaled", x = "length_50_mat_m") +
coef_scatterplot(model2, coef = "squashed_temp_vel_scaled", x = "length_50_mat_m") +
  # coord_cartesian(ylim= c(-3, 3)) +
  facet_grid(rockfish~age, scales = "free")

# coef_scatterplot(model2, coef = "DO_trend_scaled", x = "length_50_mat_m", 
#   regression = F) + 
coef_scatterplot(model2, coef = "squashed_DO_vel_scaled", x = "length_50_mat_m", 
  regression = F) + 
  # coord_cartesian(ylim= c(-1, 1)) +
  facet_grid(rockfish~age, scales = "free") 

coef_scatterplot(model2, coef = "mean_temp_scaled", x = "length_50_mat_m") + 
  #coord_cartesian(ylim= c(-1, 1))+ 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(model2, coef = "mean_DO_scaled", x = "length_50_mat_m") + 
  # coord_cartesian(ylim= c(-1, 1)) +
  facet_grid(rockfish~age, scales = "free")


# for immature rockfish, much more negative interaction with larger size at maturity
coef_scatterplot(filter(model2, depth > 150), 
  # coef = "temp_trend_scaled:mean_temp_scaled", 
  coef = "squashed_temp_vel_scaled:mean_temp_scaled", 
  x = "length_50_mat_m", group="group", regression = T) + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth > 120), 
  coef = "squashed_DO_trend_scaled:mean_DO_scaled", 
  x = "length_50_mat_m", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free") +
  gfplot:::theme_pbs()

coef_scatterplot(filter(model2, depth > 120), coef = "DO_trend_scaled", 
  x = "length_50_mat_m", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free") + 
  gfplot:::theme_pbs()

coef_scatterplot(filter(model2, depth < 200), coef = "DO_trend_scaled:mean_DO_scaled", 
  x = "length_50_mat_m", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth < 200), coef = "DO_trend_scaled", 
  x = "length_50_mat_m", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")


#### FEMALE
coef_scatterplot(model2, coef = "temp_trend_scaled", x = "length_50_mat_f", 
  regression = F) + 
  #coord_cartesian(ylim= c(-1, 1))+ 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(model2, coef = "DO_trend_scaled", x = "length_50_mat_f", 
  regression = F) + 
  #coord_cartesian(ylim= c(-1, 1))+ 
  facet_grid(rockfish~age, scales = "free")


coef_scatterplot(model2, coef = "mean_temp_scaled", x = "length_50_mat_f") + 
  #coord_cartesian(ylim= c(-1, 1))+ 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(model2, coef = "mean_DO_scaled", x = "length_50_mat_f") + 
  coord_cartesian(ylim= c(-1, 1)) +
  facet_grid(rockfish~age, scales = "free")

# coef_scatterplot(filter(model2, group == "FLATFISH" | group == "ROCKFISH"),
#   coef = "mean_DO_scaled", x = "length_50_mat_f") + 
#   scale_color_viridis_d(begin = 0.2, end = 0.7) +
#   facet_grid(group~age) + coord_cartesian(ylim= c(-1, 1))


#### AGE 
coef_scatterplot(model2, coef = "temp_trend_scaled", x = "age_max", group = "group") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  scale_color_brewer(palette = "Dark2") +
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(model2, coef = "DO_trend_scaled", x = "age_max", regression = F) +
  # coord_cartesian(ylim= c(-1, 1)) +
  facet_grid(~rockfish, scales = "free") 

# coef_scatterplot(model2, coef = "temp_trend_scaled:mean_temp_scaled", x = "age_max", group="group", regression = T) + 
#   # coord_cartesian(ylim= c(-3, 3)) + 
#   facet_grid(rockfish~age, scales = "free")
# 
# coef_scatterplot(model2, coef = "DO_trend_scaled:mean_DO_scaled", x = "age_max", group="group" , regression = T) + 
#   # coord_cartesian(ylim= c(-3, 3)) + 
#   facet_grid(rockfish~age, scales = "free")

# for immature rockfish, much more negative interaction with larger size at maturity
coef_scatterplot(filter(model2, depth > 150), coef = "temp_trend_scaled:mean_temp_scaled", 
  x = "age_max", group="group", regression = T) + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth > 120), coef = "DO_trend_scaled:mean_DO_scaled", 
  x = "age_max", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth > 120), coef = "DO_trend_scaled", 
  x = "age_max", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth < 200), coef = "DO_trend_scaled:mean_DO_scaled", 
  x = "age_max", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")

coef_scatterplot(filter(model2, depth < 200), coef = "DO_trend_scaled", 
  x = "age_max", group="group" , regression = F) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  # coord_cartesian(ylim= c(-3, 3)) + 
  facet_grid(rockfish~age, scales = "free")



#### 


p_depth <- coef_scatterplot(model2, coef = c("temp_trend_scaled","DO_trend_scaled"), 
  x = "depth", group = "age", regression = T) + ylab("coefficient") +
  # coord_cartesian(ylim= c(-1, 1)) +
  # geom_smooth(method = "lm") +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + ggtitle("ALL SPECIES") +
  theme(strip.background = element_blank(), 
    strip.text = element_blank(), 
    # axis.text.y = element_blank(), 
    axis.ticks = element_blank()) + 
  facet_grid(rows = vars(coefficient), scales = "free") 

p_age <- coef_scatterplot(model2, coef = c("temp_trend_scaled","DO_trend_scaled"), 
  x = "age_max", group = "age", regression = T) + ylab("")+
  # coord_cartesian(ylim= c(-1, 1)) +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + 
  # ggtitle("") +
  theme(strip.background = element_blank(), 
    strip.text.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank()) +
  facet_grid(rows = vars(coefficient), cols = vars(rockfish), scales = "free") 

p_mat <- coef_scatterplot(model2, coef = c("temp_trend_scaled","DO_trend_scaled"), 
  x = "length_50_mat_f", group = "age", regression = F) + ylab("")+
  # coord_cartesian(ylim= c(-1, 1)) +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  # guides(colour=F) +
  theme(strip.background = element_blank(), 
    legend.position = c(.25,.65),
    # strip.text = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank()) +
  facet_grid(coefficient~rockfish, scales = "free") 

# cowplot::plot_grid(p_depth, p_age, p_mat, nrow = 1, rel_widths = c(1, .9 , 1.75)) 
cowplot::plot_grid(p_depth, p_age, p_mat, nrow = 1, rel_widths = c(1.1, 1.75 , 1.75)) 



coefdata <- model2 %>% rename(longevity = age_max, 
  'length at maturity' = length_50_mat_f) %>% 
  pivot_longer(cols = c("depth", "longevity", "length at maturity"), 
    names_to = "trait")

p_do <- coef_scatterplot(coefdata, coef = c("DO_trend_scaled"), 
  x = "value", group = "age", regression = T) + 
  # coord_cartesian(ylim= c(-1, 1)) +
  # geom_smooth(method = "lm") +
  scale_colour_viridis_d(begin = .8 , end =.2) +
  guides(colour=F) + ggtitle("ALL SPECIES") +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"), strip.background = element_blank(), 
    strip.text = element_blank(), 
    # axis.text.y = element_blank(), 
    axis.ticks = element_blank()) + 
  facet_grid(cols = vars(trait), rows = vars(rockfish), scales = "free") 



filter(model2, coefficient == "temp_trend_scaled"|coefficient == "DO_trend_scaled") %>%
    ggplot(aes_string(, "Estimate", colour = group)) + 
    geom_point() + scale_color_viridis_d(direction = 1) +
    # scale_y_continuous(trans = fourth_root_power) +
    ylab(coef) + facet_wrap(~age) + gfplot:::theme_pbs() 
  if (regression) {
    p <- p + geom_smooth(method = "lm", colour = "darkgray", fill = "lightgray") 
}
  facet_grid(~rockfish, scales = "free")




coef_scatterplot(model2, coef = "mean_temp_scaled", x = "age_max") + 
  #coord_cartesian(ylim= c(-1, 1))+ 
  facet_grid(rockfish~age, scales = "free")

# coef_scatterplot(model2, coef = "mean_DO_scaled", x = "age_max") + 
#   coord_cartesian(ylim= c(-1, 1)) +
#   facet_grid(rockfish~age, scales = "free")


# coef_scatterplot(model2, coef = "temp_trend_scaled:mean_temp_scaled", x = "age_max") +
#   #coord_cartesian(ylim= c(-1, 1))+ 
#   facet_grid(rockfish~age, scales = "free")

# coef_scatterplot(model2, coef = "DO_trend_scaled:mean_DO_scaled", x = "age_max") + 
#   #coord_cartesian(ylim= c(-1, 1))+ 
#   facet_grid(rockfish~age, scales = "free")


# manipulate::manipulate({
#   filter(model2, coefficient == "temp_trend_scaled" & age == "immature") %>%
#     ggplot(aes(x, Estimate)) + geom_point()}, 
#   x = manipulate::picker(sort(unique(names(model2))), decreasing=F)
# )


##############################
#### CHECK SAMPLE SIZE AND DISTRIBUTION OF MODEL DATA
##############################
# nrow(model$data)
# mean(model$data$mean_biomass)
# range(model$data$mean_biomass)
# hist(log(model$data$mean_biomass))
# hist((model$data$biotic_trend))
# hist((model$data$temp_trend))


##############################
#### MODEL COEFFICIENTS ####
##############################

coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)

##############################
#### CHECK MODEL RESIDUALS ####
##############################
# # use 6-vocc-regression code to save pdf of all relevant plots

ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(trans = fourth_root_power) + gfplot::theme_pbs() +
  facet_wrap(~species)

# # ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# # ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)


if (is.null(model$data$residual)){
r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i
model$data$eta <- r$eta_i
}

 ggplot(model$data, aes(eta, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(biotic_trend)) + geom_histogram() + 
    facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(temp_trend, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free") 
  
  ggplot(model$data, aes(DO_trend, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_temp, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_DO, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_temp^2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_DO^2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_biomass_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_biomass_scaled2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(fishing_trend_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_effort_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  model$data %>%
    mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
    mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
    mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
    mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
    ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2() + gfplot::theme_pbs() +
    facet_wrap(~species)


if(y_type == "vel") {

  ggplot(model$data, aes(squashed_biotic_vel)) + geom_histogram() +   
    facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(squashed_temp_vel, residual)) + geom_point(alpha =0.2) + 
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free") 
  
  ggplot(model$data, aes(squashed_DO_vel, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
}


norm_resids <- qres_student(model)
norm_resids <- norm_resids[is.finite(norm_resids)]
qqnorm(norm_resids)
hist(norm_resids)
 
# qqnorm(model$data$residual)
# qqline(model$data$residual)
