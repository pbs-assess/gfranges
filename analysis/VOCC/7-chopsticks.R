##############################
##############################
#### CHOPSTICK PLOTS
##############################
##############################
library(TMB)
library(dplyr)
library(ggplot2)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

##############################
#### LOAD MODELS ####

#### ONE JUST BUILT
model <- new_model

#### ADULTS AND IMMATURE COMBINED IN ONE MODEL
model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-04-04-trend-with-do-1-500.rds")

# ### SORT BY SINGLE LIFE HISTORY STAT
# # stat <- readRDS(paste0("data/life-history-stats.rds")) %>% View()
# stats <- readRDS(paste0("data/life-history-stats.rds")) %>%
#   mutate(sort_var = -depth) %>%
#   # mutate(sort_var = depth_diff) %>%
#   # mutate(sort_var = length_99th) %>%
#   select(species, sort_var)
# ### #### #### #### #### #### #### 


temp_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
temp_slopes <- left_join(temp_slopes, stats)
temp_slopes <- temp_slopes %>% mutate(sort_var = slope_est)
# temp_slopes <- filter(temp_slopes, age == "mature")
p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = "Predicted % change in biomass", 
  # choose_age = "mature",
  slopes = temp_slopes  # if add, the global slope can be included for insig.
) + coord_cartesian(ylim=c(-11,7)) + 
  xlab("Temperature trend (scaled)") + theme(legend.position = "none")

temp_slopes$species[temp_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(temp_slopes, type = "temp", #hack= T,
  legend_position = c(.25,.95)) + 
  ylab("Slopes")

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 

#### #### #### #### #### #### #### 

do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
do_slopes <- left_join(do_slopes, stats)
do_slopes <- do_slopes %>% mutate(sort_var = slope_est)

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = "Predicted % change in biomass",
  slopes = do_slopes
) + coord_cartesian(ylim=c(-4,5)) + 
  xlab("DO trend (scaled)") + theme(legend.position = "none")

do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(do_slopes, type = "DO", #hack= T,
  legend_position = c(.25,.95)) + coord_flip(ylim =c(-3,1)) +
  ylab("Slopes") 

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 

####

stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
stats$group[stats$group == "HAKE"] <- "COD"
imm <- filter(stats, age == "immature") %>% mutate(depth == depth_imm) %>% select(-depth_imm)
mat <- filter(stats, age == "mature") %>% select(-depth_imm)
stats <- rbind(mat, imm)


head(do_slopes)
# do_slopes$chopstick <- as.factor(do_slopes$chopstick)
# p <- filter(do_slopes, depth > 0) %>% 


slope_scatterplot(do_slopes, "length_50_mat_m", col_group = "age", regression = F) + facet_grid(~chopstick) 
# slope_scatterplot(do_slopes, "length_50_mat_m", slope_var = "global_slope", col_group = "age") 
slope_scatterplot(do_slopes, "age_max", col_group = "age") + facet_grid(~chopstick) 
# slope_scatterplot(do_slopes, "age_max", slope_var = "global_slope", col_group = "age") 
slope_scatterplot(do_slopes, "depth", col_group = "age") + facet_wrap(~chopstick, nrow=2) 
# slope_scatterplot(do_slopes, "depth", slope_var = "global_slope", col_group = "age") 






#### INTERACTIONS WITH CLIMATE VELOCITIES

model_vel_t <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-temp-1-200-temp.rds")
# stat <- readRDS(paste0("data/life-history-stats.rds")) %>%
#   mutate(sort_var = -depth) %>%
#   select(species, sort_var)

temp_vel_slopes <- chopstick_slopes(model_vel_t , x_variable = "squashed_temp_vel_scaled", 
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp") 
# temp_vel_slopes <- left_join(temp_vel_slopes, stat)
temp_vel_slopes <- temp_vel_slopes %>% mutate(sort_var = slope_est)

p2 <- plot_fuzzy_chopsticks(model_vel_t ,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = "Predicted mature biomass vel", 
  slopes = temp_vel_slopes # if add, the global slope can be included for insig
) + coord_cartesian(ylim=c(-50,50)) +
  xlab("Temperature vel (scaled)") + theme(legend.position = "none")

temp_vel_slopes$species[temp_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

p1 <- plot_chopstick_slopes(temp_vel_slopes, type = "temp", legend_position = c(.25,.95), 
  hack=F) + 
  ggtitle(paste("Effect of temperature vel on biomass")) + ylab("Slopes") +  
  coord_flip(ylim =c(-12,8))
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 

#### #### #### #### #### #### #### 
model_vel_d <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/vel-all-95-all-do-04-03-vel-do-1-200-do.rds")

do_vel_slopes <- chopstick_slopes(model_vel_d, x_variable = "squashed_DO_vel_scaled", 
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO") %>% 
  mutate(sort_var = slope_est)
# do_vel_slopes <- left_join(do_vel_slopes, stat) 
do_vel_slopes <- do_vel_slopes %>% mutate(sort_var = slope_est)

p2 <- plot_fuzzy_chopsticks(model_vel_d,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  y_label = "Predicted mature biomass vel", 
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + coord_cartesian(ylim=c(-25,35)) +
  xlab("DO vel (scaled)") + theme(legend.position = "none")

do_vel_slopes$species[do_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

p1 <- plot_chopstick_slopes(do_vel_slopes, type = "DO", legend_position = c(.25,.95), 
  hack=F) + 
  ggtitle(paste("Effect of DO vel on biomass")) + ylab("Slopes") +  
  coord_flip(ylim =c(-3,3.5))

cowplot::plot_grid(p1, p2, rel_widths = c(1, 2.5)) 

#### #### #### #### #### #### #### 





#### #### #### #### #### #### #### 
#### INDIVIDUAL CHOPSTICK PLOTS
#### #### #### #### #### #### #### 

plot_fuzzy_chopsticks(model,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = y_label
) +# ylim(-1, 1) + # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))


plot_fuzzy_chopsticks(model,
  x_variable = "squashed_DO_vel_scaled", type = "do",
  y_label = y_label
) +# ylim(-1, 1) + # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))


plot_fuzzy_chopsticks(model,
  x_variable = "temp_dvocc_scaled", type = "temp",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))

plot_fuzzy_chopsticks(model,
  x_variable = "DO_dvocc_scaled", type = "do",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))

### for bi-variable dist-search
# plot_fuzzy_chopsticks(model,
#   x_variable = "DO_dvocc_scaled", type = "temp",
#   y_label = y_label
# ) + #ylim(-1.5, 1) + # 
#   # facet_wrap(vars(species), scales="free_y") +
#   ggtitle(paste(title_all, "(", data_type, ")"))

# ggsave("figs/interation-plot-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)



#### PLOT SEPARATE MATURE AND IMMATURE MODELS TOGETHER

### model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-with-do-02-29-trend-with-do-3-500.rds")
## model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-all-do-03-25-trend-with-do-1-500.rds")
# model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-all-do-03-26-trend-with-do-1-500.rds")
## imm_model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-immature-95-all-do-03-18-trend-with-do-1-500.rds")
## imm_model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-immature-95-all-do-03-26-trend-with-do-1-500.rds")
# imm_model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-immature-95-all-do-03-27-trend-with-do-1-500.rds")

mat_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
# imm_slopes <- chopstick_slopes(imm_model, x_variable = "temp_trend_scaled", 
#    interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label, 
  # imm_model = imm_model, 
  # imm_slopes = imm_slopes,
  slopes = mat_slopes
) 

mat_slopes_do <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
# imm_slopes_do <- chopstick_slopes(imm_model, x_variable = "DO_trend_scaled", 
#   interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")

plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO", rug = T,
  y_label = y_label, 
  # imm_model = imm_model, 
  # imm_slopes = imm_slopes_do,
  slopes = mat_slopes_do
) + #facet_wrap(~species, scales = "free_y") + 
  coord_cartesian(xlim=c(-3, 3), ylim=c(-5,6))

# slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
#   interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
# imm_slopes <- chopstick_slopes(imm_model, x_variable = "temp_trend_scaled", 
#   interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
# slopes <- left_join(slopes, stat) #%>% mutate(sort_var = slope_est)
# imm_slopes <- left_join(imm_slopes, stat)
# p2 <- plot_fuzzy_chopsticks(model,
#   x_variable = "temp_trend_scaled", type = "temp",
#   y_label = y_label, 
#   slopes = slopes,  # if add, the global slope can be included for insig.
#   imm_model = imm_model, imm_slopes = imm_slopes
# ) + xlab("Temperature trend (scaled)") + theme(legend.position = "none")
# 
# slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	
# imm_slopes$species[imm_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	
# 
# p1 <- plot_chopstick_slopes(slopes, type = "temp", #hack= T,
#   legend_position = c(.25,.95), #c(.8,.95), 
#   imm_slopes = imm_slopes) + # scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1)) +
#   ylab("Slopes")
# # display beside chopstick plots
# cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 
# 
# 
# #### EXTRACT SLOPES AND PLOT THEM IN WORM FORM
# do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
#   interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
# imm_slopes_do <- chopstick_slopes(imm_model, x_variable = "DO_trend_scaled", 
#   interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
# do_slopes <- left_join(do_slopes, stat) #%>% mutate(sort_var = slope_est)
# imm_slopes_do <- left_join(imm_slopes_do, stat)
# 
# p2 <- plot_fuzzy_chopsticks(model,
#   x_variable = "DO_trend_scaled", type = "DO",
#   y_label = y_label,
#   slopes = do_slopes, # if add, the global slope can be included for insig.
#   imm_model = imm_model, imm_slopes = imm_slopes_do
# ) + # ylim(-5,5) +
#   xlab("DO trend (scaled)") + theme(legend.position = "none")
# 
# do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
# imm_slopes_do$species[imm_slopes_do$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
# p1 <- plot_chopstick_slopes(do_slopes, type = "DO", #hack= T,
#   legend_position = c(.25,.95), 
#   imm_slopes = imm_slopes_do) + #ggtitle(paste("Effect of DO trend on biomass")) + 
#   #scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1)) +
#   ylab("Slopes") 
# 
# # display beside chopstick plots
# cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 
