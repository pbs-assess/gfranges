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
# compile("vocc_regression.cpp")
# dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

##############################
#### LOAD MODELS ####

#### ONE JUST BUILT
model <- new_model


title_all <- "Interation plots "


### SUBTITLE WITH DATA TYPE
data_type <- "mature-95-with-do"
# data_type <- "mature-95-with-do-sim"

# data_type <- "mature-80-all-temp"
# data_type <- "mature-80-all-temp-sim"
# data_type <- "mature-90-all-temp"
# data_type <- "mature-90-all-temp-sim"
# data_type <- "mature-95-all-temp"
# data_type <- "mature-95-all-temp-sim"


### Y LABEL
# y_label <- "Predicted biomass trend"
y_label <- "Predicted % change in biomass"


#### ADULTS AND IMMATURE COMBINED IN ONE MODEL
model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-all-95-all-do-03-27-trend-with-do-1-500.rds")

### SEPARTE MODELS
# # # model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-with-do-02-29-trend-with-do-3-500.rds")
# # model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-all-do-03-25-trend-with-do-1-500.rds")
# model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-mature-95-all-do-03-26-trend-with-do-1-500.rds")
# 
# # # imm_model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-immature-95-all-do-03-18-trend-with-do-1-500.rds")
# # imm_model <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/trend-immature-95-all-do-03-26-trend-with-do-1-500.rds")
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



stat <- readRDS(paste0("data/life-history-stats.rds")) %>% View()
#### #### #### #### #### #### #### 

mat_slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")

stat <- readRDS(paste0("data/life-history-stats.rds")) %>% 
  mutate(sort_var = -depth) %>%
  # mutate(sort_var = depth_diff) %>% 
  # mutate(sort_var = length_99th) %>% 
  select(species, sort_var) 
slopes <- left_join(mat_slopes, stat) #%>% mutate(sort_var = slope_est)

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label, 
  slopes = slopes  # if add, the global slope can be included for insig.
) + xlab("Temperature trend (scaled)") + theme(legend.position = "none")

slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	

p1 <- plot_chopstick_slopes(slopes, type = "temp", #hack= T,
  legend_position = c(.25,.95)) + 
  ylab("Slopes")

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 

#### #### #### #### #### #### #### 
stat <- readRDS(paste0("data/life-history-stats.rds")) %>% 
  mutate(sort_var = -depth) %>% 
  select(species, sort_var) 

do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
do_slopes <- left_join(do_slopes, stat) #%>% mutate(sort_var = slope_est)

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = y_label,
  slopes = do_slopes
) + # ylim(-5,5) +
  xlab("DO trend (scaled)") + theme(legend.position = "none")

do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(do_slopes, type = "DO", #hack= T,
  legend_position = c(.25,.95)) + 
  ylab("Slopes") 

cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 

#### #### #### #### #### #### #### 




#### EXTRACT SLOPES AND PLOT THEM IN WORM FORM
slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
imm_slopes <- chopstick_slopes(imm_model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")
slopes <- left_join(slopes, stat) #%>% mutate(sort_var = slope_est)
imm_slopes <- left_join(imm_slopes, stat)
p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label, 
  slopes = slopes,  # if add, the global slope can be included for insig.
  imm_model = imm_model, imm_slopes = imm_slopes
) + xlab("Temperature trend (scaled)") + theme(legend.position = "none")

slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	
imm_slopes$species[imm_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	

p1 <- plot_chopstick_slopes(slopes, type = "temp", #hack= T,
  legend_position = c(.25,.95), #c(.8,.95), 
  imm_slopes = imm_slopes) + # scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1)) +
ylab("Slopes")
# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 






#### EXTRACT SLOPES AND PLOT THEM IN WORM FORM
do_slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
imm_slopes_do <- chopstick_slopes(imm_model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")
do_slopes <- left_join(do_slopes, stat) #%>% mutate(sort_var = slope_est)
imm_slopes_do <- left_join(imm_slopes_do, stat)

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = y_label,
  slopes = do_slopes, # if add, the global slope can be included for insig.
  imm_model = imm_model, imm_slopes = imm_slopes_do
  ) + # ylim(-5,5) +
  xlab("DO trend (scaled)") + theme(legend.position = "none")

do_slopes$species[do_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
imm_slopes_do$species[imm_slopes_do$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"	
p1 <- plot_chopstick_slopes(do_slopes, type = "DO", #hack= T,
  legend_position = c(.25,.95), 
  imm_slopes = imm_slopes_do) + #ggtitle(paste("Effect of DO trend on biomass")) + 
  #scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1)) +
  ylab("Slopes") 

# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2.5)) 









#### INTERACTIONS WITH CLIMATE VELOCITIES
#### EXTRACT SLOPES AND PLOT THEM IN WORM FORM

y_label <- "Predicted mature biomass vel"
# y_label <- "Predicted immature biomass vel"

# stat <- readRDS(paste0("data/life-history-stats.rds")) %>% 
#   mutate(sort_var = -depth) %>% 
#   select(species, sort_var) 
# slopes <- left_join(slopes, stat) %>% mutate(sort_var = slope_est)


slopes <- chopstick_slopes(model, x_variable = "squashed_temp_vel_scaled", 
  interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp") %>% 
  mutate(sort_var = slope_est)
# imm_slopes <- chopstick_slopes(imm_model, x_variable = "squashed_temp_vel_scaled", 
#   interaction_column = "squashed_temp_vel_scaled:mean_temp_scaled", type = "temp")

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "squashed_temp_vel_scaled", type = "temp",
  y_label = y_label, 
  # imm_model = imm_model, imm_slopes = imm_slopes, scale_imm = 0.1,
  slopes = slopes # if add, the global slope can be included for insig.
) + xlab("Temperature vel (scaled)") + theme(legend.position = "none")

slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	


p1 <- plot_chopstick_slopes(slopes, type = "temp", legend_position = c(.25,.95), 
  # imm_slopes = imm_slopes, 
  hack=T) + 
  ggtitle(paste("Effect of temperature vel on biomass")) + ylab("Slopes") #+
# scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1))

# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 






do_vel_slopes <- chopstick_slopes(model, x_variable = "squashed_DO_vel_scaled", 
  interaction_column = "squashed_DO_vel_scaled:mean_DO_scaled", type = "DO") %>% 
  mutate(sort_var = slope_est)
# imm_slopes <- chopstick_slopes(imm_model, x_variable = "squashed_DO_vel_scaled", 
#   interaction_column = "squashed_DO_vel_scaled:mean_temp_scaled", type = "temp")

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "squashed_DO_vel_scaled", type = "DO",
  y_label = y_label, 
  # imm_model = imm_model, imm_slopes = imm_slopes, scale_imm = 0.1,
  slopes = do_vel_slopes # if add, the global slope can be included for insig.
) + xlab("DO vel (scaled)") + theme(legend.position = "none")

do_vel_slopes$species[do_vel_slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	

p1 <- plot_chopstick_slopes(do_vel_slopes, type = "DO", legend_position = c(.25,.95), 
  # imm_slopes = imm_slopes, 
  hack=F) + 
  ggtitle(paste("Effect of DO vel on biomass")) + ylab("Slopes") #+
# scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1))

# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 





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
