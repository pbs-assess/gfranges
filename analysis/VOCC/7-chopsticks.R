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
y_label <- "Predicted biomass trend"
# y_label <- "Predicted biomass vel"
## y_label <- "Predicted biomass dist-velocity" # didn't converge


#### INTERACTIONS WITH CLIMATE TRENDS
plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))

plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))


#### EXTRACT SLOPES AND PLOT THEM IN WORM FORM
slopes <- chopstick_slopes(model, x_variable = "temp_trend_scaled", 
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp")

slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	

p1 <- plot_chopstick_slopes(slopes, type = "temp", legend_position = c(.75,.95), hack=F) + 
  ggtitle(paste("Interactions (", data_type, ")")) #+
# scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1))

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label, 
  slopes = slopes # if add, the global slope can be included for insig.
) + xlab("Temperature trend (scaled)") + theme(legend.position = "none")

# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 



#### EXTRACT SLOPES AND PLOT THEM IN WORM FORM
slopes <- chopstick_slopes(model, x_variable = "DO_trend_scaled", 
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO")

slopes$species[slopes$species=="Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted Rockfish"	

p1 <- plot_chopstick_slopes(slopes, type = "DO", legend_position = c(.75,.95), hack=F) + 
  ggtitle(paste("Interactions (", data_type, ")")) #+
 #scale_y_continuous(trans = fourth_root_power, breaks=c(-1, -0.1, 0, 0.1,1))

p2 <- plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "DO",
  y_label = y_label
) + xlab("DO trend (scaled)") + theme(legend.position = "none")

# display beside chopstick plots
cowplot::plot_grid(p1,p2, rel_widths = c(1, 2)) 





#### INTERACTIONS WITH CLIMATE VELOCITIES
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
