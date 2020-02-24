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
  x_variable = "DO_trend_scaled", type = "do",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))



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
