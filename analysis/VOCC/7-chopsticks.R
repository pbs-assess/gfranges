##############################
##############################
#### CHOPSTICK PLOTS
##############################
##############################
model <- new_model

model <- readRDS("data/trend_01-25-mature-95-all-temp-all-main-2.rds")
data_type <- "mature-95-all-temp"


model <- readRDS("data/trend_01-25-mature-80-all-temp-all-main-2.rds")
data_type <- "mature-80-all-temp"

model <- readRDS("data/trend_01-25-mature-80-all-temp-all-main-sim-2.rds")
data_type <- "mature-80-all-temp-sim"


model <- readRDS("data/trend_01-25-mature-90-all-temp-all-main.rds") # 90
data_type <- "mature-90-all-temp"

model <- readRDS("data/trend_01-25-mature-90-all-temp-all-main-sim-2.rds")
data_type <- "mature-90-all-temp-sim"


model <- readRDS("data/trend-mature-95-all-do-02-20-trend-with-do-sim-1.rds")
data_type <- "mature-95-with-do-sim"


model <- readRDS("data/trend-mature-95-all-do-02-20-trend-with-do-1.rds")
data_type <- "mature-95-with-do"


#### INTERACTIONS WIHT TEMP TREND
title_all <- "Interation plots "
y_label <- "Predicted biomass trend"

plot_fuzzy_chopsticks(model,
  x_variable = "temp_trend_scaled", type = "temp",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))

# plot_fuzzy_chopsticks(model,
#   x_variable = "temp_trend_scaled", type = "biomass",
#   y_label = y_label
# ) + # facet_wrap(vars(species), scales="free_y") +
#   ggtitle(paste(title_all, "(", data_type, ")"))

# plot_fuzzy_chopsticks(model,
#   x_variable = "temp_trend_scaled", type = "temp_grad",
#   y_label = y_label
# ) + facet_wrap(vars(species), scales = "free_y") +
#   ggtitle(paste(title_all, "(", data_type, ")"))

plot_fuzzy_chopsticks(model,
  x_variable = "DO_trend_scaled", type = "do",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  #facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))




title_all <- "Interation plots "
y_label <- "Predicted biomass vel"

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
  x_variable = "squashed_temp_vel_scaled", type = "biomass",
  y_label = y_label
) + # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))


y_label <- "Predicted biomass dist-velocity"
plot_fuzzy_chopsticks(model,
  x_variable = "temp_dvocc_scaled", type = "temp",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))


y_label <- "Predicted biomass dist-velocity"
plot_fuzzy_chopsticks(model,
  x_variable = "DO_dvocc_scaled", type = "temp",
  y_label = y_label
) + #ylim(-1.5, 1) + # 
  # facet_wrap(vars(species), scales="free_y") +
  ggtitle(paste(title_all, "(", data_type, ")"))



#### INTERACTIONS WITH DO TREND
# plot_fuzzy_chopsticks(model,
#   x_variable = "DO_trend_scaled", type = "do",
#   y_label = y_label
# ) +
# plot_fuzzy_chopsticks(model,
#   x_variable = "mean_temp_scaled", type = "mean_temp",
#   y_label = y_label
# ) + ggtitle(paste(title_all, "(", data_type, ")"))
plot_fuzzy_chopsticks(model,
  x_variable = "mean_DO_scaled", type = "mean_do",
  y_label = y_label
) + ggtitle(paste(title_all, "(", data_type, ")"))

##############################
#### LOAD VELOCITY MODELS ####
# model <- readRDS("data/trend_by_vel_01-16-multi-spp-biotic-vocc-mature-chopsticks3.rds")
# title_all <- "Interation plots "
# y_label <-"Predicted biomass velocity"

#### INTERACTIONS WITH VELOCITY COVARIATES
# plot_fuzzy_chopsticks(model,
#   x_variable = "squashed_temp_vel_scaled", type = "temp",
#   y_label = y_label
# ) + #ylim(-1,1) + #facet_wrap(vars(species), scales="free_y") +
# ggtitle(paste(title_all, "(", data_type, ")"))
# ggsave("figs/interation-plot-trend-by-temp-vel.png", width = 10, height = 10, dpi = 300)
