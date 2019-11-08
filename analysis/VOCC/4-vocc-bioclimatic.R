
getwd()
setwd(here::here("/analysis/VOCC"))

### Gradient-based

d_all <- ad_prediction_list[[2]] 
unique(d_all$year)
d <- d_all
indices <- c(1,1,1,2,2)
year_range <- 5
max_dist_all_years <- 30 * year_range
# # OR 
# d <- d_all %>% filter(year > 2011) %>% filter(year < 2017)
# indices <- c(1,2)
# year_range <- 2
# glimpse(d)


# scale_fac = 3 means that the raster is reprojected to 3 X original grid (2 km)
rbrick <- make_raster_brick(d, "epsilon_st", scale_fac = scale_fac)
rbrick_est <- make_raster_brick(d, "est", scale_fac = scale_fac)
if (isTRUE(length(indices) > 2)) {
  rbrick <- raster::stackApply(rbrick, indices = indices, fun = mean)
  rbrick_est <- raster::stackApply(rbrick_est, indices = indices, fun = mean)
}



icell <- seq(1, raster::ncell(rbrick[[1]]))
epsilon_1 <- raster::getValues(rbrick[[1]])
epsilon_2 <- raster::getValues(rbrick[[2]])
est_1 <- raster::getValues(rbrick_est[[1]])
est_2 <- raster::getValues(rbrick_est[[2]])
add_vars <- data.frame(icell = icell,epsilon_1= epsilon_1,epsilon_2 = epsilon_2, est_1 = est_1, est_2 = est_2)

bio5perc <- sum(exp(add_vars$est_1), na.rm = TRUE)*0.05
s <- sort(exp(add_vars$est_1))
bio_sum <- cumsum(s)
lower_density_threshold <- s[which(bio_sum>=bio5perc)[1]]


df <- vocc_gradient_calc(d, "temp",
  scale_fac = 5, 
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years 
  # indices = indices, 
  divisor = year_range,
  quantile_cutoff = 0.05)


df <- df %>%
  mutate(trend_per_year = trend/year_range,
    trend_per_decade = trend_per_year*10,
    velocity = velocity) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

df$epsilon_1 <- raster::rasterToPoints(rbrick[[1]])[,3]
df$epsilon_2 <- raster::rasterToPoints(rbrick[[2]])[,3]
df$epsilon_st <- df$epsilon_2 - df$epsilon_1

df$est_1 <- (raster::rasterToPoints(rbrick_est[[1]])[,3])
df$est_2 <- (raster::rasterToPoints(rbrick_est[[2]])[,3])
df$est_diff <- exp(df$est_2)*10000 - exp(df$est_1)*10000
df$est_exp_1 <- exp(df$est_1)*10000

#View(df)
range(df$velocity, na.rm = TRUE)

#df <- filter(df, gradient > 0.01)
# these filter high velocities based on less than 0.5 degree change per year
#df <- mutate(df, velocity = if_else(gradient < 0.0025, NA_real_, velocity))
df <- mutate(df, velocity = if_else(gradient < 0.001, NA_real_, velocity))
df1 <- filter(df, est_exp_1 > lower_density_threshold)


grad1 <- gfranges::plot_vocc(df1,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  #raster_limits = c(raster_min, raster_max),
  na_colour = "darkred",
  # white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans # sqrt #
) + labs(title = "temp velocity", 
  subtitle = paste0(min(d$year), "-", max(d$year)))
grad1

# grad1v <- plot_gradient_vocc(df1,
#   vec_col = "black",
#   #col_label = "Trend in\nbiomass \n(per decade)",
#   vecsize = 1.5,
#   lwd = 0.5,
#   #raster = "tend_per_year"
#   #raster = "epsilon_st"
#   raster = "est_diff"
# ) + ggtitle("temp")
# grad1v 
#######
glimpse(d)
d[d$year==min(d$year), ]$bioclimatic <- (d[d$year==min(d$year), ]$est) 
#d[d$year==max(d$year), ]$bioclimatic <- d[d$year==max(d$year), ]$est - d[d$year==max(d$year), ]$epsilon_st
d[d$year==max(d$year), ]$bioclimatic <- (d[d$year==max(d$year), ]$est_non_rf - d[d$year==max(d$year), ]$est_rw_i + d[d$year==min(d$year), ]$est_rw_i - d[d$year==max(d$year), ]$epsilon_st)
#d$bioclimatic <- exp(d$bioclimatic)*10000


df2 <- vocc_gradient_calc(d, "bioclimatic",
  scale_fac = 5, 
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years 
  #indices = indices, 
  divisor = year_range,
  quantile_cutoff = 0.025)

df2 <- df2 %>%
  mutate(trend_per_year = trend/year_range,
    trend_per_decade = trend_per_year*10,
    velocity = velocity) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

df2$epsilon_1 <- df$epsilon_1 
df2$epsilon_2 <- df$epsilon_2 
df2$epsilon_st <- df$epsilon_st 

df2$est_1 <- df$est_1 
df2$est_2 <- df$est_2 
df2$est_diff <- df$est_diff 
df2$est_exp_1 <- exp(df$est_1)*10000 
#View(df2)
range(df$velocity, na.rm = TRUE)

#df2 <- mutate(df2, velocity = if_else(gradient < 0.001, NA_real_, velocity))
df2 <- filter(df2, est_exp_1 > lower_density_threshold)


grad2 <- gfranges::plot_vocc(df2,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  raster_limits = c(-400, 200),
  na_colour = "darkred",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt #no_trans # 
) + labs(title = "bioclimatic", 
  subtitle = paste0(min(d$year), "-", max(d$year)))
grad2

# 
# grad2v <- plot_gradient_vocc(df2,
#   vec_col = "trend_per_decade",
#   col_label = "Trend in\nbiomass \n(per decade)",
#   vecsize = 1.5,
#   lwd = 0.5,
#   high_col = "orange red 3",
#   mid_col = "black",
#   low_col = "steel blue 4",
#   #raster = "temp_trend"
#   #raster = "epsilon_st"
#   raster = "raw_diff"
#   #raster = "est_diff"
# ) + ggtitle("bioclimatic")
# grad2v

#######
layer <- "est"
#layer <- "est_exp"

df3 <- vocc_gradient_calc(d, layer,
  scale_fac = 5, 
  #indices = indices, 
  divisor = year_range,
  quantile_cutoff = 0.025)

df3 <- df3 %>%
  mutate(trend_per_year = trend/year_range,
    trend_per_decade = trend_per_year*10,
    velocity = velocity) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

df3$epsilon_1 <- df$epsilon_1 
df3$epsilon_2 <- df$epsilon_2 
df3$epsilon_st <- df$epsilon_st 

df3$est_1 <- df$est_1 
df3$est_2 <- df$est_2 
df3$est_diff <- df$est_diff 
df3$est_exp_1 <- exp(df$est_1)*10000 
#View(df3)

#df3 <- mutate(df3, velocity = if_else(gradient < 0.001, NA_real_, velocity))
df3 <- filter(df3, est_exp_1 > lower_density_threshold)


grad3 <- gfranges::plot_vocc(df3,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  raster_limits = c(-400, 200),
  na_colour = "darkred",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt #no_trans # 
) + labs(title = "biotic", 
  subtitle = paste0(min(d$year), "-", max(d$year)))
grad3

# 
# grad3v <- plot_gradient_vocc(df2,
#   vec_col = "trend_per_decade",
#   col_label = "Trend in\nbiomass \n(per decade)",
#   vecsize = 1.5,
#   lwd = 0.5,
#   high_col = "orange red 3",
#   mid_col = "black",
#   low_col = "steel blue 4",
#   #raster = "temp_trend"
#   #raster = "epsilon_st"
#   raster = "raw_diff"
#   #raster = "est_diff"
# ) + ggtitle("bioclimatic")
# grad3v


png(
  file = paste0("figs/", spp, "/biotic-velocities-g-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), ".png"),
  res = 600,
  units = "in",
  width = 14,
  height = 6.5
)
gridExtra::grid.arrange(
  grad1, grad2, grad3,
  nrow = 1
)
dev.off()

#############
### Distance-based
#############


### general vector settings
# d_all <- ad_prediction_list[[2]] 
# d <- d_all %>% filter(year > 2011) %>% filter(year < 2017)
# glimpse(d)

# bio5perc <- sum(d[d$year==min(d$year),]$est_exp)*0.05
# s <- sort(d[d$year==min(d$year),]$est_exp)
# bio_sum <- cumsum(s)
# lower_density_threshold <- s[which(bio_sum>=bio5perc)[1]]



unique(d$year)
#end_time <- start_time + 2
model_ssid <- c(1, 3)
#max_vector <- 80
input_cell_size <- 2
dist_intercept <- 0 # input_cell_size / 2
scale_fac <- 5
skip_time <- NULL
delta_t_step <- year_range
delta_t_total <- year_range

# specifics for type of vectors 
variable_names <- c("temp")
lower_thresholds <- c(3.7)
upper_thresholds <- c(6.2)
lower_change <- c(0.25)
upper_change <- c(0.25)
#upper_model <- root_params[rev(order(root_params$t_maxy)), ]$model
#lower_model <- root_params[rev(order(root_params$do_maxy)), ]$model
min_string <- paste0(lower_change, collapse = "-")
max_string <- paste0(upper_change, collapse = "-")


sym_temp <- make_vector_data(d,
  variable_names = variable_names,
  ssid = model_ssid,
  #start_time = 2013,
  #end_time = 2015,
  #skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  delta_t_total = delta_t_total,
  delta_t_step = delta_t_step,
  indices = indices,
  min_thresholds = lower_change,
  max_thresholds = upper_change,
  round_fact = 10
)

# specifics for type of vectors
variable_names <- c("do_est")
lower_thresholds <- c(1.02)
upper_thresholds <- c(NA)
lower_change <- c(0.25)
upper_change <- c(Inf)
#upper_model <- root_params[rev(order(root_params$t_maxy)), ]$model
#lower_model <- root_params[rev(order(root_params$do_maxy)), ]$model
min_string <- paste0(lower_change, collapse = "-")
max_string <- paste0(upper_change, collapse = "-")

dfdo <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  #skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  delta_t_total = year_range,
  delta_t_step = year_range,
  #indices = indices,
  min_thresholds = lower_change,
  max_thresholds = upper_change,
  round_fact = 10
)


 df <- dfdo
# df <- temp

sym_temp <- sym_temp %>% mutate(distance = if_else(slope<0, -distance, distance))
df <- sym_temp

df <- left_join(df, add_vars, by = "icell")
df$epsilon_diff <- df$epsilon_2 - df$epsilon_1
df$est_diff <- (df$est_2) - (df$est_1)
df$raw_diff <- exp(df$est_2)*10000 - exp(df$est_1)*10000
df$est_exp_2 <- exp(df$est_2)*10000 
df$est_exp_1 <- exp(df$est_1)*10000 

#df$diff <- df$units_per_decade / 10 * delta_t_total

dft <- filter(df, est_exp_1 > lower_density_threshold)
dft <- mutate(dft, velocity = if_else(distance > max_dist_all_years, max_dist_all_years, distance)/year_range)
dft <- mutate(dft, velocity = if_else(velocity < -max_dist_all_years/year_range, -max_dist_all_years/year_range, velocity))

df1 <- trim_vector_data(df, variable_names,
  lower_change, upper_change,
  lower_thresholds, upper_thresholds,
  cell_size = 10, dist_intercept = 0,
  max_dist = 120
)

df1t <- filter(df1, est_exp_1 > lower_density_threshold)
df1t <- mutate(df1t, velocity = if_else(distance>max_dist_all_years, max_dist_all_years, distance)/year_range)
df1t <- mutate(df1t, velocity = if_else(velocity < -max_dist_all_years/year_range, -max_dist_all_years/year_range, velocity))

#######

rbrick <- make_raster_brick(d, "epsilon_st", scale_fac = scale_fac)
rbrick_est <- make_raster_brick(d, "est", scale_fac = scale_fac)

if (isTRUE(length(indices) > 2)) {
  rbrick <- raster::stackApply(rbrick, indices = indices, fun = mean)
  rbrick_est <- raster::stackApply(rbrick_est, indices = indices, fun = mean)
}


glimpse(d)
# 
# rbrick <- make_raster_brick(d, "epsilon_st")
# 
# mean_est <- raster::stackApply(rbrick, indices = indices, fun = mean)
# mean_est_non_rf <- raster::stackApply(rbrick, indices = indices, fun = mean)
# mean_est_rw <- raster::stackApply(rbrick, indices = indices, fun = mean)
# mean_epsilon <- raster::stackApply(rbrick, indices = indices, fun = mean)

indices
all_years <- unique(d$year)

years <- all_years[which(indices==1)]
years2 <- all_years[which(indices==2)]

d1 <- d %>% filter( year %in% years) %>% mutate(bioclimatic = est) %>% group_by(X, Y) %>% summarise_all(mean) %>% mutate(est_rw_1 = est_rw_i)
d2 <- d %>% filter( year %in% years2) %>% group_by(X, Y) %>% summarise_all(mean)
d2$est_rw_1 <- d1$est_rw_1  
d2 <- mutate(d2, bioclimatic = est_non_rf - est_rw_i + est_rw_1 - epsilon_st)

dmn <- rbind(d1,d2)
unique(dmn$year)

# d[d$year==min(d$year), ]$bioclimatic <- (d[d$year==min(d$year), ]$est) 
# #d[d$year==max(d$year), ]$bioclimatic <- d[d$year==max(d$year), ]$est - d[d$year==max(d$year), ]$epsilon_st
# d[d$year==max(d$year), ]$bioclimatic <- (d[d$year==max(d$year), ]$est_non_rf - d[d$year==max(d$year), ]$est_rw_i + d[d$year==min(d$year), ]$est_rw_i - d[d$year==max(d$year), ]$epsilon_st)
# #d$bioclimatic <- exp(d$bioclimatic)*10000

# specifics for type of vectors 
variable_names <- c("bioclimatic")

# min_est <- quantile(d$bioclimatic, 0.01)
# lower_thresholds <- NA
# upper_thresholds <- min_est
biomass_change <- c(0.25)
# upper_model <- root_params[rev(order(root_params$t_maxy)), ]$model
# lower_model <- root_params[rev(order(root_params$do_maxy)), ]$model
min_string <- paste0(lower_change, collapse = "-")
max_string <- paste0(upper_change, collapse = "-")


df2 <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  # #skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  delta_t_total = year_range,
  delta_t_step = year_range,
  #indices = indices,
  min_thresholds = biomass_change,
  max_thresholds = biomass_change,
  round_fact = 10
)

df2 <- df2 %>% mutate(distance = if_else(slope<0, -distance, distance))

df2 <- left_join(df2, add_vars, by = "icell")
df2$epsilon_diff <- df2$epsilon_2 - df2$epsilon_1
df2$est_diff <- (df2$est_2) - (df2$est_1)
df2$raw_diff <- exp(df2$est_2)*10000 - exp(df2$est_1)*10000
df2$est_exp_2 <- exp(df2$est_2)*10000 
df2$est_exp_1 <- exp(df2$est_1)*10000 

#df2$diff <- df2$units_per_decade / 10 * delta_t_total

df2t <- filter(df2, est_exp_1 > lower_density_threshold)

df2t <- mutate(df2t, velocity = if_else(distance>max_dist_all_years, max_dist_all_years, distance)/year_range)
df2t <- mutate(df2t, velocity = if_else(velocity < -max_dist_all_years/year_range, -max_dist_all_years/year_range, velocity))

#######
variable_names <- "est"


df3 <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  #skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  delta_t_total = year_range,
  delta_t_step = year_range,
  #indices = indices,
  min_thresholds = biomass_change,
  max_thresholds = biomass_change,
  round_fact = 10
)

add_vars_by_cell <- data.frame(icell = icell,epsilon_1= epsilon_1,epsilon_2 = epsilon_2, est_1 = est_1, est_2 = est_2)
df3 <- left_join(df3, add_vars_by_cell, by = "icell")

df3 <- df3 %>% mutate(distance = if_else(slope < 0, -distance, distance))

df3$epsilon_diff <- df3$epsilon_2 - df3$epsilon_1
df3$est_diff <- (df3$est_2) - (df3$est_1)
df3$raw_diff <- exp(df3$est_2)*10000 - exp(df3$est_1)*10000

df3$est_exp_2 <- exp(df3$est_2)*10000 
df3$est_exp_1 <- exp(df3$est_1)*10000 

#df3$diff <- df3$units_per_decade / 10 * delta_t_total
df3t <- filter(df3, est_exp_1 > lower_density_threshold)


df3t <- mutate(df3t, velocity = if_else(distance>max_dist_all_years, max_dist_all_years, distance)/year_range)
df3t <- mutate(df3t, velocity = if_else(velocity < -max_dist_all_years/year_range, -max_dist_all_years/year_range, velocity))


# 
# fill_col <- "est_diff"
# fill_label <- paste("% biomass change")
# vec_col <- "black"
# 
# fill_col <- "raw_diff"
# fill_label <- paste("biomass change")
# vec_col <- "black"
# 
# fill_col <- "epsilon_diff"
# fill_label <- paste("epsilon change")
# vec_col <- "black"
# 
# vec_lwd_range <- c(.7, 1)
# 
# fill_col <- "est_exp_2"
# fill_label <- paste("final biomass")
# vec_col <- "white"
# 
# 
# vec_lwd_range <- c(.9, 1.2)
# 
# gvocc <- gfranges::plot_vocc(df,  
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = "diff",
#   vec_col = vec_col,
#   arrowhead_size = 0.01,
#   vec_lwd_range = vec_lwd_range,
#   min_vec_plotted = input_cell_size*scale_fac*2,
#   max_vec_plotted = max_vector,
#   NA_label = "NA",
#   fill_col = fill_col,
#   fill_label = fill_label,
#   raster_alpha = 1,
#   #raster_limits = c(raster_min, raster_max),
#   na_colour = "gray",
#   # white_zero = TRUE,
#   vec_alpha = 0.35,
#   axis_lables = FALSE,
#   transform_col = no_trans
# ) + ggtitle("temp increase of less than 0.25 degrees")
# gvocc
# 
gvocc1 <- gfranges::plot_vocc(df1t,
  # theme = "black",
  coast = TRUE,
  vec_aes = "distance",
  vec_col = "black",
  arrowhead_size = 0.01,
  vec_lwd_range = vec_lwd_range,
  min_vec_plotted = input_cell_size*scale_fac*2,
  max_vec_plotted = max_vector,
  NA_label = "NA",
  fill_col = "slope",
  fill_label = "slope",
  raster_alpha = 1,
  #raster_limits = c(raster_min, raster_max),
  na_colour = "gray",
  # white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans
) + ggtitle( paste0(variable_names, " change < 0.25 without exceeding threshold"))
gvocc1

# gvocc2 <- gfranges::plot_vocc(df2t,
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = "diff",
#   vec_col =   vec_col,
#   arrowhead_size = 0.01,
#   vec_lwd_range = vec_lwd_range,
#   min_vec_plotted = input_cell_size*scale_fac*2,
#   max_vec_plotted = max_vector,
#   NA_label = "NA",
#   fill_col = fill_col,
#   fill_label = fill_label,
#   raster_alpha = 1,
#   #raster_limits = c(raster_min, raster_max),
#   na_colour = "gray",
#   # white_zero = TRUE,
#   vec_alpha = 0.35,
#   axis_lables = FALSE,
#   transform_col = no_trans
# ) + ggtitle("bioclimatic - projected loss in abundance of less than 50%")
# gvocc2
# 
# gvocc3 <- gfranges::plot_vocc(df3t,  
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = "distance",
#   vec_col = "black" ,
#   arrowhead_size = 0.01,
#   vec_lwd_range = vec_lwd_range,
#   min_vec_plotted = input_cell_size*scale_fac*2,
#   max_vec_plotted = max_vector,
#   NA_label = "NA",
#   fill_col = fill_col,
#   fill_label = fill_label,
#   raster_alpha = 1,
#   #raster_limits = c(raster_min, raster_max),
#   na_colour = "gray",
#   # white_zero = TRUE,
#   vec_alpha = 0.35,
#   axis_lables = FALSE,
#   transform_col = no_trans
# ) + labs(title = "biotic - modelled loss in abundance of less than 50%", subtitle = paste0(min(d$year), "-", max(d$year))) 
# gvocc3
# 
# png(
#   file = paste0("figs/", spp, "/biotic-velocities-d-", spp, covs, "-", ssid_string, "-abundance-change.png"),
#   res = 600,
#   units = "in",
#   width = 14,
#   height = 6.5
# )
# gridExtra::grid.arrange(
# gvocc1, gvocc2, gvocc3,
#   nrow = 1
# )
# dev.off()
# 
# 
# 
# gvocc <- gfranges::plot_vocc(df,  
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = NULL,
#   fill_col = "velocity",
#   fill_label = "velocity",
#   raster_alpha = 1,
#   #raster_limits = c(raster_min, raster_max),
#   na_colour = "gray",
#   # white_zero = TRUE,
#   vec_alpha = 0.35,
#   axis_lables = FALSE,
#   transform_col = fourth_root_power
# ) + ggtitle("temp increase < 0.25")
# gvocc


gvocc1 <- gfranges::plot_vocc(df1t,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(2.5, 50),
  na_colour = "gray",
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans#sqrt #log10
  ) + labs(title = "temp change > 0.25 & exceeding species specific thresholds", 
    subtitle = paste0(min(d$year), "-", max(d$year))) 
gvocc1

#df2t <- mutate(df2t, velocity = if_else(velocity != 0, velocity, NA_real_))
gvocc2 <- gfranges::plot_vocc(df2t,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(-50,15),
  na_colour = "gray",
  # white_zero = TRUE,
  #mid_fill = "black",
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  # high_fill = "#66C2A5", 
  # low_fill = "#5E4FA2FF",
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans #sqrt # fourth_root_power
) + labs(title = paste0("bioclimatic - projected change in abundance > ", biomass_change*100, "%"), 
  subtitle = paste0(min(d$year), "-", max(d$year))) 
gvocc2


gvocc3 <- gfranges::plot_vocc(df3t,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(-50,15),
  na_colour = "gray",
  #white_zero = TRUE,
  #mid_fill = "black",
  high_fill = "Steel Blue 4",# "#3288BD", #"#66C2A5", #
  low_fill = "Red 3",#"#D53E4F", #"#F46D43" "#FDAE61" "#FEE08B"  # "#5E4FA2FF",# "#FDE725FF", #"#FEE08B", # "#9E0142", #
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans #fourth_root_power #sqrt #
) + labs(title = paste0("biotic - modelled change in abundance > ", biomass_change*100, "%"), 
  subtitle = paste0(min(d$year), "-", max(d$year))) 
gvocc3

gvocc_do <- gfranges::plot_vocc(dft,  
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(2.5, 50),
  na_colour = "gray",
  # white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = log10
) + ggtitle("DO decrease > 0.25")
gvocc_do


png(
  file = paste0("figs/", spp, "/biotic-velocities-d-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), "-symetrical-", biomass_change, ".png"),
  res = 600,
  units = "in",
  width = 14,
  height = 6.5
)
gridExtra::grid.arrange(
    gvocc1, gvocc2, gvocc3,
  nrow = 1
)
dev.off()

#gvocc_do,