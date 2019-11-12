getwd()
setwd(here::here("/analysis/VOCC"))

library("gfranges")

species <- "Redbanded Rockfish"
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

covs <- "-log-both-fixed-depth"
# covs <- "-log-both-noAR1"


#########################
### CHOOSE SPATIAL EXTENT
#########################

model_ssid <- c(1, 3)
dist_intercept <- 0 # input_cell_size / 2
input_cell_size <- 2
scale_fac <- 2
# scale_fac = 3 means that the raster is reprojected to 3 X original grid (2 km)


#################
### LOAD DATA
#################

d_all <- readRDS(paste0("data/", spp, "/all-predictions-", 
  spp, covs, "-mat-bioclimatic-prior-FALSE.rds")) %>% 
  filter(ssid %in% model_ssid)

# d_all <- readRDS(paste0("data/", spp, "/all-predictions-", 
#   spp, covs, "-imm-bioclimatic-prior-FALSE.rds")) %>% 
#   filter(ssid %in% model_ssid)

#d_all <- ad_prediction_list[[2]]


#########################
### CHOOSE TIME FRAME
#########################

unique(d_all$year)
d <- d_all
indices <- c(1, 1, 1, 2, 2)
year_range <- 5
# # OR
# d <- d_all %>% filter(year > 2011) %>% filter(year < 2017)
# indices <- c(1,2)
# year_range <- 2

# end_time <- start_time + 2
skip_time <- NULL
delta_t_step <- year_range
delta_t_total <- year_range


#################
### MAKE RASTERS
#################

rbrick <- make_raster_brick(d, "epsilon_st", scale_fac = scale_fac)
rbrick_est <- make_raster_brick(d, "est", scale_fac = scale_fac)
if (isTRUE(length(indices) > 2)) {
  rbrick <- raster::stackApply(rbrick, indices = indices, fun = mean)
  rbrick_est <- raster::stackApply(rbrick_est, indices = indices, fun = mean)
}

coord <- raster::xyFromCell(rbrick[[1]],1:raster::ncell(rbrick[[1]]))
x <- coord[,1]
y <- coord[,2]
icell <- seq(1, raster::ncell(rbrick[[1]]))
epsilon_1 <- raster::getValues(rbrick[[1]])
epsilon_2 <- raster::getValues(rbrick[[2]])
est_1 <- raster::getValues(rbrick_est[[1]])
est_2 <- raster::getValues(rbrick_est[[2]])
add_vars <- data.frame(x = x, y = y, icell = icell, 
  epsilon_1 = epsilon_1, epsilon_2 = epsilon_2, 
  est_1 = est_1, est_2 = est_2)


###########################
### CHOOSE BIOMASS THRESHOLD
###########################

bio5perc <- sum(exp(add_vars$est_1), na.rm = TRUE) * 0.05
s <- sort(exp(add_vars$est_1))
bio_sum <- cumsum(s)
lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]


#################################
#################################
### Gradient-based CLIMATE VECTORS
#################################
#################################


gf0 <- vocc_gradient_calc(d, "do_est",
  scale_fac = scale_fac,
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  # indices = indices,
  divisor = year_range,
  quantile_cutoff = 0.05
)


gf0 <- gf0 %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf0 <- left_join(gf0, add_vars, by = c("x","y"))

gf0$est_diff <- exp(gf0$est_2) * 10000 - exp(gf0$est_1) * 10000
gf0$est_exp_1 <- exp(gf0$est_1) * 10000

# View(gf0)
range(gf0$velocity, na.rm = TRUE)

gf0 <- mutate(gf0, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf0t <- filter(gf0, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)


grad0 <- plot_vocc(gf0t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  # raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = sqrt # no_trans # 
) + labs(
  title = "do velocity",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad0

gf <- vocc_gradient_calc(d, "temp",
  scale_fac = scale_fac,
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  # indices = indices,
  divisor = year_range,
  quantile_cutoff = 0.05
)

gf <- gf %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf <- left_join(gf, add_vars, by = c("x","y"))

gf$est_diff <- exp(gf$est_2) * 10000 - exp(gf$est_1) * 10000
gf$est_exp_1 <- exp(gf$est_1) * 10000

range(gf$velocity, na.rm = TRUE)

gf <- mutate(gf, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf1 <- filter(gf, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)

#gf1z <- mutate(gf1, velocity = if_else(velocity == 0, NA_real_, velocity))

grad1 <- plot_vocc(gf1,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  # raster_limits = c(raster_min, raster_max),
  na_colour = "white",
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = sqrt # no_trans # log10 # fourth_root_power # 
) + labs(
  title = "temp velocity",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad1


######################################
### Gradient-based BIOCLIMATIC VECTORS
######################################

glimpse(d)
d[d$year == min(d$year), ]$bioclimatic <- (d[d$year == min(d$year), ]$est)
# d[d$year==max(d$year), ]$bioclimatic <- d[d$year==max(d$year), ]$est - d[d$year==max(d$year), ]$epsilon_st
d[d$year == max(d$year), ]$bioclimatic <- (d[d$year == max(d$year), ]$est_non_rf - d[d$year == max(d$year), ]$est_rw_i + d[d$year == min(d$year), ]$est_rw_i - d[d$year == max(d$year), ]$epsilon_st)

gf2 <- vocc_gradient_calc(d, "bioclimatic",
  scale_fac = scale_fac,
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  # indices = indices,
  divisor = year_range,
  quantile_cutoff = 0.025
)

gf2 <- gf2 %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf2$epsilon_1 <- gf$epsilon_1
gf2$epsilon_2 <- gf$epsilon_2
gf2$epsilon_st <- gf$epsilon_st

gf2$est_1 <- gf$est_1
gf2$est_2 <- gf$est_2
gf2$est_diff <- gf$est_diff
gf2$est_exp_1 <- exp(gf$est_1) * 10000

range(gf$velocity, na.rm = TRUE)

# gf2 <- mutate(gf2, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf2 <- filter(gf2, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)

grad2 <- gfranges::plot_vocc(gf2,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  # grad_vec_aes = "velocity",
  # vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  raster_limits = c(-400, 200),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt # no_trans #
) + labs(
  title = "bioclimatic",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad2


######################################
### Gradient-based BIOTIC VECTORS
######################################

layer <- "est"

gf3 <- vocc_gradient_calc(d, layer,
  scale_fac = scale_fac,
  # indices = indices,
  divisor = year_range,
  quantile_cutoff = 0.025
)

gf3 <- gf3 %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf3$epsilon_1 <- gf$epsilon_1
gf3$epsilon_2 <- gf$epsilon_2
gf3$epsilon_st <- gf$epsilon_st

gf3$est_1 <- gf$est_1
gf3$est_2 <- gf$est_2
gf3$est_diff <- gf$est_diff
gf3$est_exp_1 <- exp(gf$est_1) * 10000

# gf3 <- mutate(gf3, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf3t <- filter(gf3, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)

grad3 <- gfranges::plot_vocc(gf3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  raster_limits = c(-400, 200),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt # no_trans #
) + labs(
  title = "biotic",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad3


######################################
### SAVE Gradient-based PLOTS
######################################

png(
  file = paste0("figs/", spp, "/biotic-velocities-g-", scale_fac, "-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), ".png"),
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


#################################
#################################
### Distance-based VELOCITIES 
#################################
#################################

max_dist_all_years <- 50 * year_range
# Calculate means for each time period
all_years <- unique(d$year)

years <- all_years[which(indices == 1)]
years2 <- all_years[which(indices == 2)]

d1 <- d %>%
  filter(year %in% years) %>%
  mutate(bioclimatic = est) %>%
  group_by(X, Y) %>%
  summarise_all(mean) %>%
  mutate(est_rw_1 = est_rw_i)
d2 <- d %>%
  filter(year %in% years2) %>%
  group_by(X, Y) %>%
  summarise_all(mean)
d2$est_rw_1 <- d1$est_rw_1
d2 <- mutate(d2, bioclimatic = est_non_rf - est_rw_i + est_rw_1 - epsilon_st)

dmn <- rbind(d1, d2)
unique(dmn$year)


######################################
### TEMPERATURE DISTANCE-BASED VELOCITIES 
######################################

variable_names <- c("temp")
lower_change <- c(0.25)
upper_change <- c(0.25)
# min_string <- paste0(lower_change, collapse = "-")
# max_string <- paste0(upper_change, collapse = "-")


sym_temp <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  # skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  min_dist = 0,
  delta_t_total = delta_t_total,
  delta_t_step = delta_t_step,
  #indices = indices,
  min_thresholds = lower_change,
  max_thresholds = upper_change,
  round_fact = 10
)

# add and mutate variables
df <- sym_temp %>% mutate(velocity = if_else(slope < 0, -distance, distance))
df <- left_join(df, add_vars, by = c("icell","x","y"))
df$epsilon_diff <- df$epsilon_2 - df$epsilon_1
df$est_diff <- (df$est_2) - (df$est_1)
df$raw_diff <- exp(df$est_2) * 10000 - exp(df$est_1) * 10000
df$est_exp_2 <- exp(df$est_2) * 10000
df$est_exp_1 <- exp(df$est_1) * 10000

# truncate velocities and scale to per year
dft <- mutate(df, velocity = if_else(velocity > max_dist_all_years, 
  max_dist_all_years, velocity) / year_range)
dft <- mutate(dft, velocity = if_else(velocity < -max_dist_all_years / year_range, 
  -max_dist_all_years / year_range, velocity))

# filter for 95% of biomass
dft <- filter(dft, est_exp_1 > lower_density_threshold)

# OR trim based on species thresholds and then filter as above
lower_thresholds <- c(3.6) # 50% values
upper_thresholds <- c(6.2) # 50% values
lower_thresholds <- c(3.9) # 75% values
upper_thresholds <- c(5.6) # 75% values
df1 <- trim_vector_data(df, variable_names,
  lower_change, upper_change,
  lower_thresholds, upper_thresholds,
  cell_size = 2*scale_fac, dist_intercept = 0,
  max_dist = 120, min_dist = 0
)
df1t <- filter(df1, est_exp_1 > lower_density_threshold)
df1t <- mutate(df1t, velocity = if_else(slope < 0, -distance, distance))
df1t <- mutate(df1t, velocity = if_else(velocity > max_dist_all_years, 
  max_dist_all_years, velocity) / year_range)
df1t <- mutate(df1t, velocity = if_else(velocity < -max_dist_all_years / year_range, 
  -max_dist_all_years / year_range, velocity))

# Plot trimmed to 95% of biomass
gvocc1 <- gfranges::plot_vocc(dft,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  raster_limits = c(0, 20),
  na_colour = "darkred",
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = sqrt #log10 # no_trans # 
) + labs(
  title = "temp change > 0.25",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc1

# Plot trimmmed to species thresholds
gvocc1t <- gfranges::plot_vocc(df1t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  raster_limits = c(0, 20),
  na_colour = "darkred",
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = sqrt #no_trans # log10
) + labs(
  title = "temp change > 0.25 & exceeds species thresholds",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc1t


######################################
### DO DISTANCE-BASED VELOCITIES 
######################################

variable_names <- c("do_est")
lower_change <- c(0.25)
upper_change <- c(Inf)
# min_string <- paste0(lower_change, collapse = "-")
# max_string <- paste0(upper_change, collapse = "-")

do_vec <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  # skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  min_dist = 0,
  delta_t_total = year_range,
  delta_t_step = year_range,
  # indices = indices,
  min_thresholds = lower_change,
  max_thresholds = upper_change,
  round_fact = 10
)

# add and mutate variables
do <- do_vec %>% mutate(velocity = if_else(slope < 0, -distance, distance))
do <- left_join(do, add_vars, by = c("icell","x","y"))
do$epsilon_diff <- do$epsilon_2 - do$epsilon_1
do$est_diff <- (do$est_2) - (do$est_1)
do$raw_diff <- exp(do$est_2) * 10000 - exp(do$est_1) * 10000
do$est_exp_2 <- exp(do$est_2) * 10000
do$est_exp_1 <- exp(do$est_1) * 10000

# filter for 95% of biomass and truncate velocities
dot <- filter(do, est_exp_1 > lower_density_threshold)
dot <- mutate(dot, velocity = if_else(velocity > max_dist_all_years, 
  max_dist_all_years, velocity) / year_range)
dot <- mutate(dot, velocity = if_else(velocity < -max_dist_all_years / year_range, 
  -max_dist_all_years / year_range, velocity))

# OR trim based on species thresholds and then filter as above
lower_thresholds <- c(1.76) # optimal DO
upper_thresholds <- c(NA)
do1 <- trim_vector_data(do, variable_names,
    lower_change, upper_change,
    lower_thresholds, upper_thresholds,
    cell_size = 2*scale_fac, dist_intercept = 0,
    max_dist = 120, min_dist = 0
  )
  do1t <- filter(do1, est_exp_1 > lower_density_threshold)
  do1t <- mutate(do1t, velocity = if_else(slope < 0, -distance, distance))
  do1t <- mutate(do1t, velocity = if_else(velocity > max_dist_all_years, 
    max_dist_all_years, velocity) / year_range)
  do1t <- mutate(do1t, velocity = if_else(velocity < -max_dist_all_years / year_range, 
    -max_dist_all_years / year_range, velocity))
  
# Plots
gvocc_do <- gfranges::plot_vocc(dot,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  # raster_limits = c(2.5, 50),
  na_colour = "gray",
  high_fill = "Steel Blue 4", 
  low_fill = "Red 3", 
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans
) + ggtitle("DO decrease > 0.25")
gvocc_do

gvocc_dot <- gfranges::plot_vocc(do1t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  # raster_limits = c(2.5, 50),
  na_colour = "gray",
  high_fill = "Steel Blue 4", 
  low_fill = "Red 3", 
  white_zero = TRUE,
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans
) + ggtitle("DO decrease > 0.25")
gvocc_dot


######################################
### BIOCLIMATIC DISTANCE-BASED VELOCITIES 
######################################

variable_names <- c("bioclimatic")
biomass_change <- c(0.5)
# min_est <- quantile(d$bioclimatic, 0.01)
# lower_thresholds <- NA
# upper_thresholds <- min_est

bioclim_vec <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  # #skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  min_dist = 0,
  delta_t_total = year_range,
  delta_t_step = year_range,
  # indices = indices,
  min_thresholds = biomass_change,
  max_thresholds = biomass_change,
  round_fact = 10
)

# add and mutate variables
df2 <- bioclim_vec %>% mutate(velocity = if_else(slope < 0, -distance, distance))
df2 <- left_join(df2, add_vars, by = c("icell","x","y"))
df2$epsilon_diff <- df2$epsilon_2 - df2$epsilon_1
df2$est_diff <- (df2$est_2) - (df2$est_1)
df2$raw_diff <- exp(df2$est_2) * 10000 - exp(df2$est_1) * 10000
df2$est_exp_2 <- exp(df2$est_2) * 10000
df2$est_exp_1 <- exp(df2$est_1) * 10000


# filter for 95% of biomass and truncate velocities
df2t <- filter(df2, est_exp_1 > lower_density_threshold)
df2t <- mutate(df2t, velocity = if_else(velocity > max_dist_all_years, 
  max_dist_all_years, velocity) / year_range)
df2t <- mutate(df2t, velocity = if_else(velocity < -max_dist_all_years / year_range, 
  -max_dist_all_years / year_range, velocity))


######################################
### BIOTIC DISTANCE-BASED VELOCITIES 
######################################

variable_names <- "est"

biotic_vec <- make_vector_data(dmn,
  variable_names = variable_names,
  ssid = model_ssid,
  # start_time = 2013,
  # end_time = 2015,
  # skip_time = skip_time,
  input_cell_size = input_cell_size,
  scale_fac = scale_fac,
  min_dist = 0,
  delta_t_total = year_range,
  delta_t_step = year_range,
  # indices = indices,
  min_thresholds = biomass_change,
  max_thresholds = biomass_change,
  round_fact = 10
)


# add and mutate variables
df3 <- biotic_vec %>% mutate(velocity = if_else(slope < 0, -distance, distance))
df3 <- left_join(df3, add_vars, by = c("icell","x","y"))
df3$epsilon_diff <- df3$epsilon_2 - df3$epsilon_1
df3$est_diff <- (df3$est_2) - (df3$est_1)
df3$raw_diff <- exp(df3$est_2) * 10000 - exp(df3$est_1) * 10000
df3$est_exp_2 <- exp(df3$est_2) * 10000
df3$est_exp_1 <- exp(df3$est_1) * 10000

# filter for 95% of biomass and truncate velocities
df3t <- filter(df3, est_exp_1 > lower_density_threshold)
df3t <- mutate(df3t, velocity = if_else(velocity > max_dist_all_years, 
  max_dist_all_years, velocity) / year_range)
df3t <- mutate(df3t, velocity = if_else(velocity < -max_dist_all_years / year_range, 
  -max_dist_all_years / year_range, velocity))


######################################
### PLOT DISTANCE-BASED VELOCITY RASTER 
######################################

# df2t <- mutate(df2t, velocity = if_else(velocity != 0, velocity, NA_real_))
gvocc2 <- gfranges::plot_vocc(df2t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  # raster_limits = c(-50,15),
  na_colour = "gray",
  white_zero = TRUE,
  # mid_fill = "black",
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  # high_fill = "#66C2A5",
  # low_fill = "#5E4FA2FF",
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans # sqrt # fourth_root_power #
) + labs(
  title = paste0("bioclimatic - projected change in abundance > ", biomass_change * 100, "%"),
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc2


gvocc3 <- gfranges::plot_vocc(df3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  # raster_limits = c(-50,15),
  na_colour = "gray",
  # white_zero = TRUE,
  # mid_fill = "black",
  high_fill = "Steel Blue 4", 
  low_fill = "Red 3", 
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col = no_trans # fourth_root_power #sqrt #
) + labs(
  title = paste0("biotic - modelled change in abundance > ", biomass_change * 100, "%"),
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc3


######################################
### SAVE Distance-based PLOTS
######################################

png(
  file = paste0("figs/", spp, "/biotic-velocities-d-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), "-symetrical-", biomass_change, ".png"),
  res = 600,
  units = "in",
  width = 8.5,
  height = 8.5
)
gridExtra::grid.arrange(
  gvocc1, gvocc_do, gvocc2, gvocc3,
  nrow = 2
)
dev.off()



######################################
### PLOT DISTANCE-BASED VECTORS
######################################

# fill_col <- "est_diff"
# fill_label <- paste("% biomass change")
# vec_col <- "black"

# fill_col <- "raw_diff"
# fill_label <- paste("biomass change")
# vec_col <- "black"

# fill_col <- "epsilon_diff"
# fill_label <- paste("epsilon change")
# vec_col <- "black"

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
# gvocc1 <- gfranges::plot_vocc(df1t,
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = "distance",
#   vec_col = "black",
#   arrowhead_size = 0.01,
#   vec_lwd_range = vec_lwd_range,
#   min_vec_plotted = input_cell_size * scale_fac * 2,
#   max_vec_plotted = max_vector,
#   NA_label = "NA",
#   fill_col = "slope",
#   fill_label = "slope",
#   raster_alpha = 1,
#   # raster_limits = c(raster_min, raster_max),
#   na_colour = "gray",
#   # white_zero = TRUE,
#   vec_alpha = 0.35,
#   axis_lables = FALSE,
#   transform_col = no_trans
# ) + ggtitle(paste0(variable_names, " change < 0.25 without exceeding threshold"))
# gvocc1

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

