getwd()
setwd(here::here("/analysis/VOCC"))

library("gfranges")
library("dplyr")
library("ggplot2")

#############
species <- "Redbanded Rockfish"
lower_change_t <- c(0.25)
upper_change_t <- c(0.25)
lower_thresholds_t <- c(3.6) # 50% values
upper_thresholds_t <- c(6.2) # 50% values
# lower_thresholds <- c(3.9) # 75% values
# upper_thresholds <- c(5.6) # 75% values

lower_change_do <- c(0.25)
upper_change_do <- c(Inf)
lower_thresholds_do <- c(1.76) # optimal DO
upper_thresholds_do <- c(NA)


##############
# species <- "Pacific Cod"
# lower_change_t <- c(0.25)
# upper_change_t <- c(0.25)
# lower_thresholds_t <- c(4.59) # 50% values
# upper_thresholds_t <- c(9.08) # 50% values
# # lower_thresholds <- c(3.9) # 75% values
# # upper_thresholds <- c(5.6) # 75% values
# 
# lower_change_do <- c(0.25)
# upper_change_do <- c(Inf)
# lower_thresholds_do <- c(3.1) # optimal DO
# upper_thresholds_do <- c(NA)


##############
# species <- "Pacific Ocean Perch"
# lower_change_t <- c(0.25)
# upper_change_t <- c(0.25)
# lower_thresholds_t <- c(4.84) # 50% values
# upper_thresholds_t <- c(6.55) # 50% values
# 
# # $ lower_t_50   <dbl> 4.835491, 4.614679
# # $ upper_t_50   <dbl> 6.547783, 6.171273
# # $ lower_t_75   <dbl> 5.103299, 4.859539
# # $ upper_t_75   <dbl> 6.204172, 5.860318
# lower_change_do <- c(0.25)
# upper_change_do <- c(Inf)
# lower_thresholds_do <- c(1.58) # optimal DO
# upper_thresholds_do <- c(NA)


##############
# species <- "Petrale Sole"
# lower_change_t <- c(0.25)
# upper_change_t <- c(0.25)
# lower_thresholds_t <- c(4.77) # 50% values
# upper_thresholds_t <- c(8.47) # 50% values
# 
# # $ lower_t_50   <dbl> 4.771915, 5.463951
# # $ upper_t_50   <dbl> 8.466562, 7.955737
# # $ lower_t_75   <dbl> 5.284308, 5.841591
# # $ upper_t_75   <dbl> 7.645602, 7.441424
# lower_change_do <- c(0.25)
# upper_change_do <- c(Inf)
# lower_thresholds_do <- c(2.52) # optimal DO
# upper_thresholds_do <- c(NA)


spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))


covs <- "-fixed"
# covs <- "-tv-depth"

# multiyear <- TRUE

 multiyear <- FALSE
# start_year <- 2009
# start_year <- 2011
# start_year <- 2013
 start_year <- 2015


#########################
### Distance-based VELOCITIES 
#########################

max_dist_all_years <- 50 * year_range


#########################
### CHOOSE SPATIAL EXTENT
#########################

# model_ssid <- c(4)
# model_ssid <- c(16)
model_ssid <- c(1, 3)
ssid_string <- paste0(model_ssid, collapse = "n")
dist_intercept <- 0 # input_cell_size / 2
input_cell_size <- 2
scale_fac <- 5 # means that the raster is reprojected to _ X original grid (2 km)


#################
### LOAD DATA
#################

d_all <- readRDS(paste0("data/", spp, "/bioclimatic-predictions-", 
  spp, covs, "-1n3n4n16-mat-2015-cutoff.rds")) %>% 
  filter(ssid %in% model_ssid)
glimpse(d_all)

# # # for climate independent biotic values...
# covs <- "-trawled-ssid-reml"
# d_all <- readRDS(paste0("data/", spp, "/all-predictions-",
#   spp, covs, "-mat-biomass-prior-FALSE.rds")) %>%
#   filter(ssid %in% model_ssid)

#########################
### SET TIME FRAME
#########################

if (multiyear){
  #unique(d_all$year)
  d <- d_all
  length(unique(d$year))
  year_range <- length(unique(d$year))
  start_year <- 2007
  if (year_range==5) indices <- c(1, 1, 1, 2, 2)
  if (year_range==6) indices <- c(1, 1, 1, 1, 2, 2)
  
} else{ 
  d <- d_all %>% filter(year >= start_year) %>% filter(year <= start_year+3)
  unique(d$year)
  indices <- c(1,2)
  year_range <- 2
}

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
### TRIM environment to range sampled
###########################

#d <- d %>% filter(do_est > 0.23) %>% filter(do_est < 7.91) # full range
d <- d %>% filter(do_est > 0.28) %>% filter(do_est < 7.06) # 0.005 and 0.995
#d <- d %>% filter(temp > 2.61) %>% filter(temp < 14.31) # full range
d <- d %>% filter(temp > 3.07) %>% filter(temp < 11.3) # 0.005 and 0.995



###########################
### CHOOSE BIOMASS THRESHOLD
###########################

bio5perc <- sum(exp(add_vars$est_1), na.rm = TRUE) * 0.05
s <- sort(exp(add_vars$est_1))
bio_sum <- cumsum(s)
lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]


###########################
### CHOOSE BIOCLIMATIC ESTIMATES
###########################

all_years <- unique(d$year)
years <- all_years[which(indices == 1)]
years2 <- all_years[which(indices == 2)]


if (length(indices)==2) {
  if (years2==2011) {
    dmn <- mutate(d, bioclimatic = bioclim_2011)
  }
  if (years2==2012) {
    dmn <- mutate(d, bioclimatic = bioclim_2011)
  }
  if (years2==2013) {
    dmn <- mutate(d, bioclimatic = bioclim_2013)
  }
  if (years2==2014) {
    dmn <- mutate(d, bioclimatic = bioclim_2013)
  }
  if (years2==2015) {
    dmn <- mutate(d, bioclimatic = bioclim_2015)
  }
  if (years2==2016) {
    dmn <- mutate(d, bioclimatic = bioclim_2015)
  }
  if (years2==2017) {
    dmn <- mutate(d, bioclimatic = bioclim_2017)
  }
  if (years2==2018) {
    dmn <- mutate(d, bioclimatic = bioclim_2017)
  }
} else {
  d1 <- d %>% filter(year %in% years) %>%
    group_by(X, Y) %>%
    summarise_all(mean)
  d2 <- d %>%
    group_by(X, Y) %>%
    filter(year %in% years2) %>%
    summarise_all(mean) 
  
  dmn <- rbind(d1, d2) %>% mutate(bioclimatic = bioclim_blob)
}



######################################
### TEMPERATURE DISTANCE-BASED VELOCITIES 
######################################

variable_names <- c("temp")

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
  min_thresholds = lower_change_t,
  max_thresholds = upper_change_t,
  round_fact = 10
)

saveRDS(sym_temp, file = paste0("data/", spp, "/dvocc-temp-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))

sym_temp <- readRDS(paste0("data/", spp, "/dvocc-temp-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))

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

df1 <- trim_vector_data(df, variable_names,
  lower_change_t, upper_change_t,
  lower_thresholds_t, upper_thresholds_t,
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
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(0, 20),
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
  coast = TRUE,
  vec_aes = NULL,
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(0, 20),
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
  min_thresholds = lower_change_do,
  max_thresholds = upper_change_do,
  round_fact = 10
)

saveRDS(do_vec, file = paste0("data/", spp, "/dvocc-do-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))

do_vec <- readRDS(paste0("data/", spp, "/dvocc-do-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))


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

do1 <- trim_vector_data(do, variable_names,
    lower_change_do, upper_change_do,
    lower_thresholds_do, upper_thresholds_do,
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
  transform_col = sqrt #no_trans
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
) + ggtitle("DO decrease > 0.25 & exceeds species thresholds")
gvocc_dot


######################################
### BIOCLIMATIC DISTANCE-BASED VELOCITIES 
######################################

variable_names <- c("bioclimatic")
biomass_change <- c(0.1)
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

saveRDS(bioclim_vec, file = paste0("data/", spp, "/dvocc-bioclim-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))

bioclim_vec <- readRDS(paste0("data/", spp, "/dvocc-bioclim-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac,  
  covs, "-", lower_change, "-", upper_change, ".rds"))

#max(df2$distance, na.rm = TRUE)
# add and mutate variables
df2 <- bioclim_vec %>% mutate(distance = if_else(distance == max(distance, na.rm = TRUE), NA_real_, distance))
df2 <- df2 %>% mutate(velocity = if_else(slope < 0, -distance, distance))
df2 <- left_join(df2, add_vars, by = c("icell","x","y"))
df2$epsilon_diff <- df2$epsilon_2 - df2$epsilon_1
df2$est_diff <- (df2$est_2) - (df2$est_1)
df2$raw_diff <- exp(df2$est_2) * 10000 - exp(df2$est_1) * 10000
df2$est_exp_2 <- exp(df2$est_2) * 10000
df2$est_exp_1 <- exp(df2$est_1) * 10000


# filter for 95% of biomass and truncate velocities
# max_dist_all_years <- 20 * year_range
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

saveRDS(biotic_vec, file = paste0("data/", spp, "/dvocc-biotic-", spp, 
  "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
  covs, "-", lower_change, "-", upper_change, ".rds"))

biotic_vec <- readRDS(paste0("data/", spp, "/dvocc-biotic-", spp, 
   "-", min(d$year), "-", max(d$year), "-", ssid_string, "-x", scale_fac, 
   covs, "-", lower_change, "-", upper_change, ".rds"))

# add and mutate variables
df3 <- biotic_vec %>% mutate(distance = if_else(distance == max(distance, na.rm = TRUE), NA_real_, distance))
#max(df3$distance, na.rm = TRUE)
df3 <- df3 %>% mutate(velocity = if_else(slope < 0, -distance, distance))
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

r2 <- range(df2t$velocity, na.rm = TRUE) 
r3 <- range(df3t$velocity, na.rm = TRUE) 
raster_min <- min(c(r2, r3))
raster_max <- max(c(r2, r3))


gvocc2 <- gfranges::plot_vocc(df2t,
  # theme_black = TRUE,
  coast = TRUE,
  vec_aes = NULL,
  # vec_aes = "velocity",
  # vec_col = "black",
  # vec_lwd_range = c(1,1),
  # min_vec_plotted = 5, # this is in distance
  # max_vec_plotted = max_dist_all_years, # this is in distance
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  # mid_fill = "black",
  # high_fill = "#66C2A5",
  # low_fill = "#5E4FA2FF",
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col =  no_trans # sqrt # fourth_root_power # 
) + labs(
  title = paste0("bioclimatic - projected change in abundance > ", biomass_change * 100, "%"),
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc2

#df3v <- df3t %>% mutate(distance = if_else(slope < 0, distance, 0))
gvocc3 <- gfranges::plot_vocc(df3t,
  #theme_black = TRUE,
  coast = TRUE,
  vec_aes = NULL,
  # vec_aes = "velocity",
  # vec_col = "black",
  # vec_lwd_range = c(1,1),
  # min_vec_plotted = 5, # this is in distance
  # max_vec_plotted = max_dist_all_years, # this is in distance
  fill_col = "velocity",
  fill_label = "velocity",
  raster_alpha = 1,
  #raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  # mid_fill = "black",
  # high_fill = "#66C2A5",
  # low_fill = "#5E4FA2FF",
  vec_alpha = 0.35,
  axis_lables = FALSE,
  transform_col =  no_trans # fourth_root_power # sqrt #
) + labs(
  title = paste0("biotic - modelled change in abundance > ", biomass_change * 100, "%"),
  subtitle = paste0(min(d$year), "-", max(d$year))
)
gvocc3


######################################
### SAVE Distance-based PLOTS
######################################

png(
  file = paste0("figs/", spp, "/biotic-velocities-d-", scale_fac, "-", 
    spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), 
    "-symetrical-", biomass_change, "trim.png"),
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
### Calculate distance-based biotic lags
######################################
# glimpse(df3t)

biotic_values <- df3t %>% rename( biotic_trend = slope, 
  #biotic_grad = gradient, 
  #lat = Y, lon = X
  biotic_vel = velocity) %>% 
  select(x, y, #lat, lon, 
    icell, biotic_vel, #biotic_trend, #biotic_grad, 
    est_1, est_2, epsilon_1, epsilon_2) %>% mutate(start_year = !!start_year)

bioclim_values <- df2t %>% rename( bioclim_trend = slope, 
  #bioclim_grad = gradient, 
  bioclim_vel = velocity) %>% 
  select(x, y, icell, bioclim_vel, #bioclim_trend, #bioclim_grad
    )


DO_values <- dot %>% rename( DO_vel = velocity, 
  DO_trend = slope) %>% select(x, y, icell, DO_vel, DO_trend)

temp_values <- df1t %>% rename( temp_vel = velocity, 
  temp_trend = slope) %>% select(x, y, icell, temp_vel, temp_trend)


lags <- left_join(bioclim_values, biotic_values)

lags <- left_join(lags, DO_values) # add DO
lags <- left_join(lags, temp_values) # add temp
# lags <- mutate(lags, lag = biotic_vel - bioclim_vel, species = !!species, multiyear = !!multiyear, ssid_string = !!ssid_string, method = "gradient")

lags <- mutate(lags, lag = biotic_vel - bioclim_vel, species = !!species, multiyear = !!multiyear, ssid_string = !!ssid_string, method = "distance")

write.csv(lags, file = paste0(
  "data/_lags", covs, "/dist-lags-multiyear-", multiyear, "-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), ".csv"
))

