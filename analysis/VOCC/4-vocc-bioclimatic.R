getwd()
setwd(here::here("/analysis/VOCC"))

library("gfranges")
library("dplyr")
library("ggplot2")

# species <- "Redbanded Rockfish"
 species <- "Pacific Ocean Perch"
# species <- "Pacific Cod"
# species <- "Arrowtooth Flounder"
# species <- "Petrale Sole"
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

# covs <- "-log-both-fixed-depth"
# covs <- "-log-both-noAR1"

 covs <- "-fixed"
# covs <- "-tv-depth"

# multiyear <- TRUE
 multiyear <- FALSE

 start_year <- 2009
# start_year <- 2011
# start_year <- 2013
# start_year <- 2015
 
 
#########################
### CHOOSE SPATIAL EXTENT
#########################

# model_ssid <- c(4)
# model_ssid <- c(16)
model_ssid <- c(1,3)
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
    summarise_all(mean) #%>% mutate(new_bioclim = est)
  d2 <- d %>%
    group_by(X, Y) %>%
    filter(year %in% years2) %>%
    summarise_all(mean) 
  
  #d2$new_bioclim = d1$new_bioclim - d1$epsilon_st
  
  dmn <- rbind(d1, d2) %>% mutate(bioclimatic = bioclim_blob)
}



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
  #theme_black = TRUE,
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

gf1 <- filter(gf, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)
gf1 <- mutate(gf1, velocity = if_else(gradient < 0.001, NA_real_, velocity))

range(gf1$velocity, na.rm = TRUE)

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
  transform_col = no_trans # log10 # fourth_root_power # sqrt # 
) + labs(
  title = "temp velocity",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad1



######################################
### Gradient-based BIOCLIMATIC VECTORS
######################################

g2 <- vocc_gradient_calc(dmn, 
  #"new_bioclim", #
  "bioclimatic",
  scale_fac = scale_fac,
  # all layers with min indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  indices = c(1,2),
  divisor = year_range,
  quantile_cutoff = 0.025
)

g2 <- g2 %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf2 <- left_join(g2, add_vars, by = c("x","y"))

gf2$est_diff <- exp(gf2$est_2) * 10000 - exp(gf2$est_1) * 10000
gf2$est_exp_1 <- exp(gf2$est_1) * 10000
#gf2$epsilon_st <- gf$epsilon_st


# gf2 <- mutate(gf2, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf2t <- filter(gf2, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)
grad_threshold <- quantile(gf2t$gradient, 0.001)
gf2t <- mutate(gf2t, velocity = if_else(gradient < grad_threshold, 
  NA_real_, velocity))


######################################
### Gradient-based BIOTIC VECTORS
######################################

layer <- "est"

g3 <- vocc_gradient_calc(dmn, layer,
  scale_fac = scale_fac,
  # indices = indices,
  divisor = year_range,
  quantile_cutoff = 0.025
)

g3 <- g3 %>%
  mutate(
    trend_per_year = trend / year_range,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) %>%
  gfplot:::utm2ll(., utm_zone = 9)

gf3 <- left_join(g3, add_vars, by = c("x","y"))

gf3$est_diff <- exp(gf3$est_2) * 10000 - exp(gf3$est_1) * 10000
gf3$est_exp_1 <- exp(gf3$est_1) * 10000

# gf3 <- mutate(gf3, velocity = if_else(gradient < 0.001, NA_real_, velocity))
gf3t <- filter(gf3, est_exp_1 > lower_density_threshold) %>% 
  mutate(velocity = velocity/year_range)

#grad_threshold <- quantile(gf3t$gradient, 0.0001)
gf3t <- mutate(gf3t, velocity = if_else(gradient < grad_threshold, 
  NA_real_, velocity))


######################################
### PLOT GRADIENT-BASED VELOCITY RASTER 
######################################

r2 <- range(gf2t$velocity, na.rm = TRUE) 
r3 <- range(gf3t$velocity, na.rm = TRUE) 
raster_min <- -64 #min(c(r2, r3))
raster_max <- 64 #max(c(r2, r3))

grad2 <- gfranges::plot_vocc(gf2t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  # grad_vec_aes = "velocity",
  # vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "velocity",
  fill_label = "gradient velocity",
  raster_alpha = 1,
  raster_limits = c(raster_min, raster_max),
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
  raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col =  sqrt # no_trans #
) + labs(
  title = "biotic ", #
  subtitle = paste0(min(d$year), "-", max(d$year))
)
grad3


######################################
### SAVE Gradient-based PLOTS
######################################

png(
  file = paste0("figs/", spp, "/biotic-velocities-g-", scale_fac, 
    "-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), 
    ".png"),
  res = 600,
  units = "in",
  width = 8.5,
  height = 8.5
)
gridExtra::grid.arrange(
  grad1, grad0, grad2, grad3,
  nrow = 2
)
dev.off()



######################################
### Calculate gradient-based biotic lags
######################################

biotic_values <- gf3t %>% rename( biotic_vel = velocity, 
  biotic_trend = trend_per_year, 
  biotic_grad = gradient, lat = Y, lon = X) %>% select(x, y, lat, lon, icell, biotic_vel, biotic_trend, biotic_grad, est_1, est_2, epsilon_1, epsilon_2) %>% mutate(start_year = !!start_year)

bioclim_values <- gf2t %>% rename( bioclim_vel = velocity, 
  bioclim_trend = trend_per_year, 
  bioclim_grad = gradient) %>% select(x, y, icell, bioclim_vel, bioclim_trend, bioclim_grad)

DO_values <- gf0t %>% rename( DO_vel = velocity, 
  DO_trend = trend_per_year, 
  DO_grad = gradient) %>% select(x, y, icell, DO_vel, DO_trend, DO_grad)

temp_values <- gf1 %>% rename( temp_vel = velocity, 
  temp_trend = trend_per_year, 
  temp_grad = gradient) %>% select(x, y, icell, temp_vel, temp_trend, temp_grad)


lags <- left_join(bioclim_values, biotic_values)
lags <- left_join(lags, DO_values) # add DO
lags <- left_join(lags, temp_values) # add temp
lags <- mutate(lags, lag = biotic_vel - bioclim_vel, species = !!species, multiyear = !!multiyear, ssid_string = !!ssid_string, method = "gradient")

write.csv(lags, file = paste0(
  "data/_lags", covs, "/lags-multiyear-", multiyear, "-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), ".csv"
))

