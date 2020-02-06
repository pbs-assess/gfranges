getwd()
setwd(here::here("/analysis/VOCC"))

library("gfranges")
library("dplyr")
library("ggplot2")

# species <- "Redbanded Rockfish"
# species <- "Pacific Ocean Perch"
# species <- "Pacific Cod"
species <- "Arrowtooth Flounder"
# species <- "Petrale Sole"


spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
covs <- "-tv-depth-only"

multiyear <- TRUE

#########################
### CHOOSE SPATIAL EXTENT
#########################

model_ssid <- c(4)
# model_ssid <- c(16)
# model_ssid <- c(1,3)
ssid_string <- paste0(model_ssid, collapse = "n")
dist_intercept <- 0 # input_cell_size / 2
input_cell_size <- 2
scale_fac <- 2 # means that the raster is reprojected to _ X original grid (2 km)


#################
### LOAD DATA
#################

d_all <-readRDS(paste0("data/", spp,
  "/predictions-", spp, covs, "-1n3n4n16-mature-biomass-prior-FALSE.rds")) %>% filter(year > 2007) 

nd_all <- readRDS(paste0("data/predicted-DO-new.rds")) %>% select (X, Y, year, temp, do_est)

d_all <- left_join(d_all, nd_all, by=c("X","Y", "year"))

glimpse(d_all)

#########################
### SET TIME FRAME
#########################

if (multiyear){
 #unique(d_all$year)
 d <- d_all %>% filter(ssid %in% model_ssid)
 length(unique(d$year))
 years_sampled <- length(unique(d$year))
 year_range <- (years_sampled -1)*2
 start_year <- 2007
 if (years_sampled==5) indices <- c(1, 2, 2, 2, 2)
 if (years_sampled==6) indices <- c(1, 2, 2, 2, 2, 2)
 
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


# # #################
# # ### MAKE RASTERS
# # #################
#  rbrick <- make_raster_brick(d, "est", scale_fac = scale_fac)
#  if (isTRUE(length(indices) > 2)) {
#     rbrick <- raster::stackApply(rbrick, indices = c(1,2,2,2,2), 
#       fun = mean) 
#     }
#  
# coord <- raster::xyFromCell(rbrick[[1]],1:raster::ncell(rbrick[[1]]))
# x <- coord[,1]
# y <- coord[,2]
# icell <- seq(1, raster::ncell(rbrick[[1]]))
# est_1 <- raster::getValues(rbrick[[1]])
# est_2 <- raster::getValues(rbrick[[2]])
# add_vars <- data.frame(x = x, y = y, icell = icell,
#   est_1 = est_1, est_2 = est_2)

# ###########################
### TRIM environment to range sampled
###########################

#d <- d %>% filter(do_est > 0.23) %>% filter(do_est < 7.91) # full range
d <- d %>% filter(do_est > 0.28) %>% filter(do_est < 7.06) # 0.005 and 0.995
#d <- d %>% filter(temp > 2.61) %>% filter(temp < 14.31) # full range
d <- d %>% filter(temp > 3.07) %>% filter(temp < 11.3) # 0.005 and 0.995



###########################c
### CHOOSE BIOMASS THRESHOLD
###########################

# # mean_biomass <- d_all %>% group_by(X, Y) %>% mutate(mean_est = mean(exp(est), na.rm = TRUE))
# # bio5perc <- sum((mean_biomass$mean_est), na.rm = TRUE) * 0.05
# # s <- sort((mean_biomass$mean_est))
# # 
# # first_biomass <- d_all %>% filter(year == min(d_all$year)) 
# # bio5perc <- sum(exp(first_biomass$est), na.rm = TRUE) * 0.05
# # s <- sort(exp(first_biomass$est))
# 
# bio5perc <- sum(exp(add_vars$est_1)* 10000, na.rm = TRUE) * 0.01
# s <- sort(exp(add_vars$est_1)* 10000)
# 
# bio_sum <- cumsum(s)
# lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
# 
# # d_mean <- d %>% group_by(X, Y) %>% 
# #   mutate(mean_est = mean(exp(est), na.rm = TRUE)) %>% 
# #   select(X, Y, mean_est)
# # 
# # d_mean <- d %>% filter(year == min(d$year)) %>% 
# #   mutate(mean_est = mean(exp(est), na.rm = TRUE)) %>% 
# #   select(X, Y, mean_est)
# 
# 
# # all_years <- unique(d$year)
# # years <- all_years[which(indices == 1)]
# # years2 <- all_years[which(indices == 2)]
# # 
# 

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
  divisor = 1,
  quantile_cutoff = 0.05
)


gf0 <- gf0 %>%
  mutate(
    trend_per_year = trend,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>% dplyr::mutate(X = x, Y = y) %>%
   gfplot:::utm2ll(., utm_zone = 9)

##gf0 <- left_join(gf0, d_mean, by = c("X","Y"))
#gf0 <- left_join(gf0, add_vars, by = c("x","y"))
 
# gf0$est_diff <- exp(gf0$est_2) * 10000 - exp(gf0$est_1) * 10000
#gf0$est_exp_1 <- exp(gf0$est_1) * 10000

# View(gf0)
#range(gf0$velocity, na.rm = TRUE)

#gf0 <- mutate(gf0, velocity = if_else(gradient < 0.001, NA_real_, velocity))
# gf0t <- filter(gf0, est_exp_1 > lower_density_threshold) %>% 
#   mutate(velocity = velocity/year_range)



grad0 <- plot_vocc(gf0,
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
#grad0

gf2 <- vocc_gradient_calc(d, "temp",
  scale_fac = scale_fac,
  # all layers with max indice will be averaged for spatial gradient
  # if default (NULL) will use all years
  # indices = indices,
  divisor = 1,
  quantile_cutoff = 0.05
)

gf2 <- gf2 %>%
  mutate(
    trend_per_year = trend ,
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) #%>%
  #gfplot:::utm2ll(., utm_zone = 9)
# 
# gf <- left_join(gf, d_mean)
# 
# # gf$est_diff <- exp(gf$est_2) * 10000 - exp(gf$est_1) * 10000
# # gf$est_exp_1 <- exp(gf$est_1) * 10000
# 
# range(gf$velocity, na.rm = TRUE)
# 
# gf1 <- filter(gf, mean_est > lower_density_threshold) %>% 
#   mutate(velocity = velocity/year_range)
# gf1 <- mutate(gf1, velocity = if_else(gradient < 0.001, NA_real_, velocity))

# range(gf1$velocity, na.rm = TRUE)

#gf1z <- mutate(gf1, velocity = if_else(velocity == 0, NA_real_, velocity))

grad2 <- plot_vocc(gf2,
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
  transform_col = sqrt # no_trans # log10 # fourth_root_power # sqrt # 
) + labs(
  title = "temp velocity",
  subtitle = paste0(min(d$year), "-", max(d$year))
)
# grad2


######################################
### Gradient-based BIOTIC VECTORS
######################################

#indices <- c(1,2,3,4,5)
gf3 <- vocc_gradient_calc(d, "est",
  scale_fac = scale_fac,
  #indices = indices,
  divisor = 1,
  log_space = TRUE,
  quantile_cutoff = 0.025
)

gf3 <- gf3 %>%
  mutate(
    CV_formula = sqrt(exp(cv^2)-1),
    CV = sd/exp(mean), 
    mean_biomass = exp(mean)*10000,
    trend_per_year = exp(trend),
    trend_per_decade = trend_per_year * 10,
    velocity = velocity
  ) %>%
  dplyr::mutate(X = x, Y = y) #%>%
  #gfplot:::utm2ll(., utm_zone = 9)


bio5perc <- sum(gf3$mean_biomass, na.rm = TRUE) * 0.0001
s <- sort(gf3$mean_biomass)

bio_sum <- cumsum(s)
lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]

gf3t <- filter(gf3, mean_biomass > lower_density_threshold) #%>%
   #mutate(velocity = velocity/year_range)

# gf3 <- left_join(gf3, d_mean)
# 
# gf3$est_diff <- exp(gf3$est_2) * 10000 - exp(gf3$est_1) * 10000
# gf3$est_exp_1 <- exp(gf3$est_1) * 10000
# 
# # gf3 <- mutate(gf3, velocity = if_else(gradient < 0.001, NA_real_, velocity))
# gf3t <- filter(gf3, mean_est > lower_density_threshold) %>%
#   mutate(velocity = velocity/year_range)
# 
# #grad_threshold <- quantile(gf3t$gradient, 0.0001)
# gf3t <- mutate(gf3t, velocity = if_else(gradient < grad_threshold,
#   NA_real_, velocity))
# 
# 
######################################
### PLOT GRADIENT-BASED VELOCITY RASTER 
######################################

# r2 <- range(gf2$velocity, na.rm = TRUE) 
# r3 <- range(gf3$velocity, na.rm = TRUE) 
# raster_min <- -64 #min(c(r2, r3))
# raster_max <- 64 #max(c(r2, r3))

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
 # raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col =  sqrt # no_trans #
) + labs(
  title = paste0(species), #
  subtitle = paste0("Biotic velocity ", min(d$year), "-", max(d$year))
)
grad3


grad4 <- gfranges::plot_vocc(gf3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "mean_biomass",
  fill_label = "mean",
  raster_alpha = 1,
  # raster_limits = c(raster_min, raster_max),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col =  sqrt#
) + labs(
  title = paste0(species), #
  subtitle = paste0("Biotic velocity ", min(d$year), "-", max(d$year))
)
grad4


grad5 <- gfranges::plot_vocc(gf3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "trend_per_year",
  fill_label = "biomass trend",
  raster_alpha = 1,
  #raster_limits = c(0, 10),
  na_colour = "black",
  white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt #
) + labs(
  title = paste0(species), #
  subtitle = paste0("Biotic velocity ", min(d$year), "-", max(d$year))
)
grad5


grad6 <- gfranges::plot_vocc(gf3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "cv",
  fill_label = "sd in log space",
  raster_alpha = 1,
  #raster_limits = c(0, 7),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt #
) + labs(
  title = paste0(species), #
  subtitle = paste0("Biotic velocity ", min(d$year), "-", max(d$year))
)
grad6


raster_max <- quantile(gf3$CV, 0.90, na.rm = TRUE)
grad7 <- gfranges::plot_vocc(gf3t,
  # theme = "black",
  coast = TRUE,
  vec_aes = NULL,
  #grad_vec_aes = "velocity",
  #vec_lwd_range = c(0.5, 0.5),
  NA_label = "NA",
  fill_col = "CV",
  fill_label = "CV",
  raster_alpha = 1,
  raster_limits = c(0, raster_max),
  na_colour = "black",
  # white_zero = TRUE,
  high_fill = "Steel Blue 4",
  low_fill = "Red 3",
  axis_lables = FALSE,
  transform_col = sqrt #
) + labs(
  title = paste0(species), #
  subtitle = paste0("Biotic velocity ", min(d$year), "-", max(d$year))
)
grad7

# gf4 <- dmn %>% filter(year == 2009) %>% mutate(x = X, y = Y, diff = exp(depth_est)*10000 - exp(est)*10000)
# 
# grad4 <- gfranges::plot_vocc(gf4,
#   # theme = "black",
#   coast = TRUE,
#   vec_aes = NULL,
#   #grad_vec_aes = "velocity",
#   #vec_lwd_range = c(0.5, 0.5),
#   NA_label = "NA",
#   fill_col = "diff",
#   fill_label = "depth only est \n- climate est (kg/ha)",
#   raster_alpha = 1,
#   #raster_limits = c(-0.5, 1.5),
#   na_colour = "black",
#   # white_zero = TRUE,
#   high_fill = "Steel Blue 4",
#   low_fill = "Red 3",
#   axis_lables = FALSE,
#   transform_col =   sqrt #no_trans # 
# ) + labs(
#   title = "comparison of biotic estimates from depth and climate models", #
#   subtitle = paste0(min(gf4$year))
# )
# grad4

######################################
### SAVE Gradient-based PLOTS
######################################

# png(
#   file = paste0("figs/", spp, "/biotic-only-", scale_fac, 
#     "-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), 
#     "trim.png"),
#   res = 600,
#   units = "in",
#   width = 8.5,
#   height = 8.5
# )
# gridExtra::grid.arrange(
#   grad2, grad0, grad3,
#   nrow = 2
# )
# dev.off()



######################################
### Calculate gradient-based biotic lags
######################################

biotic_values <- gf3t %>% rename(
    biotic_vel = velocity, 
    biotic_trend = trend_per_year, 
    biotic_CV = CV,
    sd_est = cv,
    biotic_grad = gradient) %>% 
  mutate(sd_biomass = sd*10000,
    mean_biomass = exp(mean)*10000) %>%
  select(x, y, biotic_vel, biotic_trend, biotic_grad, biotic_CV, sd_est, sd_biomass, mean_biomass) %>% 
  mutate(start_year = !!start_year)

DO_values <- gf0 %>% rename( 
    DO_vel = velocity, 
    DO_trend = trend_per_year, 
    DO_grad = gradient) %>% 
  select(x, y, DO_vel, DO_trend, DO_grad)

temp_values <- gf2 %>% rename( 
    temp_vel = velocity, 
    temp_trend = trend_per_year, 
    temp_grad = gradient) %>% 
  select(x, y, temp_vel, temp_trend, temp_grad)

vocc <- left_join(biotic_values, DO_values) # add DO
vocc  <- left_join(vocc , temp_values) # add temp
vocc  <- mutate(vocc, species = !!species, method = "gradient")

write.csv(vocc, file = paste0(
  "data/biotic_test/biotic-vocc-", spp, covs, "-", ssid_string, "-", min(d$year), "-", max(d$year), ".csv"
))

