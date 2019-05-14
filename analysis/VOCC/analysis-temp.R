

# devtools::install_github("seananderson/vocc")
# install.package(ggnewscale)
# install.package(gfplot)

library(dplyr)
library(ggplot2)
library(sdmTMB)

all_depth <- readRDS("analysis/tmb-sensor-explore/data/dat-sensor-trawl-all-depth.rds")

# add and scale predictors after filtering for survey(s) of interest
data <- all_depth$data %>%
  filter(depth_max>10) %>%
  dplyr::filter(year > 2003) %>%
  # filter(ssid != 1) %>%
  # filter(ssid != 3) %>%
  # filter(ssid != 4) %>%
  # filter(ssid != 16) %>%
  gfplot:::scale_survey_predictors()

# remove obvious sensor fail, d_trawl %>% filter(ssid==4, temperature_c >12), fishing event id = 481861
data <- data[data$fishing_event_id != 481861, ]
# add any new predictors
# data <- data %>%
#   dplyr::mutate(
#   DOY = lubridate::yday(date),
#   shallow = ifelse(depth > 35, 0, 1)
# )


# choose base year(s) to create grid from
dummy_year <- c(2004, 2005)

# create grids for each ssid separately so that only years with data are included
ssid <- 1
survey_abbrev <- "SYN QCS"
nd_1 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)
unique(nd_1$year)

ssid <- 3
survey_abbrev <- "SYN HS"
nd_3 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)

ssid <- 4
survey_abbrev <- "SYN WCVI"
nd_4 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year)

ssid <- 16
survey_abbrev <- "SYN WCHG"
nd_16 <- spatiotemporal_grid(data, ssid, survey_abbrev, dummy_year = 2006)

nd <- rbind(nd_1, nd_3, nd_4, nd_16)
nd <- nd %>% dplyr::mutate(shallow = ifelse(depth > 35, 0, 1))


# # choose the spatial range to model
# dat <- data # %>% filter(ssid == 4)
# nd <- nd # %>% filter(ssid == 4)
# 
# spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = 500)
# sdmTMB::plot_spde(spde)
# 
# m_temp <- sdmTMB::sdmTMB(dat,
#   temperature_c ~ 0 + as.factor(year),
#   time_varying = ~ 0 + depth_scaled + depth_scaled2,
#   time = "year", spde = spde,
#   family = gaussian(link = "identity"),
#   ar1_fields = TRUE, # maybe TRUE is better for all areas combined?
#   include_spatial = TRUE,
#   silent = FALSE
# )
# 
# stopifnot(m_temp$model$convergence == 0L)
# m_temp
# # Warning messages:
# #   1: In doTryCatch(return(expr), name, parentenv, handler) :
# #   restarting interrupted promise evaluation
# 
# saveRDS(m_temp, file = "analysis/tmb-sensor-explore/data/m_temp_allpost2003.rds")



m_temp <- readRDS("analysis/tmb-sensor-explore/data/m_temp_allpost2003.rds")

predictions <- predict(m_temp, newdata = nd)

plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed()
}

p <- plot_map(predictions , "est") +
  scale_fill_viridis_c(trans = "sqrt", option = "C") +
  ggtitle("Prediction (fixed effects + all random effects)")
print(p)

# choose the spatial range to build raster on

predicted1 <- predictions %>% filter(ssid == 1) 
predicted3 <- predictions %>% filter(ssid == 3) 
predicted4 <- predictions %>% filter(ssid == 4) 
predicted16 <- predictions %>% filter(ssid == 16) 



# scale_fac = 2 means that the raster is reprojected to 2 X original grid (2 km)
rbrick <- make_raster_brick(predicted3, scale_fac = 2)
saveRDS(rbrick, file = "analysis/rbrick-temp-hs.rds")

# scale_fac = 2 means that the raster is reprojected to 2 X original grid (2 km)
rbrick <- make_raster_brick(predicted1, scale_fac = 2)
saveRDS(rbrick, file = "analysis/rbrick-temp-qcs.rds")

# scale_fac = 2 means that the raster is reprojected to 2 X original grid (2 km)
rbrick <- make_raster_brick(predicted4, scale_fac = 2)
saveRDS(rbrick, file = "analysis/rbrick-temp-wcvi.rds")

# scale_fac = 2 means that the raster is reprojected to 2 X original grid (2 km)
rbrick <- make_raster_brick(predicted16, scale_fac = 2)
saveRDS(rbrick, file = "analysis/rbrick-temp-wchg.rds")



#####
# QCS
#####

temp_rbrick_qcs <- readRDS("analysis/rbrick-temp-qcs.rds")
glimpse(temp_rbrick_qcs)
slopedat_qcs <- calcslope(temp_rbrick_qcs)
mnraster_brick1 <- raster::stackApply(temp_rbrick_qcs, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick_qcs, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
start_temp_qcs <- mnraster_brick1[[1]]
end_temp_qcs <- mnraster_brick2[[3]]


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data_qcs <- list(temp = start_temp_qcs)
end_data_qcs <- list(temp = end_temp_qcs)


#profvis({
  out1_qcs <- dist_based_vocc(
  start_data = start_data_qcs,
  end_data = end_data_qcs,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.75),
  cell_size = 2,
  delta_t = 10,
  raster = TRUE
  )
  #})

out1_qcs <- left_join(out1_qcs, slopedat_qcs, by = c("x", "y")) %>% select(-icell)
out1_qcs$C_per_decade <- out1_qcs$slope * 10
out1_qcs$km_per_decade <- out1_qcs$distance * 10 / 10 # dived by delta_t
head(out1_qcs)
#saveRDS(out1, file = "analysis/simple-dist-vocc-qcs1.rds")

gvocc1_qcs <- plot_vocc(out1_qcs,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,1),
  #vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.4
)
gvocc1_qcs


out2_qcs <- dist_based_vocc(
  start_data = start_data_qcs,
  end_data = end_data_qcs,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.95),
  cell_size = 4,
  delta_t = 10,
  raster = TRUE
)

out2_qcs <- left_join(out2_qcs, slopedat_qcs, by = c("x", "y")) %>% select(-icell)
out2_qcs$C_per_decade <- out2_qcs$slope * 10
out2_qcs$km_per_decade <- out2_qcs$distance * 10 / 10 # dived by delta_t
head(out2_qcs)
#saveRDS(out2, file = "analysis/simple-dist-vocc-qcs2.rds")
View(out2_qcs)
out2_qcs_1per <- do.call(rbind, lapply(split(out2_qcs, out2_qcs$id), head, 1)) %>%
  mutate(target_X = mean_target_X, target_Y = mean_target_Y)
View(out2_qcs_1per)

gvocc2_qcs <- plot_vocc(out2_qcs,
  low_col = "white",
  high_col = "white", 
  vec_lwd_range = c(1,1),
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.2
)
gvocc2_qcs

gvocc2_qcs1 <- plot_vocc(out2_qcs_1per,
  low_col = "white",
  mid_col = "white",
  high_col = "white", 
  vec_aes = "distance",
  vec_lwd_range = c(1,1),
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.2
)
gvocc2_qcs1

#head(out1)
# gvocc <- plot_vocc(out2,
#   #vec_col = "Dark Slate Gray",
#   low_col = "Medium Purple",
#   mid_col = "grey97",
#   high_col = "White",
#   vec_lwd = "distance",
#   vec_lwd_range = c(1,2),
#   vec_col = "C_per_decade",
#   fill_col = "temp_e",
#   fill_label = "Current\ntemperature",
#   raster_alpha = 1,
#   vec_alpha = 0.4
# )
# gvocc



#####
# HS
#####


temp_rbrick_hs <- readRDS("analysis/rbrick-temp-hs.rds")
glimpse(temp_rbrick_hs)
slopedat_hs <- vocc::calcslope(temp_rbrick_hs)
mnraster_brick_hs1 <- raster::stackApply(temp_rbrick_hs, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
mnraster_brick_hs2 <- raster::stackApply(temp_rbrick_hs, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
start_temp_hs <- mnraster_brick_hs1[[1]]
end_temp_hs <- mnraster_brick_hs2[[3]]


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data_hs <- list(temp = start_temp_hs)
end_data_hs <- list(temp = end_temp_hs)


#profvis({
out1_hs <- dist_based_vocc(
  start_data = start_data_hs,
  end_data = end_data_hs,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(.95),
  cell_size = 4,
  delta_t = 10,
  raster = TRUE
)
#})

out1_hs <- left_join(out1_hs, slopedat_hs, by = c("x", "y")) %>% select(-icell)
out1_hs$C_per_decade <- out1_hs$slope * 10
out1_hs$km_per_decade <- out1_hs$distance * 10 / 10 # dived by delta_t
head(out1_hs)
#saveRDS(out1, file = "analysis/simple-dist-vocc-qcs1.rds")

gvocc1_hs <- plot_vocc(out1_hs,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,1),
  vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.4
)
gvocc1_hs


#####
### STICH TOGETHER HS and QCS
#####

# must have same time slices, so exclude 2004
predicted1n3 <- predictions %>% filter(ssid != 16) %>% filter(ssid != 4) %>% filter(year != 2004) 

# scale_fac = 3 means that the raster is reprojected to 3 X original grid (2 km)
rbrick <- make_raster_brick(predicted1n3, scale_fac = 3)
saveRDS(rbrick, file = "analysis/rbrick-temp-hs-qcs.rds")

temp_rbrick_stitched <- readRDS("analysis/rbrick-temp-hs-qcs.rds")
glimpse(temp_rbrick_stitched)
slopedat_stitched <- vocc::calcslope(temp_rbrick_stitched)
mnraster_brick1 <- raster::stackApply(temp_rbrick_stitched, indices = c(1, 1, 2, 2, 2, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick_stitched, indices = c(1, 1, 2, 2, 2, 3, 3), fun = mean)
start_temp_stitched <- mnraster_brick1[[1]]
end_temp_stitched <- mnraster_brick2[[3]]


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data_stitched <- list(temp = start_temp_stitched)
end_data_stitched <- list(temp = end_temp_stitched)


#profvis({
out1_stitched <- dist_based_vocc(
  start_data = start_data_stitched,
  end_data = end_data_stitched,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.75),
  cell_size = 6,
  delta_t = 12,
  raster = TRUE
)
#})

out1_stitched <- left_join(out1_stitched, slopedat_stitched, by = c("x", "y")) %>% select(-icell)
out1_stitched$C_per_decade <- out1_stitched$slope*2 # 2 is because the vocc::slope function assumes time steps of one year
out1_stitched$km_per_decade <- out1_stitched$distance * 10 / 10 # dived by delta_t
head(out1_stitched)
# View(out1_stitched)

gvocc <- plot_vocc(out1_stitched,
  low_col = "white",
  mid_col = "white",
  high_col = "grey97", 
  vec_lwd = "distance",
  vec_lwd_range = c(0.5,0.6),
  vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_s",
  fill_label = "Mean\ntemperature\n2005 & 2007",
  raster_alpha = 1,
  vec_alpha = 0.8
)
gvocc


gtrend <- plot_vocc(out1_stitched,
  max_vec_plotted = NULL,
  fill_col = "C_per_decade",
  fill_label = "Temperature\ntrend (°C/decade)\nfor 2005-2017",
  raster_alpha = 1,
  vec_alpha = 0.8
)
gtrend

png(file = "accasp-wg-fig.png",   # The directory you want to save the file in
  res = 600,
  units = 'in',
  width = 10, # The width of the plot in inches
  height = 7) # The height of the plot in inches

gridExtra::grid.arrange(gtrend, gvocc, nrow = 1)
 dev.off()
# start_temp <- raster::mosaic(start_data_hs, start_data_qcs)
# end_temp <- raster::mosaic(end_temp_hs, end_temp_qcs)
# 
# # data with just one climate variable
# start_data <- list(temp = start_temp)
# end_data <- list(temp = end_temp)


out1_wcvi <- dist_based_vocc(
  start_data = start_data,
  end_data = end_data,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.49),
  cell_size = 4,
  max_dist = NULL,
  delta_t = 10,
  raster = TRUE
)

out1_wcvi <- left_join(out1_wcvi, slopedat_wcvi, by = c("x", "y")) %>% select(-icell)
out1_wcvi$C_per_decade <- out1_wcvi$slope * 10
out1_wcvi$km_per_decade <- out1_wcvi$distance * 10 / 5 # dived by delta_t
head(out1_wcvi)

#saveRDS(out1_wcvi, file = "analysis/simple-dist-vocc-wcvi.rds")
#saveRDS(out1_wcvi, file = "analysis/knn-dist-vocc-wcvi.rds")


gvocc_wcvi <- plot_vocc(out1_wcvi,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,1),
  vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.8
)
gvocc_wcvi





#####
# WCVI
#####



temp_rbrick <- readRDS("analysis/rbrick-temp-WCVI.rds")
glimpse(temp_rbrick)
slopedat_wcvi <- vocc::calcslope(temp_rbrick)
mnraster_brick1 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
start_temp <- mnraster_brick1[[1]]
end_temp <- mnraster_brick2[[3]]

# data with just one climate variable
start_data_wcvi <- list(temp = start_temp)
end_data_wcvi <- list(temp = end_temp)


out1_wcvi <- dist_based_vocc(
  start_data = start_data_wcvi,
  end_data = end_data_wcvi,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.49),
  cell_size = 4,
  max_dist = NULL,
  delta_t = 10,
  raster = TRUE
)

out1_wcvi <- left_join(out1_wcvi, slopedat_wcvi, by = c("x", "y")) %>% select(-icell)
out1_wcvi$C_per_decade <- out1_wcvi$slope * 10
out1_wcvi$km_per_decade <- out1_wcvi$distance * 10 / 5 # dived by delta_t
head(out1_wcvi)

#saveRDS(out1_wcvi, file = "analysis/simple-dist-vocc-wcvi.rds")
#saveRDS(out1_wcvi, file = "analysis/knn-dist-vocc-wcvi.rds")


gvocc_wcvi <- plot_vocc(out1_wcvi,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,1),
  vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.8
)
gvocc_wcvi

#####
# WCHG
#####

temp_rbrick_wchg <- readRDS("analysis/rbrick-temp-wchg.rds")
glimpse(temp_rbrick_wchg)
slopedat_wchg <- vocc::calcslope(temp_rbrick_wchg)
mnraster_brick1 <- raster::stackApply(temp_rbrick_wchg, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick_wchg, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
start_temp_wchg <- mnraster_brick1[[1]]
end_temp_wchg <- mnraster_brick2[[3]]


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data_wchg <- list(temp = start_temp_wchg)
end_data_wchg <- list(temp = end_temp_wchg)


#profvis({
out1_wchg <- dist_based_vocc(
  start_data = start_data_wchg,
  end_data = end_data_wchg,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.25),
  cell_size = 4,
  delta_t = 10,
  raster = TRUE
)
#})

out1_wchg <- left_join(out1_wchg, slopedat_wchg, by = c("x", "y")) %>% select(-icell)
out1_wchg$C_per_decade <- out1_wchg$slope * 10
out1_wchg$km_per_decade <- out1_wchg$distance * 10 / 10 # dived by delta_t
head(out1_wchg)
#saveRDS(out1, file = "analysis/simple-dist-vocc-qcs1.rds")

gvocc1_wchg <- plot_vocc(out1_wchg,
  low_col = "white",
  mid_col = "white",
  high_col = "grey87", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,1),
  vec_col = "distance",
  max_vec_plotted = 100,
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.4
)
gvocc1_wchg



# #head(out1)
# gvocc <- plot_vocc(out2,
#   #vec_col = "Dark Slate Gray",
#   low_col = "Medium Purple",
#   mid_col = "grey97",
#   high_col = "White",
#   vec_lwd = "distance",
#   vec_lwd_range = c(1,3),
#   vec_col = "C_per_decade",
#   fill_col = "temp_e",
#   fill_label = "Current\ntemperature",
#   raster_alpha = 1,
#   vec_alpha = 0.5
# )
# gvocc


# ggplot(out1) +
#   geom_segment(aes(x, y,
#     xend = target_X, yend = target_Y,
#     colour = C_per_decade), arrow=arrow(length=unit(0.2, "cm")), size=0.5) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()
#

# 
# out2 <- out2 %>% mutate(target_temp = as.numeric(target_values))
# out2 <- out2 %>% mutate(temp_diff = temp_s-target_temp)
# 
# out2[out2$distance > 80, ]$target_X <- NA
# out2[out2$distance > 80, ]$target_Y <- NA
# 
