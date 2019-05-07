# Simple example data in raster form
temp_rbrick <- readRDS("analysis/rbrick-temp-qcs.rds")
glimpse(temp_rbrick)
slopedat <- vocc::calcslope(temp_rbrick)
mnraster_brick1 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 3, 3, 3), fun = mean)
start_temp <- mnraster_brick1[[1]]
end_temp <- mnraster_brick2[[3]]


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

# data with just one climate variable
start_data_qcs <- list(temp = start_temp)
end_data_qcs <- list(temp = end_temp)


out2 <- dist_based_vocc(
  start_data = start_data_qcs,
  end_data = end_data_qcs,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.25),
  cell_size = 4,
  max_dist = 100,
  delta_t = 10,
  raster = TRUE
)
### 50 + errors for qcs... seem to be missing blocks... 


out2 <- left_join(out2, slopedat, by = c("x", "y")) %>% select(-icell)
out2$C_per_decade <- out2$slope * 10
out2$km_per_decade <- out2$distance * 10 / 5 # dived by delta_t
# out1$speed_per_decade <- out1$speed*10
head(out2)

out_trim <- out2 %>% filter(distance<100)
#saveRDS(out2, file = "analysis/simple-dist-vocc-qcs.rds")


# ggplot(out1) +
#   geom_segment(aes(x, y,
#    xend = target_X, yend = target_Y,
#     colour = C_per_decade), size = 1) +
#   #scale_colour_gradient2(low = low_col, high = high_col) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   #labs(colour = col_label) +
#   #coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()



#head(out1)
gvocc <- plot_vocc(out_trim,
  #vec_col = "Dark Slate Gray",
  low_col = "Medium Purple",
  mid_col = "grey97",
  high_col = "White",
  vec_lwd = "distance",
  vec_lwd_range = c(1,2),
  vec_col = "C_per_decade",
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.4
)
gvocc

gvocc <- plot_vocc(out_trim,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,3),
  vec_col = "distance",
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.4
)
gvocc
# ggplot(out1) +
#   geom_segment(aes(x, y,
#     xend = target_X, yend = target_Y,
#     colour = C_per_decade), arrow=arrow(length=unit(0.2, "cm")), size=0.5) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()
#




temp_rbrick <- readRDS("analysis/rbrick-temp-WCVI.rds")
glimpse(temp_rbrick)
slopedat <- vocc::calcslope(temp_rbrick)
mnraster_brick1 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
mnraster_brick2 <- raster::stackApply(temp_rbrick, indices = c(1, 1, 1, 2, 2, 2, 3, 3), fun = mean)
start_temp <- mnraster_brick1[[1]]
end_temp <- mnraster_brick2[[3]]

# data with just one climate variable
start_data1 <- list(temp = start_temp)
end_data1 <- list(temp = end_temp)


out2 <- dist_based_vocc(
  start_data = start_data1,
  end_data = end_data1,
  x = "x",
  y = "y",
  variable_names = c("index_1"),
  thresholds = c(0.5),
  cell_size = 4,
  max_dist = NULL,
  delta_t = 5,
  raster = TRUE
)
### 50 + errors for qcs... seem to be missing blocks... 


out2 <- left_join(out2, slopedat, by = c("x", "y")) %>% select(-icell)
out2$C_per_decade <- out2$slope * 10
out2$km_per_decade <- out2$distance * 10 / 5 # dived by delta_t
# out1$speed_per_decade <- out1$speed*10
head(out2)

saveRDS(out2, file = "analysis/simple-dist-vocc-qcs.rds")
#saveRDS(out2, file = "analysis/knn-dist-vocc-wcvi.rds")


# ggplot(out1) +
#   geom_segment(aes(x, y,
#    xend = target_X, yend = target_Y,
#     colour = C_per_decade), size = 1) +
#   #scale_colour_gradient2(low = low_col, high = high_col) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   #labs(colour = col_label) +
#   #coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()



#head(out1)
gvocc <- plot_vocc(out2,
  #vec_col = "Dark Slate Gray",
  low_col = "Medium Purple",
  mid_col = "grey97",
  high_col = "White",
  vec_lwd = "distance",
  vec_lwd_range = c(1,3),
  vec_col = "C_per_decade",
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.5,
  isobath = isobath
)
gvocc

gvocc <- plot_vocc(out2,
  low_col = "white",
  mid_col = "white",
  high_col = "grey77", 
  vec_lwd = "distance",
  vec_lwd_range = c(1,3),
  vec_col = "distance",
  fill_col = "temp_e",
  fill_label = "Current\ntemperature",
  raster_alpha = 1,
  vec_alpha = 0.5,
  isobath = isobath
)
gvocc
# ggplot(out1) +
#   geom_segment(aes(x, y,
#     xend = target_X, yend = target_Y,
#     colour = C_per_decade), arrow=arrow(length=unit(0.2, "cm")), size=0.5) +
#   scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red", l=50, c=90)) +
#   xlab("UTM") + ylab("UTM") +
#   coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
#   gfplot::theme_pbs()
#

