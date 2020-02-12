
# choose base year(s) to create grid from
# dummy_year <- c(2005, 2006)
bath <- readRDS("../VOCC/data/bathymetry-data")
# dat <- bath$data %>% scale_survey_predictors()
dat <- bath$data %>% mutate(raw_depth = depth, depth = log(raw_depth))
dat <- gfranges::scale_predictors(dat, predictors = c(quo(depth)))

# create grids for each ssid separately
grid_locs <- gfplot::synoptic_grid %>%
  dplyr::filter(survey == "SYN QCS") %>%
  dplyr::select(X, Y, depth)
grid_locs$depth_scaled <- (log(grid_locs$depth) - dat$depth_mean[1])/dat$depth_sd[1]
grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
# Expand the prediction grid to create a slice for each time:
original_time1 <- sort(unique(filter(dat, ssid == 1)$year))
nd_1 <- do.call("rbind",
  replicate(length(original_time1), grid_locs, simplify = FALSE))
nd_1[["year"]] <- rep(original_time1, each = nrow(grid_locs))
nd_1[["ssid"]] <- 1
unique(nd_1$year)

grid_locs <- gfplot::synoptic_grid %>%
  dplyr::filter(survey == "SYN HS") %>%
  dplyr::select(X, Y, depth)
grid_locs$depth_scaled <- (log(grid_locs$depth) - dat$depth_mean[1])/dat$depth_sd[1]
grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
# original_time3 <- sort(unique(filter(dat, ssid == 3)$year))
nd_3 <- do.call("rbind",
  replicate(length(original_time1), grid_locs, simplify = FALSE))
nd_3[["year"]] <- rep(original_time1, each = nrow(grid_locs))
nd_3[["ssid"]] <- 3
unique(nd_3$year)

# grid_locs <- gfplot::synoptic_grid %>%
#       dplyr::filter(survey == "SYN WCVI") %>%
#       dplyr::select(X, Y, depth)
# grid_locs$depth_scaled <- (log(grid_locs$depth) - dat$depth_mean[1]) / dat$depth_sd[1]
#   grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
# original_time4 <- sort(unique(filter(dat, ssid == 4)$year))
# nd_4 <- do.call("rbind",
# replicate(length(original_time4), grid_locs, simplify = FALSE))
# nd_4[["year"]] <- rep(original_time4, each = nrow(grid_locs))
# nd_4[["ssid"]] <- 4
# unique(nd_4$year)
# 
# grid_locs <- gfplot::synoptic_grid %>%
#       dplyr::filter(survey == "SYN WCHG") %>%
#       dplyr::select(X, Y, depth)
# grid_locs$depth_scaled <- (log(grid_locs$depth) - dat$depth_mean[1])/dat$depth_sd[1]
# grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
# original_time16 <- sort(unique(filter(dat, ssid == 16)$year))
# nd_16 <- do.call("rbind",
# replicate(length(original_time4), grid_locs, simplify = FALSE))
# nd_16[["year"]] <- rep(original_time4, each = nrow(grid_locs))
# nd_16[["ssid"]] <- 16
# unique(nd_16$year)
# 
# nd_all <- rbind(nd_1, nd_3, nd_4) %>% #, nd_16
# mutate(year = as.numeric(year))
# saveRDS(nd_all, file = paste0("data/nd_just_depth.rds"))

nd_odd <- rbind(nd_1, nd_3) %>% mutate(year = as.numeric(year))
saveRDS(nd_odd, file = paste0("data/nd_odd.rds"))
#  
# nd_even <- rbind(nd_4, nd_16) %>% mutate(year = as.numeric(year))
# saveRDS(nd_even, file = paste0("data/nd_even.rds"))
#   
#   
# grid_locs <- gfplot::synoptic_grid %>%
#       dplyr::select(X, Y, depth, survey)
# grid_locs$depth_scaled <- (log(grid_locs$depth) - dat$depth_mean[1]) / dat$depth_sd[1]
# grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
# original_time <- sort(unique(data$year))
# nd_whole_coast_index <- do.call("rbind",
#   replicate(length(original_time), grid_locs, simplify = FALSE))
# nd_whole_coast_index[["year"]] <- rep(original_time, each = nrow(grid_locs))
# 
# saveRDS(nd_whole_coast_index, file = paste0("data/nd_whole_coast_index.rds"))
