# GET SEA SURFACE TEMPERATURE 

# based on https://cran.r-project.org/web/packages/rerddap/vignettes/Using_rerddap.html
# install.packages("rerddap")
# devtools::install_github("ropensci/rerddap")



# TEMPORARY CODE TO WRANGLE SENSOR DATA FROM MULTIPLE SENSORS
# first need to refine gfdata SQL query output before finalizing any of this...

# GET SENSOR DATA
# trawl_depth <- gfdata::get_sensor_data_trawl(
#   ssid = c(1),
#   attribute = c("depth"),
#   spread_attributes = FALSE)
# trawl_temp <- gfdata::get_sensor_data_trawl(
#   ssid = c(1),
#   attribute = c("temperature"),
#   spread_attributes = FALSE)
## saveRDS(trawl_depth, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-depth.rds")
## saveRDS(trawl_temp, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-temp.rds")
#
# trawl_depth <- readRDS(here::here("analysis/tmb-sensor-explore/dat-sensor-trawl-depth.rds"))
# trawl_temp <- readRDS(here::here("analysis/tmb-sensor-explore/dat-sensor-trawl-temp.rds"))
# library(dplyr)
# trawl_date <- trawl_temp %>% select (fishing_event_id, survey_desc, start_time, end_time)
# trawl_date$date <- format(as.Date(trawl_date$start_time), "%Y-%m-%d")
# # trawl_date$month <- as.numeric(format(as.Date(trawl_date$start_time), "%m"))
# # trawl_date$day <- as.numeric(format(as.Date(trawl_date$start_time), "%d"))
#
## combine duplicate rows for fishing events from two sensors
# aggdepth <- trawl_depth %>%
#   group_by(fishing_event_id) %>%
#   summarise_if(is.numeric, mean, na.rm=TRUE) %>%
#   select(-count) %>%
#   rename(depth_m = avg, depth_min = min, depth_max = max)
# aggtemp <- trawl_temp %>%
#   group_by(fishing_event_id) %>%
#   summarise_if(is.numeric, mean, na.rm=TRUE) %>%
#   select(fishing_event_id, avg, count) %>%
#   rename(temperature_c = avg)
#
# sd_trawl <- inner_join(trawl_date, aggdepth, by ="fishing_event_id")
# sd_trawl <- inner_join(sd_trawl, aggtemp, by ="fishing_event_id", suffix = c(".depth",".temp"))
#
## saveRDS(sd_trawl, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-processed.rds")

sd_trawl <- readRDS(here::here("analysis/tmb-sensor-explore/dat-sensor-trawl-processed.rds"))

# View(sd_trawl)

# GET SST AT LOCATION OF AND ON DAY OF FISHING EVENT
get_event_SST <- function(data,
                          latitude = "latitude",
                          longitude = "longitude",
                          date = "date") {
  data <- data[!is.na(data$latitude), ]
  data <- data[!is.na(data$longitude), ]
  data <- data[!is.na(data$date), ]
  lat <- data[[latitude]]
  lon <- data[[longitude]]
  date <- data[[date]]
  sstInfo <- rerddap::info("jplMURSST41")
  SST <- list()
  data$SST <- NA
  for (i in seq_len(nrow(data))) {
    SST[[i]] <- rerddap::griddap(sstInfo,
      latitude = c(lat[i], lat[i]),
      longitude = c(lon[[i]], lon[i]),
      time = c(date[i], date[i]),
      fields = "analysed_sst"
    )
    data$SST[i] <- SST[[i]]$data$analysed_sst
  }
  data
}

new_sd_trawl <- get_event_SST(sd_trawl)

# rows with NAs removed by get_SST
nrow(sd_trawl) - nrow(new_sd_trawl)

# convert lat and lon to UTMs
new_sd_trawl <- new_sd_trawl %>%
  mutate(X = longitude, Y = latitude) %>%
  gfplot:::ll2utm(., utm_zone = 9)

# saveRDS(new_sd_trawl, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-SST.rds")



# GET SST AVERAGE FOR PERIOD
# BETWEEN FIXED 'START' AND 'END' DAYS FOR EACH YEAR IN DATAFRAME

get_mean_SST <- function(data,
                         start = "-05-01",
                         end = "-07-31",
                         latitude = "latitude",
                         longitude = "longitude") {
  data <- data[!is.na(data$latitude), ]
  data <- data[!is.na(data$longitude), ]
  data <- data[!is.na(data$year), ]
  lat <- data[[latitude]]
  lon <- data[[longitude]]
  sstInfo <- rerddap::info("jplMURSST41")
  SST <- list()
  data$meanSST <- NA
  for (i in seq_len(nrow(data))) {
    start_date <- paste(data$year[i], start, sep = "")
    end_date <- paste(data$year[i], end, sep = "")

    SST[[i]] <- rerddap::griddap(sstInfo,
      latitude = c(lat[i], lat[i]),
      longitude = c(lon[[i]], lon[i]),
      time = c(start_date, end_date),
      fields = "analysed_sst"
    )
    data$meanSST[i] <- mean(SST[[i]]$data$analysed_sst)
  }
  data
}

# FIXME: this code works for some periods and not others
#    Error in R_nc4_open: NetCDF: Unknown file format
#    Error in ncdf4::nc_open(file) :
#        Error in nc_open trying to open file
#    /Users/dfomac/Library/Caches/R/rerddap/2d3403596ec1641ba7fa9a5468d8370a.nc

new_sd_trawl <- readRDS(here::here("analysis/tmb-sensor-explore/dat-sensor-trawl-SST.rds"))

# sd_trawl_meanSST <- get_mean_SST(new_sd_trawl[1:500,], start = "-06-02", end = "-06-30")
# sd_trawl_meanSSTx <- get_mean_SST(new_sd_trawl[654:656,], start = "-06-02", end = "-06-30")
# some rows (eg. 654:656; all same location) have an issue with -06-01

sd_trawl_meanSST2 <- get_mean_SST(new_sd_trawl[501:656, ], start = "-06-01", end = "-06-30")
sd_trawl_meanSST3 <- get_mean_SST(new_sd_trawl[1001:1500, ], start = "-06-02", end = "-06-30")

# saveRDS(sd_trawl_meanSST, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-meanSST.rds")
# other misc code ideas
# sst <- tabledap(sstInfo, fields = c('latitude','longitude','time'), 'latitude>=50.9', 'latitude<=52.7', 'longitude>=-131.3', 'longitude<=-127.8', 'time>=2003-07-01', 'time<=2003-07-31')



# MAKE DATAFRAME OF BOTH SST AND meanSST

# just_meanSST <- sd_trawl_meanSST %>% select(fishing_event_id, meanSST)
# sd_trawl_meanSST <- full_join(new_sd_trawl, just_meanSST, by = fishing_event_id)
# saveRDS(sd_trawl_bothSST, file = "analysis/tmb-sensor-explore/dat-sensor-trawl-bothSST.rds")


# GET STT FOR PREDICTION GRIDS
library(sdmTMB)
library(dplyr)

# first convert UTMs to lat lon
qcs_grid_ll <- gfplot:::utm2ll(qcs_grid, utm_zone = 9) %>% rename(longitude = X, latitude = Y) %>% select(latitude, longitude)
qcs_grid_all <- cbind(qcs_grid, qcs_grid_ll)

# tested on few rows it works...
qcs_grid_data <- get_mean_SST(qcs_grid_all[1:10, ], start = "-06-01", end = "-06-30")
qcs_grid_data$SST


# library("rerddap")
# library("gfdata")
# library("akima")
# library("mapdata")
# library("ncdf4")
