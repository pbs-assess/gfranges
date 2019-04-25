# GET SEA SURFACE TEMPERATURE 

# based on https://cran.r-project.org/web/packages/rerddap/vignettes/Using_rerddap.html
# install.packages("rerddap")
# devtools::install_github("ropensci/rerddap")

# GET SENSOR DATA
# trawl_depth <- gfdata::get_sensor_data_trawl(
#   ssid = c(1,4,3,16),
#   attribute = c("depth"),
#   spread_attributes = FALSE)
# trawl_temp <- gfdata::get_sensor_data_trawl(
#   ssid = c(1,4,3,16),
#   attribute = c("temperature"),
#   spread_attributes = FALSE)
# # saveRDS(trawl_depth, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl-depth.rds")
# # saveRDS(trawl_temp, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl-temp.rds")

# TEMPORARY CODE TO WRANGLE SENSOR DATA FROM MULTIPLE SENSORS
#TODO: need to refine gfdata SQL query output before finalizing any of this...

# trawl_depth <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-depth.rds"))
# trawl_temp <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-temp.rds"))
# library(dplyr)
# trawl_date <- trawl_temp %>% select (fishing_event_id, survey_desc, start_time, end_time) %>% distinct()
# trawl_date$date <- format(as.Date(trawl_date$start_time), "%Y-%m-%d")
# trawl_date <- trawl_date %>% select (fishing_event_id, survey_desc, date) %>% distinct()
# # trawl_date$month <- as.numeric(format(as.Date(trawl_date$start_time), "%m"))
# # trawl_date$day <- as.numeric(format(as.Date(trawl_date$start_time), "%d"))
# 
# # combine duplicate rows for fishing events from two sensors
# aggdepth <- trawl_depth %>%
#   group_by(fishing_event_id) %>%
#   summarise_if(is.numeric, mean, na.rm=TRUE) %>%
#   select(-count, -latitude, -longitude, -year, -ssid) %>%
#   rename(depth_m = avg, depth_min = min, depth_max = max)
# aggtemp <- trawl_temp %>%
#   group_by(fishing_event_id) %>%
#   summarise_if(is.numeric, mean, na.rm=TRUE) %>%
#   select(fishing_event_id, year, ssid, latitude, longitude, avg, count) %>%
#   rename(temperature_c = avg)
# 
# sd_trawl1 <- full_join(aggtemp, aggdepth, by ="fishing_event_id")
# sd_trawl <- right_join(trawl_date, sd_trawl1, by ="fishing_event_id")
# sd_trawl[is.na(sd_trawl$year), ]$year <- 2003

# glimpse(sd_trawl)
## saveRDS(sd_trawl, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl-processed.rds")

sd_trawl <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-processed.rds"))

# RECOMMENDED TO DO IN CHUNKS TO AVOID TIMING OUT
sd_trawl <- sd_trawl %>% filter(ssid == 16)


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
    tryCatch({
      SST[[i]]  <- rerddap::griddap(sstInfo,
        url = 'https://coastwatch.pfeg.noaa.gov/erddap/',
        latitude = c(lat[i], lat[i]),
        longitude = c(lon[[i]], lon[i]),
        time = c(date[i], date[i]),
        fields = "analysed_sst"
      )
      data$SST[i] <- SST[[i]]$data$analysed_sst
    }, error=function(x){
      data$SST[i] <- "NA"
    })
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


# GET SST AVERAGE FOR PERIOD
# BETWEEN FIXED 'START' AND 'END' DAYS FOR EACH YEAR IN DATAFRAME

get_mean_SST <- function(data,
  start = "-06-20",
  end = "-06-30",
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
    
    tryCatch({
      SST[[i]]  <- rerddap::griddap(sstInfo,
        url = 'https://coastwatch.pfeg.noaa.gov/erddap/',
        latitude = c(lat[i], lat[i]),
        longitude = c(lon[[i]], lon[i]),
        time = c(start_date, end_date),
        fields = "analysed_sst"
      )
      data$meanSST[i] <- mean(SST[[i]]$data$analysed_sst, na.rm = TRUE)
    }, error=function(x){
      data$meanSST[i] <- "NA"
    })
    # browser()
  }
  data
}

# new_sd_trawl <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-SST-3.rds"))

new_sd_trawl %>% group_by(year) %>% summarise(start = min(date), end = max(date))
#TODO: if meanSST seems useful, we could fine-tune date range to be year specific?
# for now these values were used to choose an ~ 1.5 month window that covers about 2 weeks before and the whole survey window in most years

# ssid = 1 
# 1  2003 2003-07-04 2003-08-08
# 2  2004 2004-07-06 2004-08-07
# 3  2005 2005-07-06 2005-08-06
# 4  2007 2007-07-04 2007-08-01
# 5  2009 2009-07-09 2009-08-06
# 6  2011 2011-07-06 2011-07-29
# 7  2013 2013-07-04 2013-07-26
# 8  2015 2015-07-08 2015-08-08
# 9  2017 2017-07-05 2017-07-30

# ssid = 3 
# 1  2005 2005-05-27 2005-06-27
# 2  2007 2007-05-24 2007-06-16
# 3  2009 2009-05-28 2009-06-18
# 4  2011 2011-05-26 2011-06-18
# 5  2013 2013-05-30 2013-06-21
# 6  2015 2015-05-28 2015-06-20
# 7  2017 2017-05-21 2017-06-12

# ssid = 4 
# 1  2004 2004-05-26 2004-06-09
# 2  2006 2006-05-24 2006-06-18
# 3  2008 2008-05-27 2008-06-21
# 4  2010 2010-06-08 2010-06-28
# 5  2012 2012-05-23 2012-06-15
# 6  2014 2014-05-29 2014-06-20
# 7  2016 2016-05-25 2016-06-14
# 8  2018 2018-05-19 2018-06-12

# ssid = 16
# 1  2006 2006-08-31 2006-09-21
# 2  2007 2007-09-14 2007-10-12
# 3  2008 2008-08-28 2008-09-18
# 4  2010 2010-08-29 2010-09-16
# 5  2012 2012-08-27 2012-09-16
# 6  2014 2014-08-30 2014-09-26
# 7  2016 2016-08-28 2016-09-24
# 8  2018 2018-09-05 2018-09-19

# note: fishing_event_id == 3234358 is missing lat/lon data

# sd_trawl_meanSST1 <- get_mean_SST(new_sd_trawl[1:nrow(new_sd_trawl),], start = "-06-15", end = "-07-31")
sd_trawl_meanSST1 <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-SST-1.rds"))

# sd_trawl_meanSST3 <- get_mean_SST(new_sd_trawl[1:nrow(new_sd_trawl),], start = "-05-10", end = "-06-10")
# shortened duration from 06-20 to 60-10 due to majority failed retrieval 
sd_trawl_meanSST3 <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-SST-3.rds"))

# sd_trawl_meanSST4 <- get_mean_SST(new_sd_trawl[1:nrow(new_sd_trawl),], start = "-05-10", end = "-06-20")
sd_trawl_meanSST4 <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-SST-4.rds"))

# sd_trawl_meanSST16 <- get_mean_SST(new_sd_trawl[1:nrow(new_sd_trawl),], start = "-08-15", end = "-09-30")
sd_trawl_meanSST16 <- readRDS(here::here("analysis/tmb-sensor-explore/data/dat-sensor-trawl-SST-16.rds"))



trawl_meanSST <- dplyr::bind_rows(list(
  sd_trawl_meanSST1, 
  sd_trawl_meanSST3,
  sd_trawl_meanSST4,
  sd_trawl_meanSST16
  ))

# dplyr::glimpse(trawl_meanSST)
# View(trawl_meanSST)
# saveRDS(trawl_meanSST, file = "analysis/tmb-sensor-explore/data/dat-sensor-trawl-meanSST-all.rds")

# retrieve SST values from prior run to add to new sensor data pull
# sst_trawl <- d_trawl %>% select(fishing_event_id, SST, X, Y) %>% distinct()
# new_sd_trawl <- left_join(sd_trawl, sst_trawl, by="fishing_event_id")



# GET STT FOR PREDICTION GRIDS
library(sdmTMB)
library(dplyr)

# first convert UTMs to lat lon
qcs_grid_ll <- gfplot:::utm2ll(qcs_grid, utm_zone = 9) %>% rename(longitude = X, latitude = Y) %>% select(latitude, longitude)
qcs_grid_all <- cbind(qcs_grid, qcs_grid_ll)

# tested on few rows it works...
qcs_grid_data <- get_mean_SST(qcs_grid_all[1:10, ], start = "-06-01", end = "-06-30")
qcs_grid_data$SST
