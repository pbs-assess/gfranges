library(tidyverse)
library(gfplot)
library(sdmTMB)
library(gfranges)


# get older sensor data
library(tidyverse)

d <- gfdata::get_table("FE_SALINITY_DO")
names(d) <- tolower(names(d))
head(d)


.d <- d %>% # left_join(d, fe, by = "fishing_event_id") %>%
  group_by(fishing_event_id) %>%
  mutate(
    # lat = latitude,
    # lon = longitude,
    do_mlpl.y = mean(do, na.rm = TRUE),
    do_mlpl_min.y = min(do, na.rm = TRUE),
    do_mlpl_max.y = max(do, na.rm = TRUE),
    salinity_psu.y = mean(salinity, na.rm = TRUE),
    salinity_psu_min.y = min(salinity, na.rm = TRUE),
    salinity_psu_max.y = max(salinity, na.rm = TRUE),
    do_event_start = min(fe_event_time),
    do_event_end = max(fe_event_time)
  ) %>%
  # rename(X = longitude, Y = latitude) %>%
  add_tally(do, name = "do_mlpl_N.y") %>%
  add_tally(salinity, name = "salinity_psu_N.y") %>%
  mutate(do_mlpl_N.y = as.integer(do_mlpl_N.y), salinity_psu_N.y = as.integer(salinity_psu_N.y)) %>%
  select(-do, -salinity, -fe_event_time)

.d <- unique(.d)


View(.d)
head(.d)
glimpse(.d)
# merge with newer sensor data
new_sensor_data <- readRDS("analysis/tmb-sensor-explore/data/dat-sensor-trawl.rds")
# library(tidyverse)
head(new_sensor_data)
View(new_sensor_data)
.d_trawl <- new_sensor_data %>%
  group_by(fishing_event_id) %>%
  mutate(
    fe_event_start = min(start_time),
    fe_event_end = max(end_time), attribute = tolower(attribute)
  ) %>%
  ungroup() %>%
  reshape2::dcast(
    fishing_event_id + year + ssid + survey_desc + fe_event_start + fe_event_end ~ attribute,
    mean,
    value.var = "avg"
  )

.d_trawl_max <- new_sensor_data %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, mean, value.var = "max")

.d_trawl_min <- new_sensor_data %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, mean, value.var = "min")

.d_trawl_N <- new_sensor_data %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, sum, value.var = "count") %>%
  .d_trawl2() <- full_join(.d_trawl, .d_trawl_max, by = "fishing_event_id", suffix = c("", "_max"))
.d_trawl3 <- full_join(.d_trawl2, .d_trawl_min, by = "fishing_event_id", suffix = c("", "_min"))
.d_trawl4 <- full_join(.d_trawl3, .d_trawl_N, by = "fishing_event_id", suffix = c("", "_N")) %>%
  mutate(
    fishing_event_id = as.integer(fishing_event_id),
    do_mlpl_N = if_else(do_mlpl_N == 0, NA_integer_, do_mlpl_N),
    salinity_psu_N = if_else(salinity_psu_N == 0, NA_integer_, salinity_psu_N)
  )

glimpse(.d_trawl4)
View(.d_trawl4)

all_sensor <- full_join(.d_trawl4, .d, by = "fishing_event_id") %>%
  mutate(
    do_mlpl = coalesce(do_mlpl, do_mlpl.y),
    do_mlpl_max = coalesce(do_mlpl_max, do_mlpl_max.y),
    do_mlpl_min = coalesce(do_mlpl_min, do_mlpl_min.y),
    # note that N values pre2017 are not on same scale as those post2017
    do_mlpl_N = coalesce(do_mlpl_N.y, do_mlpl_N),
    salinity_psu = coalesce(salinity_psu, salinity_psu.y),
    salinity_psu_max = coalesce(salinity_psu_max, salinity_psu_max.y),
    salinity_psu_min = coalesce(salinity_psu_min, salinity_psu_min.y),
    # note that N values pre2017 are not on same scale as those post2017
    salinity_psu_N = coalesce(salinity_psu_N.y, salinity_psu_N),
    # depth_m = coalesce(depth_m.y, depth_m.x),
    year = as.double(year)
  ) %>%
  mutate(year = if_else(is.na(year), lubridate::year(do_event_start), year)) %>%
  select(
    -do_mlpl.y, -do_mlpl_max.y, -do_mlpl_min.y, -do_mlpl_N.y,
    -salinity_psu.y, -salinity_psu_max.y, -salinity_psu_min.y, -salinity_psu_N.y
  )

# # reduced dataframe
# all_sensor <- all_sensor %>%
#   select(fishing_event_id, year, ssid, survey_desc,
#     fe_event_start, fe_event_end,
#     do_event_start, do_event_end,
#     temperature_c,
#     do_mlpl,
#     salinity_psu,
#     depth_m)

glimpse(.d_trawl)
glimpse(all_sensor)
View(all_sensor)
bath <- readRDS(file = "analysis/VOCC/data/bathymetry-data")
bath$data$fishing_event_id <- as.integer(bath$data$fishing_event_id)
glimpse(bath$data$year)
bath$data$year <- as.double(bath$data$year)

d_trawl5 <- left_join(all_sensor, bath$data, by = "fishing_event_id") %>% mutate(
  ssid = coalesce(ssid.x, ssid.y),
  year = coalesce(year.x, year.y)
) %>% select(-year.x, -year.y, -ssid.x, -ssid.y)


# get fishing event locations for sensor samples not included as valid survey samples
fe <- gfdata::run_sql("GFBioSQL", "SELECT
  FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS
  LONGITUDE, FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
  FROM B21_Samples")
names(fe) <- tolower(names(fe))
head(fe)

d_trawl6 <- left_join(d_trawl5, fe, by = "fishing_event_id") %>%
  mutate(
    lat = coalesce(lat, latitude),
    lon = coalesce(lon, longitude),
    depth_bath = coalesce(depth, depth_m.y),
    depth_edit = coalesce(depth_m.x, depth_bath),
    depth_edit = if_else(depth_m.x < 20, depth_bath, depth_m.x),
    depth = round(depth_edit),
    log_depth = log(depth),
    depth_diff = abs(depth_bath - depth_m.x),
    depth_range = depth_m_max - depth_m_min,
    depth_change = depth_m_max - depth_m.x,
    event_length = fe_event_end - fe_event_start,
    temp_range = temperature_c_max - temperature_c_min,
    do_range = do_mlpl_max - do_mlpl_min,
    do_change = do_mlpl - do_mlpl_min,
    exclude = if_else(depth > 400 & temperature_c > 12, 1, 0, missing = 0)
  ) %>%
  select(-depth_m.y, -depth_m.x, -latitude, -longitude, -X, -Y) %>%
  filter(exclude != 1)


# filter missing location data
d_trawl <- d_trawl6 %>% dplyr::filter(!is.na(lat), !is.na(lon))

d_trawl$X <- d_trawl$lon
d_trawl$Y <- d_trawl$lat

d_trawl <- as_tibble(gfplot:::ll2utm(d_trawl, utm_zone = 9))

# d_trawl[d_trawl$fishing_event_id == 308835,]$year <- 2003
d_trawl <- readRDS("analysis/tmb-sensor-explore/data/all-sensor-data-processed.rds")


glimpse(d_trawl)
View(d_trawl)
# d_trawl <- filter(d_trawl, !is.na(temperature_c), !is.na(depth))


ggplot(d_trawl, aes(depth, temperature_c, colour = temperature_c, size = temperature_c_N / 100)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()

ggplot(d_trawl, aes(X, Y, colour = temperature_c)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()
hist(d_trawl$temperature_c)
hist(log(d_trawl$temperature_c))

ggplot(d_trawl, aes(X, Y, colour = do_mlpl)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()

ggplot(d_trawl, aes(depth, do_mlpl, colour = do_mlpl)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()


ggplot(d_trawl, aes(depth, salinity_psu, colour = salinity_psu, size = salinity_psu_N / 100)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()


ggplot(d_trawl, aes(X, Y, colour = salinity_psu)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()
hist(d_trawl$salinity_psu)

# note that N values pre2017 are not on same scale as those post2017
ggplot(d_trawl, aes(depth, salinity_psu, colour = temperature_c, size = sqrt(salinity_psu_N))) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()


ggplot(d_trawl, aes(X, Y, colour = depth)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()


saveRDS(d_trawl, "analysis/tmb-sensor-explore/data/all-sensor-data-processed.rds")
