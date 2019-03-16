# gfplot::get_sensor_attributes()

d_trawl <- gfplot::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
saveRDS(d_trawl, file = "analysis/tmb-sensor-explore/dat-sensor-trawl.rds")
d_trawl <- readRDS("analysis/tmb-sensor-explore/dat-sensor-trawl.rds")

dovsol <- gfplot::get_survey_sets(join_sample_ids = TRUE, species = "dover sole", ssid = c(1, 3, 4, 16))
saveRDS(dovsol, file = "analysis/tmb-sensor-explore/dover-sole.rds")


# d_ll <- gfplot::get_sensor_data_ll_ctd(c(22, 36), sensor_min_max = TRUE)
# saveRDS(d_ll, file = "analysis/tmb-sensor-explore/dat-sensor-ll.rds")
# d_ll <- readRDS("analysis/tmb-sensor-explore/dat-sensor-ll.rds")

#surv <- readRDS("../gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets

library(tidyverse)

d_trawl <- d_trawl %>%
  select(-count, -start_time, -end_time, -min, -max) %>%
  mutate(attribute = tolower(attribute)) %>%
  distinct() %>%
  reshape2::dcast(fishing_event_id + year + ssid + survey_desc ~ attribute, value.var = "avg")

surv_fish <- surv %>% select(year, ssid, fishing_event_id, survey_series_id, longitude, latitude, density_kgpm2) %>%
  distinct() %>%
  rename(ssid = survey_series_id)

d_trawl <- left_join(surv_fish, d_trawl) #%>%
  filter(ssid == 1)

d_trawl2 <- rename(d_trawl, X = longitude, Y = latitude)
d_trawl <- as_tibble(gfplot:::ll2utm(d_trawl2, utm_zone = 9))


ggplot(d_trawl, aes(X, Y, colour = temperature_c)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()
hist(d_trawl$temperature_c)
hist(log(d_trawl$temperature_c))

ggplot(d_trawl, aes(X, Y, colour = depth_m)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()

ggplot(d_trawl, aes(X, Y, colour = do_mlpl)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()
hist(d_trawl$do_mlpl)
hist(log(d_trawl$do_mlpl))

ggplot(d_trawl, aes(X, Y, colour = salinity_psu)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()
hist(d_trawl$salinity_psu)

d_trawl <- filter(d_trawl, !is.na(temperature_c), !is.na(depth_m))

d_trawl <- mutate(d_trawl,
  depth_scaled = arm::rescale(log(depth_m)),
  depth_scaled2 = depth_scaled^2
)

ggplot(d_trawl, aes(X, Y, colour = depth_scaled)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()

ggplot(d_trawl, aes(X, Y, colour = depth_scaled2)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_c()

saveRDS(d_trawl, "analysis/tmb-sensor-explore/sensor-data-processed.rds")

