library(dplyr)
d <- gfdata::get_cpue_index("bottom trawl", min_cpue_year = 2008)
saveRDS(d, file = "analysis/VOCC/fishing-effort.rds") # 2008:2018
d <- readRDS("analysis/VOCC/fishing-effort.rds")
d <- dplyr::select(d, fishing_event_id, longitude, latitude, fe_end_date, fe_start_date)
d <- d %>%
  filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
  filter(fe_start_date < fe_end_date) %>%
  mutate(
    effort =
      as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))
  ) %>%
  filter(effort > 0)
d <- dplyr::distinct(d)
d <- dplyr::select(d, -fe_end_date, -fe_start_date)
d <- filter(d, effort <= 6) # max 6 hours
d <- filter(d, longitude < -120) 
d <- filter(d, latitude > 45) 
d <- filter(d, longitude > -150) 

d$X <- d$longitude
d$Y <- d$latitude
d2 <- filter(d, !is.na(X), !is.na(Y))
d <- gfplot:::ll2utm(d, utm_zone = 9)
d$X <- d$X
d$Y <- d$Y

grid <- readRDS("prediction-grids/overall-grid.rds")

grid$X_lower <- grid$X - 1
grid$Y_lower <- grid$Y - 1

# max 6/9 hours?
