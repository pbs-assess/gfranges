library(dplyr)
d <- gfdata::get_cpue_index("bottom trawl", min_cpue_year = 2008)
saveRDS(d, file = "analysis/VOCC/fishing-effort.rds") # 2008:2018
d <- readRDS("analysis/VOCC/fishing-effort.rds")
d$X <- d$longitude
d$Y <- d$latitude
d2 <- filter(d, !is.na(X), !is.na(Y))
d2 <- gfplot:::ll2utm(d2, utm_zone = 9)
d2$X <- d2$X/10
d2$Y <- d2$Y/10

grid <- readRDS("prediction-grids/overall-grid.rds")

grid$X_lower <- grid$X - 1
grid$Y_lower <- grid$Y - 1

# catch %>%
#   filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
#   filter(fe_start_date < fe_end_date) %>%
#   mutate(
#     effort =
#       as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))
#   ) %>%
#   filter(.data$effort > 0)
# max 6/9 hours?
