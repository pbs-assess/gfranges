library(dplyr)
library(ggplot2)
getwd()
setwd(here::here())

d <- gfdata::get_cpue_index("bottom trawl", min_cpue_year = 2004)
saveRDS(d, file = "analysis/VOCC/data/_fishing_effort/fishing-effort.rds") # 2008:2018
d <- readRDS("analysis/VOCC/data/_fishing_effort/fishing-effort.rds")

d$year <- lubridate::year(d$best_date)
d <- dplyr::select(d, year, fishing_event_id, longitude, latitude, fe_end_date, fe_start_date, catch_kg) 
d <- d %>%
  filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
  filter(fe_start_date < fe_end_date) %>%
  mutate(
    effort =
      as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))
  ) %>% dplyr::select(-fe_end_date, -fe_start_date) 
  # filter(effort > 0)
d <- d %>% 
  group_by(year, fishing_event_id, longitude, latitude) %>% 
  summarise(effort = mean(effort), 
    catch = sum(catch_kg)) 



d2 <- filter(d, effort <= 6 & year >2007) # max 6 hours

ggplot(d2, aes(effort, log(catch))) + 
  geom_point(alpha=0.2) + facet_wrap(~year)

d <- d %>% mutate(effort2 = if_else(effort > 6, 1, effort),
  revised = if_else(effort > 6, "revised", "true"))
d <- filter(d, longitude < -120) 
d <- filter(d, latitude > 45) 
d <- filter(d, longitude > -150) 

ggplot(d, aes((effort2), log(catch), colour = revised)) + 
  geom_point(alpha=0.1) + facet_wrap(~year) + gfplot::theme_pbs()


library(sf)
proj.to <- "+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

d <- dplyr::distinct(d) %>% ungroup()

dsf <- st_as_sf(d,
  coords = c("longitude", "latitude"),
  crs = 4326,
  na.fail = FALSE)%>%
  #st_transform(crs = 32609) # number found after using full proj.to
  st_transform(crs = proj.to)

xy <- sf::st_coordinates(dsf)
d <- cbind(dsf, xy)
head(d)
#d <- gfplot:::ll2utm(d, utm_zone = 9)
plot(d$X,d$Y)

d$X <- d$X/1000
d$Y <- d$Y/1000

#d <- filter(d, !is.na(X), !is.na(Y))

grid <- readRDS("prediction-grids/overall-grid.rds")

# grid$X_lower <- grid$X - 1
# grid$Y_lower <- grid$Y - 1

plot(grid$X,grid$Y)

d <- filter(d, X < 820) 
d <- filter(d, X > 180) 
d <- filter(d, Y < 6100) 
d <- filter(d, Y > 5300) 

#### TODO: maybe should redo? to exactly match grid?
# range(grid$X)
# range(grid$Y)
# d <- filter(d, X < 808) 
# d <- filter(d, X > 166) 
# d <- filter(d, Y < 6060) 
# d <- filter(d, Y > 5346) 



plot(d$X,d$Y)

d$X <- 2 * round(d$X/2)
d$Y <- 2 * round(d$Y/2)

d <- st_set_geometry(d, NULL) 


dat <- inner_join(d, grid) 
# dat <- filter(dat, effort <= 9) # or max 6 hours?
dat <- dat %>% mutate(effort1 = if_else(effort > 9, 0, effort))

quantile(dat$effort1, .975)

hist(dat$effort1)


data <- dat %>% group_by(X, Y) %>% mutate(effort = sum(effort1), effort2 = sum(effort2), catch = sum(catch)) %>% select(-fishing_event_id) %>% distinct() %>% filter(year < 2019) %>% mutate(log_effort = log(effort1))

saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/fishing-effort-grid.rds")
# saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/fishing-effort-grid-6hr.rds")

hist(data$log_effort)

# ggplot(data, aes(X,Y, colour=log_effort, size=(effort))) + 
#   geom_point(shape=20, alpha=0.2) + 
#   scale_colour_viridis_c() + 
#   facet_wrap(~year) 
# ggplot(data, aes(effort)) + geom_histogram() + 
#   #scale_x_continuous(trans = 'log')+ 
#   facet_wrap(~year, scales = "free") 
# yr <- data %>% group_by(year) %>% 
#   mutate(tot_effort = sum(effort)) %>% 
#   select (year, tot_effort) %>% distinct()
# yr 
