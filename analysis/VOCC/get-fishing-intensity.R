library(tidyverse)
library(ggplot2)
getwd()
setwd(here::here())

d <- gfdata::get_cpue_index("bottom trawl", min_cpue_year = 2004)
saveRDS(d, file = "analysis/VOCC/data/_fishing_effort/fishing-effort.rds") # 2008:2018
d <- readRDS("analysis/VOCC/data/_fishing_effort/fishing-effort.rds")

d <- readRDS("analysis/VOCC/data/_fishing_effort/HookLine_raw.rds")
## %>% filter(fishery_sector== "ROCKFISH INSIDE")
unique(d$fishery_sector)

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
  group_by(year, fishing_event_id) %>% 
  summarise(effort = mean(effort, na.rm = T), 
    catch = sum(catch_kg, na.rm = T),
    latitude = mean(latitude, na.rm = T),
    longitude = mean(longitude, na.rm = T)
    ) 

# d1 <- filter(d, effort <= 8 & year > 2007 & year < 2020) # 2019 effort caps here
d1 <- filter(d, effort <= 60 & year > 2007 & year < 2020) # max 6 hours

ggplot(d1, aes(effort, log(catch+1))) + 
  geom_point(alpha=0.2) + 
  ylim(0, 10) +
  facet_wrap(~year)

median(d1$effort)
mean(d1$effort)
 
# d2 <- filter(d, effort > 8 & year > 2007 & year < 2020) # max 6 hours
d2 <- filter(d, effort > 60 & effort < 200 & year > 2007 & year < 2020) # max 6 hours

ggplot(d2, aes(effort, log(catch+1))) + 
  geom_point(alpha=0.2) + 
  ylim(0, 10) +
  facet_wrap(~year)


# d3 <- filter(d,  effort > 200 &year >2007 & year <2020) 
# log(median(d3$catch)+1)


# d <- d %>% mutate(effort2 = if_else(effort > 6, 1, effort),
  # revised = if_else(effort > 6, "revised", "true"))

# # Feb 2021: use median effort and change cutoff to 8 hrs
# d <- d %>% mutate(effort8 = ifelse(effort > 8, median(d1$effort), effort))
d <- d %>% mutate(effort60 = ifelse(effort > 60, median(d1$effort), effort))

d <- filter(d, year >2007 & year <2020) 
d <- filter(d, latitude < 57) 
d <- filter(d, longitude < -122) 
d <- filter(d, latitude > 45) 
d <- filter(d, longitude > -140) 

# ggplot(d, aes((effort2), log(catch), colour = revised)) + 
#   geom_point(alpha=0.1) + facet_wrap(~year) + gfplot::theme_pbs()
ggplot(d, aes(longitude, latitude)) + 
  geom_point(alpha=0.2) + facet_wrap(~year)

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
range(grid$X)
range(grid$Y)
d <- filter(d, X < 808)
d <- filter(d, X > 166)
d <- filter(d, Y < 6060)
d <- filter(d, Y > 5346)



plot(d$X,d$Y)

d$X <- 2 * round(d$X/2)
d$Y <- 2 * round(d$Y/2)

d <- st_set_geometry(d, NULL) 


dat <- inner_join(d, grid) 
# dat <- filter(dat, effort <= 9) # or max 6 hours?
# dat <- dat %>% mutate(effort1 = if_else(effort > 9, 0, effort))

# quantile(dat$effort1, .975)

# hist(dat$effort1)

data <- dat %>% group_by(X, Y) %>% 
  mutate(
    effort = sum(effort), 
    # effort8 = sum(effort8), # for trwl
    effort60 = sum(effort60), # for ll
    # effort = sum(effort1), effort2 = sum(effort2), 
    catch = sum(catch)) %>% 
  select(-fishing_event_id) %>% 
  distinct() %>% 
  filter(year < 2019) %>% 
  mutate(
    # log_effort = log(effort8) # for trwl
    log_effort = log(effort60) # for ll
    )

# saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/ll-fishing-effort-grid.rds")
# saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/trwl-fishing-effort-grid.rds")

# saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/fishing-effort-grid.rds")
# saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/fishing-effort-grid-6hr.rds")

trwl <- readRDS(file = "analysis/VOCC/data/_fishing_effort/trwl-fishing-effort-grid.rds") %>%
   select(year, X, Y, trwl_catch = catch, trwl_effort = effort8)

ll <- readRDS(file = "analysis/VOCC/data/_fishing_effort/ll-fishing-effort-grid.rds") %>% 
  select(year, X, Y, ll_catch = catch, ll_effort = effort60)


data <- left_join(trwl, ll) %>% mutate(
  trwl_catch = replace_na(trwl_catch, 0),
  ll_catch = replace_na(ll_catch, 0),
  tot_catch = trwl_catch + ll_catch
  )

saveRDS(data, file = "analysis/VOCC/data/_fishing_effort/fishing-effort-grid-all.rds")

ggplot(data, aes(X,Y, colour=log(trwl_catch+1))) +
  geom_point(shape=20, alpha=0.2) +
  scale_colour_viridis_c() +
  facet_wrap(~year)

ggplot(data, aes(X,Y, colour=log(ll_catch+1))) +
  geom_point(shape=20, alpha=0.2) +
  scale_colour_viridis_c() +
  facet_wrap(~year)

ggplot(data, aes(log(trwl_catch+1),log(ll_catch+1) )) +
  geom_point(shape=20, alpha=0.2) +
  facet_wrap(~year)


fishing_yr <- data %>% group_by(year) %>% summarise(
  tot_trwl_effort = sum(trwl_effort, na.rm = T), tot_trwl_catch = sum(trwl_catch, na.rm = T),
  tot_ll_effort = sum(ll_effort, na.rm = T), tot_ll_catch = sum(ll_catch, na.rm = T),
  tot_catch = sum(tot_catch, na.rm = T)
  ) %>% filter(year>2006)

ggplot(data = fishing_yr) + 
  # geom_point(aes(year,(tot_trwl_catch)))+
  # ylim(0,max((fishing_yr$tot_trwl_catch))) +
  geom_point(aes(year,(tot_ll_catch)))+ 
  ylim(0,max((fishing_yr$tot_ll_catch))) +
  theme_bw()

(max(fishing_yr$tot_trwl_catch)-min(fishing_yr$tot_trwl_catch))/max(fishing_yr$tot_trwl_catch)
(max(fishing_yr$tot_ll_catch)-min(fishing_yr$tot_ll_catch))/max(fishing_yr$tot_ll_catch)

ggplot(data = fishing_yr) + 
  geom_point(aes(year,(tot_catch)))+ 
  ylim(0,max(fishing_yr$tot_catch)) +
  theme_bw()


(max(fishing_yr$tot_catch)-min(fishing_yr$tot_catch))/max(fishing_yr$tot_catch)
sum(fishing_yr$tot_ll_catch)/sum(fishing_yr$tot_catch)


ggplot(data = fishing_yr) + 
  # geom_point(aes(year,log(tot_trwl_catch)))+
  geom_point(aes(year,log(tot_ll_catch)))+ 
  # ylim(0,max(log(fishing_yr$tot_trwl_catch))) + 
  theme_bw()

ggplot(data = fishing_yr) + geom_point(aes(year,tot_ll_catch)) + ylim(0,max(fishing_yr$tot_ll_catch
))+ theme_bw()

ggplot(data = fishing_yr) + geom_point(aes(year,tot_ll_effort)) + ylim(0,max(fishing_yr$tot_ll_effort
  ))+ theme_bw()

ggplot(data, aes(year,log(ll_catch+1) )) +
  geom_point(shape=20, alpha=0.2) 

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
