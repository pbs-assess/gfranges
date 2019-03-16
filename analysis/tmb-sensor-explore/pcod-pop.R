library(sdmTMB)
library(tidyverse)

dp <- readRDS("~/src/gfsynopsis/report/data-cache/pacific-ocean-perch.rds")$survey_sets
dc <- readRDS("~/src/gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets

dp <- dp %>% filter(survey_series_id == 1)
dc <- dc %>% filter(survey_series_id == 1)

dp <- rename(dp, X = longitude, Y = latitude)
dp <- as_tibble(gfplot:::ll2utm(dp, utm_zone = 9))

dc <- rename(dc, X = longitude, Y = latitude)
dc <- as_tibble(gfplot:::ll2utm(dc, utm_zone = 9))

dp <- rename(dp, density_kgpm2_pop = density_kgpm2) %>%
  select(X, Y, year, density_kgpm2_pop, fishing_event_id)

dc <- rename(dc, density_kgpm2_pcod = density_kgpm2) %>%
  select(X, Y, year, density_kgpm2_pcod, fishing_event_id)

d <- inner_join(dp, dc)
d <- filter(d, !is.na(density_kgpm2_pcod), !is.na(density_kgpm2_pop))

d$density_kgpm2_pcod <- d$density_kgpm2_pcod * 1e6
d$density_kgpm2_pop <- d$density_kgpm2_pop * 1e6

spde <- make_spde(d$X, d$Y, n_knots = 50) # only 100 knots for example speed
plot_spde(spde)

plot(log(d$density_kgpm2_pop+1), log(d$density_kgpm2_pcod+1))

d <- mutate(d, pop_present = as.numeric(density_kgpm2_pop > 0))
d <- mutate(d, pcod_present = as.numeric(density_kgpm2_pcod > 0))

m <- sdmTMB(
  data = d,
  formula = density_kgpm2_pop ~ 0 + as.factor(year),
  time_varying = ~ 0 + density_kgpm2_pcod,
  time = "year", spde = spde, family = tweedie(link = "log"),
  silent = FALSE)

m$model$par

r <- m$tmb_obj$report()
r$b_rw_t
plot(r$b_rw_t)

sr <- TMB::sdreport(m$tmb_obj, bias.correct = FALSE)
library(TMB)
ssr <- summary(sr)

plot(plogis(m$model$par[1:9] + r$b_rw_t))
