library(dplyr)
library(ggplot2)
library(gfplot)
library(gfdata)

# events <- gfdata::get_survey_sets("pacific ocean perch", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("pacific ocean perch", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-pop-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-pop-1n3.rds")

events <- readRDS("analysis/VOCC/data/event-data-pop-1n3.rds")
fish <- readRDS("analysis/VOCC/data/bio-data-pop-1n3.rds")

m <- gfplot::fit_mat_ogive(fish, type = "length", sample_id_re = FALSE)
gfplot::plot_mat_ogive(m)

m$mat_perc$f.p0.5
m$mat_perc$m.p0.5



ggplot(fish, aes(length, weight, colour= as.factor(sex))) + geom_point(size = 3, alpha=0.35) + facet_wrap(~year) + scale_color_viridis_d()

# disagreement between catch_weight and bio sample for single fishing event with 17 large POP. 
events[events$fishing_event_id==1506954,]$catch_weight
fish[fish$fishing_event_id==1506954,]$length



f_fish <- fish %>% 
  filter(sex == 2) %>% 
  mutate(adult = if_else(length >= m$mat_perc$f.p0.5, 1, 0, missing = NULL)) 
# TMB model to estimate mass of length only fish, but slow so avoiding rerunning...
f_mass <- fit_length_weight(f_fish, sex = "all", method == "tmb")
f_fish <- f_fish %>% mutate(model_mass = exp(f_mass$pars$log_a + f_mass$pars$b*(log(length)))*1000, new_mass = weight)
# Alternate linear model
# f_mass <- lm(log(weight)~log(length), data = f_fish)
# f_fish <- f_fish %>% mutate(model_mass = exp(f_mass$coefficients[1] + f_mass$coefficients[2]*log(length)), new_mass = weight)
# plot(model_mass~length, col= year, data = f_fish)

# max_model <- max(f_fish$weight, na.rm = TRUE)
max_model <- quantile(f_fish$weight, c(.9995), na.rm = TRUE)
f_fish$model_mass[f_fish$model_mass > max_model] <- max_model

f_fish$new_mass[is.na(f_fish$weight)] <- f_fish$model_mass[is.na(f_fish$weight)]

plot(new_mass~length, col=year, data = f_fish)



m_fish <- fish %>% 
  filter(sex == 1) %>% 
  mutate(adult = if_else(length >= m$mat_perc$m.p0.5, 1, 0, missing = NULL)) 

# TMB model to estimate mass of length only fish, but slow so avoiding rerunning...
m_mass <- fit_length_weight(f_fish, sex = "all", method == "tmb")
m_fish <- m_fish %>% mutate(model_mass = exp(m_mass$pars$log_a + m_mass$pars$b*(log(length)))*1000, new_mass = weight)
# Alternate linear model
# m_mass <- lm(log(weight)~log(length), data = f_fish)
# m_fish <- m_fish %>% mutate(model_mass = exp(m_mass$coefficients[1] + m_mass$coefficients[2]*log(length)), new_mass = weight)
# plot(model_mass~length, col= year, data = m_fish)
# max_model_m <- max(m_fish$weight, na.rm = TRUE)
max_model_m <- quantile(m_fish$weight, c(.9995), na.rm = TRUE)
m_fish$model_mass[m_fish$model_mass > max_model_m] <- max_model_m
m_fish$new_mass[is.na(m_fish$weight)] <- m_fish$model_mass[is.na(m_fish$weight)]
plot(new_mass~length, col= year, data = m_fish)

fish_maturity <- rbind(f_fish, m_fish)
View(fish_maturity)

ggplot(fish_maturity, aes(length, new_mass, colour= as.factor(sex))) + geom_point(size = 3, alpha=0.35) + facet_wrap(~year) + scale_color_viridis_d()


event_maturity <- fish_maturity %>%
  group_by(fishing_event_id, adult) %>% mutate(maturity_mass = sum(new_mass)) %>% add_tally() %>% rename(adult_count = n) %>% ungroup()

View(event_maturity)


event_ratio <- event_maturity %>% group_by(fishing_event_id) %>% add_tally() %>%
  mutate(total_bio_mass = sum(new_mass, na.rm = TRUE), adult_mass_ratio = maturity_mass/total_bio_mass, total_measured_weight = sum(weight)/1000) %>%
  filter(adult==1) %>% 
  select(fishing_event_id, total_bio_mass, adult_mass_ratio, adult_count, n, total_measured_weight) %>% unique()

#event_ratio$adult_mass_ratio[is.na(event_ratio$adult_mass_ratio)] <- 0

View(events)
tidy_events <- tidy_survey_sets(events, survey = c("SYN HS","SYN QCS"), years = c(2004,2005,2007,2009,2011,2013,2015,2017))
View(tidy_events)
tidy_events[tidy_events$fishing_event_id==1506954,]$catch_weight
events_w_ratio <- left_join(tidy_events, event_ratio)



events_w_ratio$errors <- events_w_ratio$total_bio_mass - events_w_ratio$catch_weight*1000

View(events_w_ratio)

# d <- events_w_ratio[events_w_ratio$depth_m<150,]
# # d <- events_w_ratio[events_w_ratio$density_kgpm2>mean(events_w_ratio$density_kgpm2),]
# ggplot(d,aes(x=adult_mass_ratio, colour=density_kgpm2))+geom_histogram() #+facet_grid(~year)+theme_bw()

# chose value to use when a sample mass ratio is not available
na_value <- mean(events_w_ratio$adult_mass_ratio, na.rm = TRUE)

events_w_ratio$adult_mass_ratio[is.na(events_w_ratio$adult_mass_ratio)] <- na_value


biomass <- events_w_ratio %>% mutate(adult_density = density*adult_mass_ratio, imm_density = density*(1-adult_mass_ratio))
View(biomass)

# saveRDS(biomass, file = "analysis/VOCC/data/maturity-biomass-data-pop-1n3.rds")

