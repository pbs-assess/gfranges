#' Split catch weight by mass ratio of maturity classes
#'
#' @param survey_sets 
#' @param fish 
#' @param survey 
#' @param years 
#' @param cutoff_quantile 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
#' events <- readRDS("analysis/VOCC/data/event-data-pop-1n3.rds")
#' fish <- readRDS("analysis/VOCC/data/bio-data-pop-1n3.rds")
#' 
#' biomass <- split_catch_maturity (events, fish, 
#'   survey = c("SYN HS","SYN QCS"), 
#'   years = NULL,
#'   cutoff_quantile = c(.9995),
#'   plot = TRUE
#' )
#' 
split_catch_maturity <- function(survey_sets, fish, 
  survey = c("SYN HS","SYN QCS"), 
  years = NULL,
  cutoff_quantile = c(.9995),
  plot = FALSE
  ){

if (is.null(years)) years <- unique(survey_sets[["year"]])  
species <- fish$species_common_name[1]

tidy_sets <- gfplot::tidy_survey_sets(survey_sets, survey = survey, years = years)
m <- fit_mat_ogive(fish, type = "length", sample_id_re = FALSE, year_re = TRUE)
f_mass <- gfplot::fit_length_weight(fish, sex = "female", method == "tmb")
m_mass <- gfplot::fit_length_weight(fish, sex = "male", method == "tmb")

f_fish <- fish %>% filter(sex == 2) %>% mutate(year_f = as.character(year))
f_fish$threshold <- lapply(f_fish$year_f, function (x) m$mat_perc[[x]]$f.p0.5)
f_fish <- f_fish %>% mutate(adult = if_else(length >= threshold, 1, 0, missing = NULL)) 

# TMB model to estimate mass of length only fish, but slow so avoiding rerunning...
f_mass <- gfplot::fit_length_weight(fish, sex = "female", method == "tmb")
f_fish <- f_fish %>% mutate(model_mass = exp(f_mass$pars$log_a + f_mass$pars$b*(log(length)))*1000, new_mass = weight)
max_model <- quantile(f_fish$weight, probs = cutoff_quantile, na.rm = TRUE)
f_fish$model_mass[f_fish$model_mass > max_model] <- max_model
f_fish$new_mass[is.na(f_fish$weight)] <- f_fish$model_mass[is.na(f_fish$weight)]

m_fish <- fish %>% filter(sex == 1) %>% mutate(year_f = as.character(year))
m_fish$threshold <- lapply(m_fish$year_f, function (x) m$mat_perc[[x]]$m.p0.5)
m_fish <- m_fish %>% mutate(adult = if_else(length >= threshold, 1, 0, missing = NULL)) 

# TMB model to estimate mass of length only fish, but slow so avoiding rerunning...
m_fish <- m_fish %>% mutate(model_mass = exp(m_mass$pars$log_a + m_mass$pars$b*(log(length)))*1000, new_mass = weight)
max_model_m <- quantile(m_fish$weight, probs = cutoff_quantile, na.rm = TRUE)
m_fish$model_mass[m_fish$model_mass > max_model_m] <- max_model_m
m_fish$new_mass[is.na(m_fish$weight)] <- m_fish$model_mass[is.na(m_fish$weight)]

fish_maturity <- rbind(f_fish, m_fish) %>% mutate(sex = ifelse(sex == 2, "F", "M"))

set_maturity <- fish_maturity %>%
  group_by(fishing_event_id, adult) %>% 
  mutate(maturity_mass = sum(new_mass)) %>% 
  add_tally() %>% rename(count = n) %>% 
  ungroup()

set_ratio <- set_maturity %>% 
  group_by(fishing_event_id) %>% add_tally() %>%
  mutate(est_sample_mass = sum(new_mass, na.rm = TRUE), 
         mass_ratio = maturity_mass/est_sample_mass, 
         measured_weight = sum(weight)) %>%
  filter(adult==1) %>% rename(mass_ratio_mature = mass_ratio, mature = count) %>%
  select(fishing_event_id, est_sample_mass, mass_ratio_mature, mature, n, measured_weight) %>% unique()

sets_w_ratio <- left_join(tidy_sets, set_ratio)


# chose value to use when a sample mass ratio is not available
na_value <- mean(sets_w_ratio$mass_ratio_mature, na.rm = TRUE)
sets_w_ratio$mass_ratio_mature[is.na(sets_w_ratio$mass_ratio_mature)] <- na_value

# add column to check for discrepencies between total catch weight and biological sample weights 
sets_w_ratio$errors <- sets_w_ratio$est_sample_mass - sets_w_ratio$catch_weight*1000

data <- sets_w_ratio %>% 
  mutate(adult_density = density*mass_ratio_mature, imm_density = density*(1-mass_ratio_mature))

if (plot) {
  maturity_plot <- plot_mat_ogive(m)
  
  mass_plot <- ggplot(fish_maturity, aes(length, new_mass, colour= as.factor(sex))) + 
    geom_point(size = 1.5, alpha=0.35, shape = 1) + 
    geom_point(aes(length, weight), shape = 16, size = 1.25, alpha=0.65) + 
    scale_color_viridis_d(begin= 0.1, end=0.6) + 
    facet_wrap(~year) + theme_pbs() + 
    xlab("") + ylab("Weight (open circles are estimates)") + labs(colour = "Sex") +
    ggplot2::ggtitle(paste("Length-weight relationship for", species, ""))
  
  gridExtra::grid.arrange(mass_plot, maturity_plot, nrow = 2)
}

data
}


