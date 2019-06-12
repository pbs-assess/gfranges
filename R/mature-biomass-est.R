#' Split catch densities by mass ratio of maturity classes
#'
#' @param survey_sets Data from get_survey_sets.
#' @param fish Data from get_survey_samples.
#' @param survey List of survey abbreviations.
#' @param years List of years. Default 'NULL' includes all years.  
#' @param cutoff_quantile Set max cutoff for mass modeled from lengths.
#' @param plot Logical for whether to produce plots 
#'    (length-weight and length-at-maturity relationships).
#'
#' @export
#'
#' @examples
#' events <- readRDS("analysis/VOCC/data/event-data-pop-1n3.rds")
#' fish <- readRDS("analysis/VOCC/data/bio-data-pop-1n3.rds")
#' 
#' biomass <- split_catch_maturity (events, fish, 
#'   survey = c("SYN HS","SYN QCS"), 
#'   years = NULL,
#'   cutoff_quantile = 0.9995,
#'   plot = TRUE
#' )
#' 
split_catch_maturity <- function(survey_sets, fish, bath,
  survey = c("SYN HS","SYN QCS"), 
  years = NULL,
  #year_re = TRUE,
  cutoff_quantile = 0.9995,
  plot = FALSE
  ){

if (is.null(years)) years <- unique(survey_sets[["year"]])  
species <- fish$species_common_name[1]
fish <- fish %>% filter(survey_abbrev %in% survey) %>% filter (year %in% years)
survey_sets <- survey_sets %>% filter(survey_abbrev %in% survey) %>% filter (year %in% years)
#browser()
# use internal version of tidy_survey_sets to include ssid and month columns
tidy_sets <- tidy_survey_sets(survey_sets, survey = survey, years = years) 
tidy_sets <- add_missing_depths(tidy_sets, survey = survey, years = years, bath = bath) 

# TMB model to estimate mass of length only fish
f_mass <- gfplot::fit_length_weight(fish, sex = "female", method == "tmb")
m_mass <- gfplot::fit_length_weight(fish, sex = "male", method == "tmb")

f_fish <- fish %>% filter(sex == 2) %>% mutate(year_f = as.character(year))
m_fish <- fish %>% filter(sex == 1) %>% mutate(year_f = as.character(year))


#FIXME: what to do when we have only some years without maturity data?
# set up check, and set year_re = FALSE in that case?

years_w_maturity <- fish %>% group_by(year) %>% 
  mutate(maturity_levels = length(unique(maturity_code))) 

levels_per_year <- unique(years_w_maturity$maturity_levels)

if (max(levels_per_year)<3) {
  return(list(data = tidy_sets, maturity = NA, mass_model = NA))
} 

if (min(levels_per_year)<3) {
  m <- fit_mat_ogive_re(fish, type = "length", sample_id_re = FALSE, year_re = FALSE)
  f_fish$threshold <- m$mat_perc$f.p0.5
  m_fish$threshold <- m$mat_perc$m.p0.5
  
  } else {
  m <- fit_mat_ogive_re(fish, type = "length", sample_id_re = FALSE, year_re = TRUE) 
  f_fish$threshold <- lapply(f_fish$year_f, function (x) m$mat_perc[[x]]$f.p0.5)
  m_fish$threshold <- lapply(m_fish$year_f, function (x) m$mat_perc[[x]]$m.p0.5)
  
  }

f_fish <- mutate(f_fish, 
  adult = if_else(length >= threshold, 1, 0, missing = NULL),
  model_mass = exp(f_mass$pars$log_a + f_mass$pars$b*(log(length)))*1000, 
  new_mass = weight)
max_model <- quantile(f_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
f_fish$model_mass[f_fish$model_mass > max_model] <- max_model
f_fish$new_mass[is.na(f_fish$weight)] <- f_fish$model_mass[is.na(f_fish$weight)]

m_fish <- mutate( m_fish, adult = if_else(length >= threshold, 1, 0, missing = NULL), 
  model_mass = exp(m_mass$pars$log_a + m_mass$pars$b*(log(length)))*1000, 
  new_mass = weight)
max_model_m <- quantile(m_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
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
  filter(adult==1) %>% 
  rename(mass_ratio_mature = mass_ratio, n_mature = count, sample_n = n) %>%
  select(fishing_event_id, est_sample_mass, mass_ratio_mature, n_mature, sample_n, measured_weight) %>% 
  unique()

sets_w_ratio <- left_join(tidy_sets, set_ratio, by="fishing_event_id")


# chose value to use when a sample mass ratio is not available
na_value <- mean(sets_w_ratio$mass_ratio_mature, na.rm = TRUE)
sets_w_ratio$mass_ratio_mature[is.na(sets_w_ratio$mass_ratio_mature)] <- na_value

# add column to check for discrepencies between total catch weight and biological sample weights 
sets_w_ratio$errors <- sets_w_ratio$est_sample_mass - sets_w_ratio$catch_weight*1000

data <- sets_w_ratio %>% 
  mutate(adult_density = density*mass_ratio_mature, imm_density = density*(1-mass_ratio_mature))

if (plot) {
  maturity_plot <- plot_mat_ogive(m) + 
    ggplot2::ggtitle(paste("Length at maturity for", species, ""))
  
  mass_plot <- ggplot(fish_maturity, aes(length, new_mass, colour= as.factor(sex))) + 
    geom_point(size = 1.5, alpha=0.35, shape = 1) + 
    geom_point(aes(length, weight), shape = 16, size = 1.25, alpha=0.65) + 
    scale_color_viridis_d(begin= 0.1, end=0.6) + 
    facet_wrap(~year) + theme_pbs() + 
    xlab("") + ylab("Weight (open circles are estimates)") + labs(colour = "Sex") +
    ggplot2::ggtitle(paste("Length-weight relationship for", species, ""))
  
  #gridExtra::grid.arrange(mass_plot, maturity_plot, nrow = 2)
  print(maturity_plot)
  print(mass_plot)
}

list(data = data, maturity = maturity_plot, mass_model = mass_plot)

}


