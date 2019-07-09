# get data for biomass partitioning

species <- list(
# Flatfish  
  "Arrowtooth Flounder",
  "English Sole",
  "Dover Sole",
  "Southern Rock Sole",
  "Petrale Sole",

# The dominant species from trawl surveys off the west coast of Vancouver Island were
  "North Pacific Spiny Dogfish", # (Squalus suckleyi), 
  "Sharpchin Rockfish", # (Sebastes zacentrus), 
  "Sablefish", # (Anoplopoma fimbria), 
  "Splitnose Rockfish", # (Sebastes diploproa) and 
  "Canary Rockfish", # (Sebastes pinniger) 

# Off the west coast of Haida Gwaii, the dominant species were 
  "Pacific Ocean Perch", # (Sebastes alutus), 
  #"Sharpchin Rockfish", 
  "Rougheye/Blackspotted Rockfish Complex", # (Sebastes aleutianus/melanostictus), 
  "Silvergray Rockfish", # (Sebastes brevispinis), and 
  "Shortspine Thornyhead", # (Sebastolobus alascanus) 
  
# Notable trends in abundance 
  
  # increases in the abundance indices for 
  "Bocaccio Rockfish", # (Sebastes paucispinis), 
  #"Sablefish", 
  #"Petrale Sole", # (Eopsetta jordani), 
  "Flathead Sole", # (Hippoglossoides elassodon), and 
  "Longspine Thornyhead", # (Sebastolobus altivelis) 

  # decreases in abundance indices in some areas for 
  #"Arrowtooth Flounder", # (Atheresthes stomias), 
  "Pacific Cod", # (Gadus macrocephalus), 
  #"Silvergray Rockfish", and 
  "Lingcod", # (Ophiodon elongatus) 

  # return to the west coast of Vancouver Island after an absence of about four years of 
  # "North Pacific Spiny Dogfish" 
  
# Other high catch species
  "Spotted Ratfish",
  "Greenstriped Rockfish",
  "Pacific Cod",
  "Longnose Skate",
  "Walleye Pollock",
  "Bocaccio",
  "Redbanded Rockfish",
  "Quillback Rockfish",
  
# Other species of interest  
  "Pacific Ocean Perch"
)

species <- "Yelloweye Rockfish"

for (i in species) {
species <- tolower(i)
events <- gfdata::get_survey_sets(species, ssid = c(1, 3, 4, 16))
fish <- gfdata::get_survey_samples(species, ssid = c(1, 3, 4, 16))
spp <- gsub(" ", "-", gsub("\\/", "-", species))
saveRDS(fish, file = paste0("analysis/VOCC/raw/bio-data-", spp, "")) 
saveRDS(events, file = paste0("analysis/VOCC/raw/event-data-", spp, "")) 
}


# create akima_depth once

anyspecies <- "pacific cod"
survey_sets <- gfdata::get_survey_sets(anyspecies, ssid = c(1, 3, 4, 16))
years <- unique(survey_sets[["year"]]) 
survey <- c("SYN HS","SYN QCS","SYN WCVI","SYN WCHG")
tidy_sets <- tidy_survey_sets(survey_sets, survey = survey, years = years) 

bath <- tidy_sets %>% gfplot:::interp_survey_bathymetry()

saveRDS(bath, file = "analysis/VOCC/data/bathymetry-data")


# get older sensor data
library(tidyverse)

d <- gfdata::get_table("FE_SALINITY_DO")
names(d) <- tolower(names(d))
head(d)

.d <- d %>% group_by(fishing_event_id) %>% 
  mutate(
    do_mlpl = mean(do, na.rm = TRUE), 
    do_mlpl_min = min(do, na.rm = TRUE), 
    do_mlpl_max = max(do, na.rm = TRUE), 
    salinity_psu = mean(salinity, na.rm = TRUE), 
    salinity_psu_min = min(salinity, na.rm = TRUE), 
    salinity_psu_max = max(salinity, na.rm = TRUE),
    do_event_start = min(fe_event_time),
    do_event_end = max(fe_event_time)
    ) %>% 
  add_tally(do, name= "do_mlpl_N") %>% 
  add_tally(salinity, name = "salinity_psu_N") %>%
  select(-do, -salinity, -fe_event_time)
.d <- unique(.d)
#saveRDS(.d, file = "analysis/VOCC/data/")


# merge with newer sensor data
d_trawl <- readRDS("analysis/tmb-sensor-explore/data/dat-sensor-trawl.rds")
library(tidyverse)

.d_trawl <- d_trawl %>% group_by(fishing_event_id) %>% mutate(fe_event_start = min(start_time),
fe_event_end = max(end_time)) %>% ungroup() %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id + year + ssid + survey_desc + fe_event_start + fe_event_end ~ attribute, mean, value.var = "avg")

.d_trawl_max <- d_trawl %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, mean, value.var = "max")

.d_trawl_min <- d_trawl %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, mean, value.var = "min")

.d_trawl_N <- d_trawl %>%
  mutate(attribute = tolower(attribute)) %>%
  reshape2::dcast(fishing_event_id ~ attribute, sum, value.var = "count")

.d_trawl <- full_join(.d_trawl, .d_trawl_max, by="fishing_event_id", suffix = c("", "_max"))
.d_trawl <- full_join(.d_trawl, .d_trawl_min, by="fishing_event_id", suffix = c("", "_min"))
.d_trawl <- full_join(.d_trawl, .d_trawl_N, by="fishing_event_id", suffix = c("", "_N"))

glimpse(d_trawl)
glimpse(.d_trawl)

.d_trawl <- full_join(.d_trawl, .d) %>%
  mutate(do_mlpl = coalesce(do_mlpl.x, do_mlpl.y),
    do_mlpl_max = coalesce(do_mlpl_max.x, do_mlpl_max.y),
    do_mlpl_min = coalesce(do_mlpl_min.x, do_mlpl_min.y),
    do_mlpl_N = coalesce(do_mlpl_N.x, do_mlpl_N.y),
    salinity_psu = coalesce(salinity_psu.x, salinity_psu.y),
    salinity_psu_max = coalesce(salinity_psu_max.x, salinity_psu_max.y),
    salinity_psu_min = coalesce(salinity_psu_min.x, salinity_psu_min.y),
    salinity_psu_N = coalesce(salinity_psu_N.x, salinity_psu_N.y)) %>%
  select()



# disagreement between catch_weight and bio sample for single fishing event with 17 large POP. 
# events[events$fishing_event_id==1506954,]$catch_weight
# fish[fish$fishing_event_id==1506954,]$length
# tidy_sets[tidy_sets$fishing_event_id==1506954,]$catch_weight

