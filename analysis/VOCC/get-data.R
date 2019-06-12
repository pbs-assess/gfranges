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



# events <- gfdata::get_survey_sets("sablefish", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("sablefish", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-sablefish-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-sablefish--1n3.rds")
# 
# events <- gfdata::get_survey_sets("REDBANDED ROCKFISH", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("REDBANDED ROCKFISH", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-REDBANDED-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-REDBANDED-1n3.rds")
# 
# 
# events <- gfdata::get_survey_sets("REDBANDED ROCKFISH", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("REDBANDED ROCKFISH", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-REDBANDED-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-REDBANDED-1n3.rds")
# 
# 
# events <- gfdata::get_survey_sets("SHORTSPINE THORNYHEAD", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("SHORTSPINE THORNYHEAD", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-SHORTSPINE-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-SHORTSPINE-1n3.rds")
# 
# events <- gfdata::get_survey_sets("BIG SKATE", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("BIG SKATE", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-BIGSKATE-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-BIGSKATE-1n3.rds")
# 
# events <- gfdata::get_survey_sets("lingcod", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("lingcod", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-lingcod-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-lingcod-1n3.rds")

# events <- gfdata::get_survey_sets("pacific ocean perch", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("pacific ocean perch", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-pop-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-pop-1n3.rds")

# events <- gfdata::get_survey_sets("pacific cod", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("pacific cod", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-pcod-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-pcod-1n3.rds")

# events <- gfdata::get_survey_sets("arrowtooth flounder", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("arrowtooth flounder", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-arrowtooth-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-arrowtooth-1n3.rds")

# events <- gfdata::get_survey_sets("ENGLISH SOLE", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("ENGLISH SOLE", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-ENGLISH-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-ENGLISH--1n3.rds")

# events <- gfdata::get_survey_sets("NORTH PACIFIC SPINY DOGFISH", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("NORTH PACIFIC SPINY DOGFISH", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-dogfish-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-dogfish--1n3.rds")

# events <- gfdata::get_survey_sets("SILVERGRAY ROCKFISH", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("SILVERGRAY ROCKFISH", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-silvergray-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-silvergray--1n3.rds")

# events <- gfdata::get_survey_sets("YELLOWTAIL ROCKFISH", ssid = c(1, 3))
# fish <- gfdata::get_survey_samples("YELLOWTAIL ROCKFISH", ssid = c(1, 3))
# saveRDS(fish, file = "analysis/VOCC/data/bio-data-YELLOWTAIL-1n3.rds")
# saveRDS(events, file = "analysis/VOCC/data/event-data-YELLOWTAIL--1n3.rds")


# disagreement between catch_weight and bio sample for single fishing event with 17 large POP. 
# events[events$fishing_event_id==1506954,]$catch_weight
# fish[fish$fishing_event_id==1506954,]$length
# tidy_sets[tidy_sets$fishing_event_id==1506954,]$catch_weight

