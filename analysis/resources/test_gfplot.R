#testing gfplot on my system

library(gfplot)
## Not run: 
## Import survey catch density and location data by tow or set for plotting
## Specify single or multiple species by common name or species code and
## single or multiple survey series id(s).
get_survey_sets(species = "lingcod", ssid = 1)

## Import survey or commercial biological data for various plots (eg. length frequency, growth, age frequency, maturity, etc.)

get_survey_samples(species = 442, ssid = c(1, 3, 4, 16))

get_commercial_samples(c(442, 397))

## Import catch data by species for barcharts of landings by fishing area,
## geartype, and year.
get_catch("lingcod")

## Import spatial commercial catch per unit effort data for trawl or longline
## data by species for plotting along BC coast.
get_cpue_spatial("lingcod")
get_cpue_spatial_ll("yelloweye rockfish")

## Import catch and effort data by gear type for modelling commercial trawl
## cpue index.
get_cpue_index(gear = "bottom trawl", min_cpue_year = 2012)

## Import survey bootstrapped biomass estimates for plotting relative biomass
## indices by specified survey series.
get_survey_index("pacific cod", ssid = c(1, 3, 4, 16))

## End(Not run)

## Not run: 
cache_pbs_data_iphc("redbanded rockfish")
cache_pbs_data_iphc(c("redbanded rockfish",
                      "pacific ocean perch",
                      "made up species") )