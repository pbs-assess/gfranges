library(dplyr)

### HBLL DATA
getwd()
setwd(here::here("/analysis/SOPO"))

gfdata::get_ssids() %>% View()

# gfdata::get_survey_index(222, ssid = c(22,36))

gfdata::get_survey_index(222, ssid = c(22,36, 39, 40))

dat <- gfdata::get_survey_sets(222, ssid = c(22))
  
dat <- gfdata::get_survey_sets(222, ssid = c(39, 40))

saveRDS(dat, "data/hbll-inside.rds")

gfdata::get_survey_ids()
gfdata::get_other_surveys()
# survey surveys_conducted_since_2008
# 1        Queen Charlotte Sound Multispecies Small-mesh Bottom Trawl                            7
# 2  West Coast Vancouver Island Multispecies Small-mesh Bottom Trawl                           12
# 3          Strait of Georgia Ecosystem Research Initiative Acoustic                            4
# 4                                 Sablefish Research and Assessment                            3
# 5                                Hard Bottom Longline Inside North                             6
# 6                                Hard Bottom Longline Inside South                             5
# 7                                 Inlet Standardized Sablefish Trap                           11
# 8                              Offshore Standardized Sablefish Trap                            3
# 9                         Offshore Stratified Random Sablefish Trap                           12
# 10                          Strait of Georgia Synoptic Bottom Trawl                            2
# 11                                    Joint Canada/US Hake Acoustic                            9
# 12                               Strait of Georgia Dogfish Longline                            4
# 13                    Eulachon Migration Study Bottom Trawl (South)                            3
# 14                    Eulachon Migration Study Bottom Trawl (North)                            2

dat <- gfdata::get_survey_sets("Sablefish", ssid = c(3))
saveRDS(dat, file = "data/sablefish3.rds")

#dat <- gfdata::get_survey_sets(species_list, ssid = c(39, 40))

# for (i in seq_along(species_list)){
# dat <- gfdata::get_survey_sets(species_list[i], ssid = c(1,3))
# }


range(dat$year)
range(dat2$year)

