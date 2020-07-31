# getwd()
setwd(here::here("/analysis/VOCC/data"))
library("dplyr")

#### MATURE ####
# for models on the 06-20 that use 500 knots for temp and 800 for DO
# mydir = paste0( "_newclim2_mature")
# 800 kn for both
# mydir = paste0( "_more2016_mature")
mydir = paste0( "_optimized_mature")
myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles
biotic <- do.call(rbind, lapply(myfiles, read.csv)) %>% select(-X, -start_year)
glimpse(biotic)

saveRDS(biotic, file = paste0(
  # "mature-all-do-newclim3.rds"
  "mature-optimized-vocc.rds"
))

#### IMMATURE #####
# mydir = paste0( "_more2016_immature")
mydir = paste0( "_optimized_immature")
myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles
biotic <- do.call(rbind, lapply(myfiles, read.csv)) %>% 
  select(-X, -start_year)
glimpse(biotic)

saveRDS(biotic, file = paste0(
  # "immature-all-do-newclim3.rds"
  "immature-optimized-vocc.rds"
))
