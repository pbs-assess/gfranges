getwd()
setwd(here::here("/analysis/VOCC/data"))
library("dplyr")

# covs <- "-fixed"
# mydir = paste0( "biotic_test1")
mydir <- paste0("_new_biotic/mat")
mydir = paste0( "_new_biotic/imm")
mydir = paste0( "_scrambled1")
mydir = paste0( "_scrambled2")
mydir = paste0( "_scrambled3")
mydir = paste0( "_scrambled4")
mydir = paste0( "_scrambled5")
mydir = paste0( "_all_temp")
mydir = paste0( "_new_null")
mydir = paste0( "_do_null")

####################
myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles
biotic <- do.call(rbind, lapply(myfiles, read.csv)) %>% select(-X, -start_year)
glimpse(biotic)

#####################
saveRDS(biotic, file = paste0(
  "multi-spp-biotic-vocc-mature-with-fished.rds"
))

saveRDS(biotic, file = paste0(
  "multi-spp-biotic-vocc-immature-with-fished.rds"
))

saveRDS(biotic, file = paste0(
  "scrambled5-mature-with-fished.rds"
))

saveRDS(biotic, file = paste0(
  "all-temp-mature-with-fished.rds"
))

saveRDS(biotic, file = paste0(
  "mature-all-temp-untrimmed.rds"
))

saveRDS(biotic, file = paste0(
  "mature-all-do-untrimmed.rds"
))
