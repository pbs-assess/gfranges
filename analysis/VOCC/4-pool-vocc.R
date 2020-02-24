getwd()
setwd(here::here("/analysis/VOCC/data"))
library("dplyr")

# covs <- "-fixed"
mydir = paste0( "_new_null")
mydir = paste0( "_do_null")
mydir = paste0( "_dvocc")

####################
myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles
biotic <- do.call(rbind, lapply(myfiles, read.csv)) %>% select(-X, -start_year)
glimpse(biotic)

#####################

saveRDS(biotic, file = paste0(
  "mature-all-temp-untrimmed.rds"
))

saveRDS(biotic, file = paste0(
  "mature-all-do-untrimmed.rds"
))

saveRDS(biotic, file = paste0(
  "mature-all-do-dvocc.rds"
))
