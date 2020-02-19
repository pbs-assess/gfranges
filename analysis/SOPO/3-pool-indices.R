getwd()
setwd(here::here("/analysis/SOPO/data"))
library("dplyr")

mydir = paste0( "_indices")

####################
myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles
indices <- do.call(rbind, lapply(myfiles, read.csv)) 
glimpse(indices)

#####################

saveRDS(indices, file = paste0(
  "sopo-2019-indices2.rds"
))
