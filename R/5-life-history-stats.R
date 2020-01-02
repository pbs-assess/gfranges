#library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))

# getwd()
# setwd(here::here("/analysis/VOCC"))
# env <- new.env() #parent = baseenv()


species <- c(
  "Arrowtooth Flounder", 
  "Canary Rockfish",
  "Curlfin Sole",
  "Darkblotched Rockfish",
  "Dover Sole",
  "English Sole",
  "Flathead Sole",
  "Greenstriped Rockfish",
  "Lingcod",
  "Longspine Thornyhead",
  "North Pacific Spiny Dogfish",
  "Pacific Cod",
  "Pacific Halibut",
  "Pacific Ocean Perch",
  "Petrale Sole",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Rex Sole",
  "Sablefish",
  "Sand Sole",
  "Sharpchin Rockfish",
  "Shortspine Thornyhead",
  "Silvergray Rockfish",
  "Southern Rock Sole",
  "Splitnose Rockfish",
  "Walleye Pollock",
  "Widow Rockfish",
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish", 
  "Yellowtail Rockfish"
)

# species <- c("Arrowtooth Flounder")
# species <- c("Widow Rockfish")
# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

life_history <- purrr::map_dfr(species, function (x) {
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))
fish <- readRDS(paste0("raw/bio-data-", spp, ""))
group <- unique(fish$maturity_convention_desc) 
group <- group[!group=="MATURITIES NOT LOOKED AT"]
group <- group[!group=="PORT SAMPLES"]
group <- gsub("\\(.*","", group)
group <- gsub("PACIFIC","", group)
group <- gsub(" ","", group)

bath <- readRDS("data/bathymetry-data") 
depth <- bath$data %>% select(fishing_event_id, depth)
fish <- left_join(fish, depth)
large_threshold <- quantile(fish$length, 0.75, na.rm = TRUE)
small_threshold <- quantile(fish$length, 0.25, na.rm = TRUE)
large <- filter(fish, length > large_threshold)
small <- filter(fish, length < small_threshold)

list(
  species = x, group = group[1],
  large_depth = mean(large$depth, na.rm = TRUE),
  small_depth = mean(small$depth, na.rm = TRUE),
  max_weight = max(fish$weight, na.rm = TRUE)/1000,
  large_weight = quantile(fish$weight, 0.9999, na.rm = TRUE)/1000,
  #max_age = max(fish$age, na.rm = TRUE),
  max_age = round(quantile(fish$age, 0.999999, na.rm = TRUE)),
  max_length = max(fish$length, na.rm = TRUE),
  long_length = quantile(fish$length, 0.9999, na.rm = TRUE),
  large_threshold = large_threshold,
  small_threshold = small_threshold
)
})

saveRDS(life_history, file = "data/life-history-stats.rds")




