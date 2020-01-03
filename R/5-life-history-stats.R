# library(TMB)
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
  "Yellowtail Rockfish",
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "Bocaccio",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Shortraker Rockfish"
)

# species <- c("Spotted Ratfish")
# species <- c("Shortraker Rockfish")
# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

life_history <- purrr::map_dfr(species, function(x) {
  
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))
  fish <- readRDS(paste0("raw/bio-data-", spp, ""))
  group <- unique(fish$maturity_convention_desc)
  group <- group[!group == "MATURITIES NOT LOOKED AT"]
  group <- group[!group == "PORT SAMPLES"]
  group <- gsub("\\(.*", "", group)
  group <- gsub("PACIFIC", "", group)
  group <- gsub(" ", "", group)

  bath <- readRDS("data/bathymetry-data")
  depth <- bath$data %>% select(fishing_event_id, depth)
  fish <- left_join(fish, depth)
  
  rm(maturity)
  try({
    maturity <- readRDS(paste0("data/", spp, 
    "/maturity-ogive-", spp, "-1n3n4n16.rds"))
  })
  
  if(exists("maturity")) {
    
    if(maturity$year_re) {
    length_50_mat_m <- maturity$mat_perc$mean$m.mean.p0.5
    length_50_mat_f <- maturity$mat_perc$mean$f.mean.p0.5
    } else {
    length_50_mat_m <- maturity$mat_perc$m.p0.5
    length_50_mat_f <- maturity$mat_perc$f.p0.5
    }
    
  mat_m <- filter(fish, sex == 1) %>% filter(length > length_50_mat_m)
  mat_f <- filter(fish, sex == 2) %>% filter(length > length_50_mat_f)
  
  imm_m <- filter(fish, sex == 1) %>% filter(length < length_50_mat_m)
  imm_f <- filter(fish, sex == 2) %>% filter(length < length_50_mat_f)
  
  large <- rbind(mat_m, mat_f)
  small <- rbind(imm_m, imm_f)
  large_threshold <- NA
  small_threshold <- NA
  
  } else {
    
  length_50_mat_m <- NA
  length_50_mat_f <- NA
  large_threshold <- quantile(fish$length, 0.90, na.rm = TRUE)
  small_threshold <- quantile(fish$length, 0.10, na.rm = TRUE)
  large <- filter(fish, length > large_threshold)
  small <- filter(fish, length < 20)#small_threshold)
  }
  
  ## FOR EXPLORING SPECIAL CASES
  # fish_f <- filter(fish, sex == 2)
  # fish_m <- filter(fish, sex == 1)
  # mid <- filter(fish, length > 40)#%>% filter(length<40)#small_threshold)
  # dep <- mean(mid$depth, na.rm =TRUE)
  # dep
  
  large_depth <- mean(large$depth, na.rm = TRUE)
  small_depth <- mean(small$depth, na.rm = TRUE)
  
  prop_pos <- NA
  try({survey_sets <- readRDS(paste0("raw/event-data-", spp, ""))
  positive_sets <- filter(survey_sets, density_kgpm2 != 0)
  prop_pos <- round(nrow(positive_sets)/nrow(survey_sets), digits = 3)
  })
  
  list(
    species = x, group = group[1],
    length_50_mat_m = round(length_50_mat_m),
    length_50_mat_f = round(length_50_mat_f),
    mat_depth = round(large_depth),
    imm_depth = round(small_depth),
    depth_diff = round(small_depth - large_depth),
    max_weight = max(fish$weight, na.rm = TRUE) / 1000,
    large_weight = round(quantile(fish$weight, 0.9999, na.rm = TRUE)) / 1000,
    # max_age = max(fish$age, na.rm = TRUE),
    max_age = round(quantile(fish$age, 0.999999, na.rm = TRUE)),
    max_length = max(fish$length, na.rm = TRUE),
    long_length = round(quantile(fish$length, 0.9999, na.rm = TRUE)),
    large_threshold = large_threshold[[1]],
    small_threshold = small_threshold[[1]],
    prop_pos_sets = prop_pos
  )
})

View(life_history)
life_history$group[is.na(life_history$group)] <- "SKATE"

saveRDS(life_history, file = "data/life-history-stats.rds")
