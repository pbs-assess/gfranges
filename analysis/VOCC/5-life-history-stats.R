# library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))

# getwd()
# setwd(here::here("/analysis/VOCC"))
# env <- new.env() #parent = baseenv()

species <- c(
  "Aleutian Skate",
  "Big Skate",
  "Longnose Skate",
  "Sandpaper Skate",
  "North Pacific Spiny Dogfish",
  "Brown Cat Shark",
  "Spotted Ratfish",
  "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Lingcod",
  "Pacific Hake",
  "Buffalo Sculpin",
  "Cabezon",
  # "Pacifc Staghorn Sculpin",
  "Red Irish Lord",
  "Sturgeon Poacher",
  "Bigmouth Sculpin",
  "Kelp Greenling",
  "Threadfin Sculpin",
  "Bigfin Eelpout",
  "Black Eelpout",
  "Wattled Eelpout",
  "Blackbelly Eelpout",
  "Shiner Perch",
  "Snake Prickleback",
  # "Wolf Eel"
  "Pacific Sand Lance",
  "Pacific Herring",
  "Sablefish",
  "Bocaccio",
  "Canary Rockfish",
  "Chilipepper",
  "Copper Rockfish", # small sample
  "Darkblotched Rockfish", # need predictions still
  "Dusky Rockfish",
  "Greenstriped Rockfish",
  "Harlequin Rockfish",
  "Pacific Ocean Perch",
  "Pygmy Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Rosethorn Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Shortraker Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish", # schooling
  "Yelloweye Rockfish",
  "Longspine Thornyhead",
  "Shortspine Thornyhead",
  "Pacific Halibut",
  "Arrowtooth Flounder",
  "Butter Sole",
  "C-O Sole",
  "Curlfin Sole",
  "Dover Sole",
  "English Sole",
  "Flathead Sole",
  "Pacific Sanddab",
  "Petrale Sole",
  "Rex Sole",
  "Southern Rock Sole",
  "Slender Sole",
  "Sand Sole",
  "Starry Flounder"
)

### test run choices ####
# species <- c("Spotted Ratfish")
# species <- "Quillback Rockfish"
# species <- "North Pacific Spiny Dogfish"
# species <- c("Bocaccio")
# species <- c("Shortraker Rockfish")
# species <- c( "Widow Rockfish")


# species <- "Lingcod"
# species <- "Yelloweye Rockfish"
# species <- "Redbanded Rockfish"
# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))


#####
life_history <- purrr::map_dfr(species, function(x) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(x)))
  fish1 <- readRDS(paste0("raw/bio-data-", spp, ""))
  group <- unique(fish1$maturity_convention_desc)
  group <- group[!group == "MATURITIES NOT LOOKED AT"]
  group <- group[!group == "PORT SAMPLES"]
  group <- gsub("\\(.*", "", group)
  group <- gsub("PACIFIC", "", group)
  group <- gsub(" ", "", group)

  bath <- readRDS("data/bathymetry-data")
  depth <- bath$data %>% select(fishing_event_id, depth)
  fish <- left_join(fish1, depth) %>% group_by(year) %>% mutate(mat_count = sum(!is.na(maturity_name))) %>% ungroup() 
  fish_yrs <- filter(fish, year > 2007) # filter to the relevant years
  min_mat_count <- min(fish_yrs$mat_count) # min fish in a any year
  total_mat_count <- sum(!is.na(fish_yrs$maturity_name)) # all fish with maturity data
  rm(maturity)
  # browser()
  try({
    maturity <- readRDS(paste0(
      "data/", spp,
      "/maturity-ogive-", spp, "-1n3n4n16.rds"
    ))
  })

  print(spp)
  if (exists("maturity")) {
    if (maturity$year_re) {
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
    small <- rbind(imm_m, imm_f) # %>% mutate(growth_l = length/age, growth_m = weight/age)
    large_threshold <- NA
    small_threshold <- NA
    mat_age <- mean(large$age, na.rm = TRUE)
    imm_age <- mean(small$age, na.rm = TRUE)
    
    age_count <- sum(!is.na(large$age))
    age_count_imm <- sum(!is.na(small$age))
    
    # age_mat <- round(quantile(imm_f$age, 0.95, na.rm = TRUE))
    # age_mat_m <- round(quantile(imm_m$age, 0.95, na.rm = TRUE))
    age_mat <- NA
    age_mat_m <- NA
    
    if(age_count_imm > 50){
    # browser()
    m <- gfplot:::fit_mat_ogive(fish1, type = "age", sample_id_re = F)
    age_mat <- round(m$mat_perc$f.p0.5, 1)
    age_mat_m <- round(m$mat_perc$m.p0.5, 1)
    }
    # imm_growth <- mean(small$growth_l, na.rm = TRUE)
    # imm_growth <- mean(small$growth_m, na.rm = TRUE)

  } else {
    length_50_mat_m <- NA
    length_50_mat_f <- NA
    large_threshold <- quantile(fish$length, 0.90, na.rm = TRUE)
    small_threshold <- quantile(fish$length, 0.10, na.rm = TRUE)
    large <- filter(fish, length > large_threshold)
    small <- filter(fish, length < small_threshold)
    mat_age <- NA
    imm_age <- NA
    age_mat <- NA
    age_mat_m <- NA
    age_count <- NA
    age_count_imm <- NA
  }

  # ## FOR EXPLORING SPECIAL CASES
  # #x <- species
  # plot(length~depth, data = fish, main = x)
  # fish_f <- filter(fish, sex == 2)
  # fish_m <- filter(fish, sex == 1)
  # hist(fish_m$length, breaks = 50, main = x)
  # hist(fish_f$length, breaks = 50, main = x)
  # hist(fish$weight, breaks = 50, main = x)
  # plot(weight~length, data=fish, main = x)
  # mid <- filter(fish, length > 40)#%>% filter(length<40)#small_threshold)
  # dep <- mean(mid$depth, na.rm =TRUE)
  # dep
  #
  large_depth <- mean(large$depth, na.rm = TRUE)
  small_depth <- mean(small$depth, na.rm = TRUE)

  prop_pos <- NA
  try({
    survey_sets <- readRDS(paste0("raw/event-data-", spp, ""))
    positive_sets <- filter(survey_sets, density_kgpm2 != 0)
    prop_pos <- round(nrow(positive_sets) / nrow(survey_sets), digits = 3)
    sets_2019 <- filter(survey_sets, year == 2019)
    weight_2019 <- sum(sets_2019$catch_weight, na.rm = T) # total 2019 catch in kg
  })

  biomass <- readRDS(paste0(
    "data/", spp, "/data-by-maturity-",
    spp, "-1n3n4n16.rds"
  ))

  if (nrow(biomass) < 3000) {
    rerun <- TRUE
  } else {
    rerun <- FALSE
  }
  fish_count <- nrow(fish)
  event_count <- sum(!is.na(unique(fish$fishing_event_id)))


  if (nrow(positive_sets) > 100) {
    if (any(names(biomass) == "imm_density")) {
      depth_mat_dens <- weighted.mean(biomass$depth, biomass$adult_density, na.rm = T)

      depth_mat_dens_95 <- reldist::wtd.quantile(biomass$depth, 0.95,
        na.rm = T, weight = biomass$adult_density * 1000000000
      )
      depth_mat_dens_75 <- reldist::wtd.quantile(biomass$depth, 0.75,
        na.rm = T, weight = biomass$adult_density * 1000000000
      )
      depth_mat_dens_50 <- reldist::wtd.quantile(biomass$depth, 0.5,
        na.rm = T, weight = biomass$adult_density * 1000000000
      )
      depth_mat_dens_25 <- reldist::wtd.quantile(biomass$depth, 0.25,
        na.rm = T, weight = biomass$adult_density * 1000000000
      )
      depth_mat_iqr <- round(depth_mat_dens_75 - depth_mat_dens_25)
      rm(biomass2)
      biomass2 <- filter(biomass, adult_density > 0)
      max_depth <- max(biomass2$depth)

      if (depth_mat_dens_25 == max_depth) {
        depth_mat_dens_95 <- NA
        depth_mat_dens_75 <- NA
        depth_mat_dens_50 <- NA
        depth_mat_dens_25 <- NA
        depth_mat_iqr <- NA
      }

      depth_imm_dens <- weighted.mean(biomass$depth, biomass$imm_density, na.rm = T)

      if (x == "Bocaccio") {
        # a single massive catch of immatures overwhelmed iqr for bocaccio, so simply downweighting it to merely double the second largest catch
        biomass$imm_density[biomass$fishing_event_id == 4546483] <- biomass$imm_density[biomass$fishing_event_id == 4546377] * 1.2
        # # alternatively, we could use the actual fish samples
        # depth_imm_dens_25 <- quantile(large$depth, 0.25, na.rm = TRUE)
        # depth_imm_dens_75 <- quantile(small$depth, 0.75, na.rm = TRUE)
      }

      if (x == "Widow Rockfish") {
        # a single massive catch of immatures overwhelmed iqr, so simply downweighting it to merely double the second largest catch
        biomass$imm_density[biomass$fishing_event_id == 2177561] <- biomass$imm_density[biomass$fishing_event_id == 308673] * 1.2
      }

      depth_imm_dens_95 <- reldist::wtd.quantile(biomass$depth, 0.975, na.rm = T, 
        weight = biomass$imm_density * 1000000000)
      depth_imm_dens_75 <- reldist::wtd.quantile(biomass$depth, 0.75, na.rm = T, 
        weight = biomass$imm_density * 1000000000)
      depth_imm_dens_50 <- reldist::wtd.quantile(biomass$depth, 0.5, na.rm = T, 
        weight = biomass$imm_density * 1000000000)
      depth_imm_dens_25 <- reldist::wtd.quantile(biomass$depth, 0.25, na.rm = T, 
        weight = biomass$imm_density * 1000000000)
      # depth_imm_dens_10 <- reldist::wtd.quantile (biomass$depth, 0.1, na.rm = T, weight=biomass$imm_density*1000000000)
      depth_imm_iqr <- round(depth_imm_dens_75 - depth_imm_dens_25)
      # depth_imm_range_90 <- round(depth_imm_dens_95 - depth_imm_dens_05)
      # browser()

      rm(biomass3)
      biomass3 <- filter(biomass, imm_density > 0)
      max_depth_imm <- max(biomass3$depth)

      if (depth_imm_dens_25 == max_depth_imm) {
        depth_imm_dens_95 <- NA
        depth_imm_dens_75 <- NA
        depth_imm_dens_50 <- NA
        depth_imm_dens_25 <- NA
        depth_imm_iqr <- NA
      }
    } else {
      depth_mat_dens <- weighted.mean(biomass$depth, biomass$density, na.rm = T)
      depth_mat_dens_95 <- reldist::wtd.quantile(biomass$depth, 0.95, na.rm = T, 
        weight = biomass$density * 1000000000)
      depth_mat_dens_75 <- reldist::wtd.quantile(biomass$depth, 0.75, na.rm = T, 
        weight = biomass$density * 1000000000)
      depth_mat_dens_50 <- reldist::wtd.quantile(biomass$depth, 0.5, na.rm = T, 
        weight = biomass$density * 1000000000)
      depth_mat_dens_25 <- reldist::wtd.quantile(biomass$depth, 0.25, na.rm = T, 
        weight = biomass$density * 1000000000)
      depth_mat_iqr <- round(depth_mat_dens_75 - depth_mat_dens_25)

      rm(biomass4)
      biomass4 <- filter(biomass, density > 0)
      max_depth <- max(biomass4$depth)

      if (depth_mat_dens_25 == max_depth) {
        depth_mat_dens_95 <- NA
        depth_mat_dens_75 <- NA
        depth_mat_dens_50 <- NA
        depth_mat_dens_25 <- NA
        depth_mat_iqr <- NA
      }
      depth_imm_dens <- small_depth
      depth_imm_dens_95 <- NA
      depth_imm_dens_75 <- NA
      depth_imm_dens_50 <- NA
      depth_imm_dens_25 <- NA
      depth_imm_iqr <- NA
    }
  } else {
    # if caught on less than 100 trawls revert to means from largest and smallest fish
    depth_mat_dens <- large_depth # quantile(fish$length, 0.90, na.rm = TRUE)
    depth_imm_dens <- small_depth # quantile(fish$length, 0.10, na.rm = TRUE)
    depth_mat_dens_95 <- NA
    depth_mat_dens_75 <- NA
    depth_mat_dens_50 <- NA
    depth_mat_dens_25 <- NA
    depth_mat_iqr <- NA
    depth_imm_dens_95 <- NA
    depth_imm_dens_75 <- NA
    depth_imm_dens_50 <- NA
    depth_imm_dens_25 <- NA
    depth_imm_iqr <- NA
  }
  # browser()

  ### calculate proportion of positive events without maturity ratio estimate
  prop_mean_ratio <- NA
  mean_ratio_mature <- NA
  rm(mean_ratios)
  if (any(names(biomass) == "imm_density")) {
    mean_ratios <- filter(biomass, present == 1) %>% filter(is.na(sample_n))
    aprox_density <- sum(mean_ratios$density)
    prop_mean_ratio <- aprox_density / sum(biomass$density)
    # pos_sets <- filter(biomass, present == 1) %>% tally()
    # count_mean_ratios <- filter(biomass, present == 1) %>%
    #    filter(is.na(sample_n)) %>% tally()
    # count_prop_mean_ratio <- count_mean_ratios/pos_sets
    # true_ratios <- filter(biomass, present == 1) %>% filter(!is.na(sample_n))
    mean_ratio_mature <- mean(biomass$mass_ratio_mature, na.rm = T)
  }

  list(
    species = x, group = group[1],
    min_mat_count = min_mat_count,
    total_mat_count = total_mat_count,
    total_fish = fish_count,
    total_events = event_count,
    prop_pos_sets = prop_pos,
    
    depth = round(depth_mat_dens),
    depth_imm = round(depth_imm_dens),
    depth_diff = round(depth_imm_dens - depth_mat_dens),
    depth_mat_iqr = depth_mat_iqr,
    depth_imm_iqr = depth_imm_iqr,
    depth_fish = round(large_depth),
    depth_imm_fish = round(small_depth),
    depth_diff_fish = round(small_depth - large_depth),
    depth_mat_dens_95 = round(depth_mat_dens_95),
    depth_mat_dens_75 = round(depth_mat_dens_75),
    depth_mat_dens_50 = round(depth_mat_dens_50),
    depth_mat_dens_25 = round(depth_mat_dens_25),
    depth_imm_dens_95 = round(depth_imm_dens_95),
    depth_imm_dens_75 = round(depth_imm_dens_75),
    depth_imm_dens_50 = round(depth_imm_dens_50),
    depth_imm_dens_25 = round(depth_imm_dens_25),
    age_count = age_count,
    age_count_imm = age_count_imm,
    age_mean = round(mat_age),
    age_imm = round(imm_age),
    age_mat = round(age_mat),
    age_mat_m = round(age_mat_m),
    age_max = round(quantile(fish$age, 0.999999, na.rm = TRUE)),
    length_max = max(fish$length, na.rm = TRUE),
    length_99th = round(quantile(fish$length, 0.9999, na.rm = TRUE)),
    length_50_mat_m = round(length_50_mat_m),
    length_50_mat_f = round(length_50_mat_f),
    weight_max = max(fish$weight, na.rm = TRUE) / 1000,
    weight_99th = round(quantile(fish$weight, 0.9999, na.rm = TRUE)) / 1000,
    # max_age = max(fish$age, na.rm = TRUE),
    large_threshold = large_threshold[[1]],
    small_threshold = small_threshold[[1]],
    mean_ratio_mature = mean_ratio_mature,
    density_mean_split = prop_mean_ratio,
    total_kg_2019 = weight_2019,
    rerun = rerun
  )
})

# View(life_history)

life_history$group[is.na(life_history$group)] <- "OTHER"
life_history$group[life_history$species == "Spotted Ratfish"] <- "RATFISH"
life_history$group[life_history$species == "Brown Cat Shark"] <- "SHARK"
life_history$group[life_history$species == "North Pacific Spiny Dogfish"] <- "SHARK"
life_history$group[life_history$species == "Chilipepper"] <- "ROCKFISH"
life_history$group[life_history$species == "Sandpaper Skate"] <- "SKATE"
life_history$group[life_history$species == "Big Skate"] <- "SKATE"
life_history$group[life_history$species == "Longnose Skate"] <- "SKATE"
life_history$group[life_history$species == "C-O Sole"] <- "FLATFISH"
life_history$group[life_history$species == "Aleutian Skate"] <- "SKATE"
# life_history$group[life_history$species=="	"] <- " "


# saveRDS(life_history, file = "data/life-history-stats.rds")
# life_history <- readRDS("data/life-history-stats.rds")

life_history <- life_history %>% mutate(species_common_name = tolower(species))

# spplist <- gfdata::get_species()
spplist <- readRDS("data/allspp.rds")

taxonomic_info <- spplist %>%
  select(species_common_name, species_science_name, parent_taxonomic_unit) %>%
  unique() %>%
  filter(species_science_name != "ophiodontinae")

life_history <- inner_join(life_history, taxonomic_info)

life_history$parent_taxonomic_unit[life_history$parent_taxonomic_unit == "bathyraja"] <- "rajidae(skates)"

saveRDS(life_history, file = "data/life-history-stats5.rds")

# split adults and immatures into separate rows and add CR's behavioural classifications
stats <- readRDS(paste0("data/life-history-stats5.rds"))
behav <- readr::read_csv("data/VOCCSpeciesList.csv") %>% rename(species = Species, age = Age)
behav$species[behav$species == "Rougheye/Blackspotted"] <- "Rougheye/Blackspotted Rockfish Complex"
behav$BenthoPelagicPelagicDemersal[behav$BenthoPelagicPelagicDemersal == "BenthoPelagic"] <- "Benthopelagic"
behav$Diet[behav$Diet == "CrabShrimp"] <- "Crustaceans"


stats$rockfish <- if_else(stats$group == "ROCKFISH", "rockfish", "other fishes")
stats <- stats %>% separate(species_science_name, " ", into = c("genus", "specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
stats$group[stats$group == "HAKE"] <- "COD"
imm <- mutate(stats, age = "immature") %>%
  mutate(depth = depth_imm, age_mean = age_imm, depth25 = depth_imm_dens_25, depth75 = depth_imm_dens_75, depth_iqr = depth_imm_iqr) %>%
  select(-depth_imm, -age_imm)
mat <- mutate(stats, age = "mature") %>%
  select(-depth_imm, -age_imm) %>%
  mutate(depth25 = depth_mat_dens_25, depth75 = depth_mat_dens_75, depth_iqr = depth_mat_iqr)
stats <- rbind(mat, imm)
stats$family <- gsub("\\(.*", "", stats$parent_taxonomic_unit)

stats <- left_join(behav, stats)

saveRDS(stats, file = "data/life-history-behav-new-growth.rds")
