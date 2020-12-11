library(kableExtra)
setwd(here::here())
model_vel <- readRDS(here::here("analysis/VOCC/data/vel-all-95-optimized4-11-28-vel-both-1-600.rds"))
keepspp <- unique(model_vel$data$species_only)
stats <- readRDS(paste0("analysis/VOCC/data/life-history-behav-new-growth.rds")) %>% mutate(age = firstup(age)) 

spptab <- stats %>% filter(age == "Mature" & species %in% keepspp) %>% 
  arrange(desc(prop_pos_sets)) %>% 
  mutate( `Scientific name` = paste(firstup(genus), specific),
    family = firstup(family), 
    prop_pos_sets = paste0(round(prop_pos_sets*100),"%")
  ) %>%
  select(species, `Scientific name`, 
    #family, 
    prop_pos_sets, depth, depth_mat_iqr) %>% 
  rename(
    `Common name` = species,
    # Family = family,
    `Proportion present` = prop_pos_sets,
    `Mature mean` = depth,
    `Mature IQR` = depth_mat_iqr) 

keepimm <- filter(model_vel$data, age_class == "immature") %>% select(species_only) %>% distinct()
keepimm <- unique(keepimm$species_only)
spptabimm <- stats %>% filter(age == "Immature" & species %in% keepimm) %>% select(species, depth, depth_imm_iqr) %>%
  rename(
    `Common name` = species,
    `Immature mean` = depth,
    `Immature IQR` = depth_imm_iqr)

spptab <- left_join(spptab, spptabimm) 
# library("imputeTS")
spptab <- imputeTS::na_replace(spptab, "-") 

knitr::kable(spptab, "latex", vline = "", 
  booktabs = T) %>%  
  kable_styling(font_size = 9) %>%
  column_spec(2, italic = T) %>%
  row_spec(0, bold = T) 

