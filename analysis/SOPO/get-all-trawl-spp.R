library(dplyr)
getwd()
setwd(here::here("/analysis/SOPO"))


spp_table <- get_species() 
saveRDS(spp_table , file = "data/allspp.rds")


fish_table <- spp_table %>% filter(species_grouping == "fish")
species_list <- c(fish_table$species_common_name)
species_list <- na.omit(species_list)
species_list <- unique(species_list)

invert_table <- spp_table %>% filter(species_grouping == "invertebrate")
invert_list <- c(invert_table$species_common_name)
invert_list <- na.omit(invert_list)
invert_list <- unique(invert_list)

all_spp_trawl <- list()
for (i in species_list) {
  species <- i
  all_spp_trawl[[i]] <- try({gfdata::get_survey_sets(species, ssid = c(1, 3))})
}

all_trawl <- do.call("rbind", all_spp_trawl)
all_trawl_catch <- all_trawl %>% filter(catch_weight > 0)
all_trawl_catch_2019 <- all_trawl_catch %>% filter(year == 2019)

saveRDS(all_trawl_catch, file = "data/all_trawl_catch.rds")
saveRDS(all_trawl_catch_2019, file = "data/all_trawl_catch.rds")

####

#species codes not captured with above method

# 66S = blue mussel (exact species undetermined),	mytilus edulis complex
# 68P	= pacific scallop,	patinopecten caurinus x mizuhopecten yessoensis,	hybrid
# 394	= rougheye/blackspotted rockfish complex,	sebastes aleutianus/melanostictus complex
# 013	= unidentified shark,	unidentified shark
# 	015	unknown fish
# 390	NA	sebastosomus-type larvae