

list_species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "North Pacific Spiny Dogfish",
  # "Pacific Tomcod",
  "Lingcod", # already rerun
  "Pacific Hake", # previously excluded
  "Pacific Cod",
  "Walleye Pollock",
  "Sablefish",

  "Bocaccio", # rerun with fewer knots
  "Canary Rockfish", # already rerun
  # "Copper Rockfish", # too small sample
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Quillback Rockfish",
  "Redbanded Rockfish", # already rerun
  "Redstripe Rockfish",
  "Rosethorn Rockfish", # previously excluded
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  #"Shortbelly Rockfish", # too small sample
  "Shortraker Rockfish",# small sample so run with fewer knots
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish",
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish", # schooling

  # "Harlequin Rockfish", # too small sample
  # "Longspine Thornyhead", # too small sample
  "Shortspine Thornyhead",

  "Arrowtooth Flounder",
  "Curlfin Sole",
  "Dover Sole",
  "English Sole", # already rerun
  "Flathead Sole",
  "Petrale Sole",
  "Rex Sole",
  # "Sand Sole",# previously excluded and does not converge
  "Slender Sole",# previously excluded
  "Southern Rock Sole",

  "Pacific Sanddab",# previously excluded
  "Pacific Halibut"
)


# updated
list_species <- c(
  # "Longnose Skate",
  # "Redbanded Rockfish", # problem with tv depth estimate due to possible fish left in net
  # "Dover Sole", # deep fish found shallow...
  # "Slender Sole", # deep fish found shallow...
  # "Pacific Ocean Perch"
  
  "Bocaccio" # rerun with fewer knots
)


setwd(here::here("analysis/VOCC/"))
library("gfranges")
library(tidyverse)

depth_plots <- list()
rm(adult_biomass)
rm(imm_biomass)

for (spp_i in seq_along(list_species)) {

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
covs <- "-tv-depth-only" 
rm(adult_biomass)
rm(imm_biomass)
try({
  adult_biomass <- readRDS(paste0(
    "data/", spp,
    "/mod-mat-biomass-", spp, covs, "-1n3n4n16-new.rds"
  ))
})

if (!exists("adult_biomass")) {
  try({
    adult_biomass <- readRDS(paste0(
      "data/", spp,
      "/model-total-biomass-", spp, covs, "-1n3n4n16.rds"
    ))
    # adult_biomass<-sdmTMB:::update_model(adult_biomass)
  })
}

try({
  imm_biomass <- readRDS(paste0(
    "data/", spp,
    "/mod-imm-biomass-", spp, covs, "-1n3n4n16.rds"
  ))
  # imm_biomass<-sdmTMB:::update_model(imm_biomass)
})

# browser()
if (!exists("adult_biomass")) adult_biomass <- NULL
if (!exists("imm_biomass")) imm_biomass <- NULL

depth_model_list <- list(adult = adult_biomass, imm = imm_biomass)
depth_model_list <- depth_model_list[!sapply(depth_model_list, is.null)]

d <- list()
for (i in seq_len(length(depth_model_list))) {
  d[[i]] <- time_varying_density(depth_model_list[[i]], predictor = "depth")
  d[[i]]$x <- exp(d[[i]]$x)
  depth_plots[[(spp_i * 2) - 1 + i ]] <- plot_mountains(d[[i]], variable_label = "Depth", xlimits = c(0, 600)) +
      ggtitle(paste(list_species[spp_i], names(depth_model_list[i])))
}
}

# pdf(paste0("all-depth-plots.pdf"))
depth_plots
# dev.off()

