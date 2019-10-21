getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

list_species <- c(
  #"North Pacific Spiny Dogfish",
  #"Spotted Ratfish",
  # "Pacific Tomcod",
  # "Walleye Pollock",
   "Pacific Cod",
  # "Sablefish",
  # "Lingcod",
  # "Pacific Hake",
  # "Rosethorn Rockfish",
  # "Redstripe Rockfish",
  # "Yellowmouth Rockfish",
  # "Harlequin Rockfish",
  # "Bocaccio", # winter-birthing, overfished
  # "Canary Rockfish", # schooling, winter-birthing
  # "Copper Rockfish", # small sample
  # "Darkblotched Rockfish",
  # "Greenstriped Rockfish",
   "Pacific Ocean Perch", # schooling
  # "Quillback Rockfish",
   "Redbanded Rockfish",
  # "Rougheye/Blackspotted Rockfish Complex",
  # "Sharpchin Rockfish",
  # "Shortbelly Rockfish", # small sample
  # "Silvergray Rockfish",
  # "Splitnose Rockfish",
  # "Widow Rockfish", # schooling
  # "Yellowtail Rockfish", # schooling
  # "Yelloweye Rockfish", # summer-birthing, overfished,
  # "Longspine Thornyhead",
  # "Shortspine Thornyhead",
  # "Arrowtooth Flounder",
  # "Rex Sole",
  # "Petrale Sole",
  # "English Sole",
  # "Dover Sole",
  # "Southern Rock Sole",
  # "Flathead Sole",
  # "Curlfin Sole",
  # "Sand Sole",
 # "Slender Sole",
 # "Pacific Sanddab",
  "Pacific Halibut"
)
list_regions <- c("All synoptic surveys")


# dir.create(file.path("html/biomass-by-depth"))

for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("2-biomass-depth-only-model.Rmd",
          params = list(
            species = list_species[spp_i],
            region = list_regions[r_h],
            covariates = "+trawled+as.factor(ssid)", # additional non-climate variables
            covs = "-trawled-ssid", # string describing model covariates
            knots = 600,
            update_model = FALSE
          ),
          output_file = paste0("html/biomass-by-depth/biomass-by-depth-", spp, ".html")
        )
      })
    }
}

#dir.create(file.path("html/biomass-trends"))

list_regions <- c(
   "West Coast Vancouver Island",
#   "West Coast Haida Gwaii",
   "both odd year surveys"
#   "All synoptic surveys"
)


for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs = "-trawled-ssid" # string describing model 
    try({
      rmarkdown::render("2-biomass-trends.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covs = covs
        ),
        output_file = paste0("html/biomass-trends/biomass-trends-", spp, covs, ".html")
      )
    })
  }
}

# covariates <- "+muddy+any_rock"
# covariates <- "+muddy+mixed+rocky"
# covariates <- "+mixed+rocky"
# covariates <- "+rocky"
# covariates <- "+mixed_scaled+rocky_scaled+muddy_scaled"

# covs <- gsub("\\+", "-", covariates)
# covs <- gsub("\\+", "-", "+muddy+any_rock")
# covs <- gsub("\\+", "-", "-variable-substrate")
# covs <- gsub("\\+", "-", "-all-substrates")


# # Species run so far...

# species <- "Silvergray Rockfish"
# species <- "Quillback Rockfish" # summer-birthing
# species <- "Yelloweye Rockfish" # summer-birthing, overfished, 
# species <- "Bocaccio" # winter-birthing, overfished
# species <- "Sharpchin Rockfish"
# species <- "Splitnose Rockfish"

# species <- "Rougheye/Blackspotted Rockfish Complex"
# species <- "Redbanded Rockfish"
# species <- "Greenstriped Rockfish"
# # species <- "Copper Rockfish" # small sample
# species <- "Darkblotched Rockfish"
# # species <- "Shortbelly Rockfish" # small sample

# species <- "Pacific Ocean Perch" # schooling
# species <- "Widow Rockfish" # schooling
# species <- "Yellowtail Rockfish" # schooling
# species <- "Canary Rockfish" # schooling, winter-birthing
# species <- "Shortraker Rockfish"
# species <- "Longspine Thornyhead"
# species <- "Shortspine Thornyhead"

# species <- "Walleye Pollock"
# species <- "Pacific Cod"
# species <- "Sablefish"
# species <- "Lingcod"
# species <- "Pacific Hake"

# species <- "North Pacific Spiny Dogfish" # note: using all data for maturity thresholds
# species <- "Longnose Skate"
# species <- "Big Skate"
# species <- "Spotted Ratfish"
# # species <- "Sandpaper Skate" # small sample
# # species <- "Brown Cat Shark" # small sample
