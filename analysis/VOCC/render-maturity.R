getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

# SPECIES with maturity
list_species <- c(
  "Bocaccio",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Shortraker Rockfish",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod",
  # "Pacific Hake",
  # "Pacific Tomcod",
  # "Rosethorn Rockfish",
  "Redstripe Rockfish",
  "Yellowmouth Rockfish",
  # "Harlequin Rockfish"
  "Bocaccio", # winter-birthing, overfished
  "Canary Rockfish", # schooling, winter-birthing
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowtail Rockfish", # schooling
  "Yelloweye Rockfish", # summer-birthing, overfished,
  "Longspine Thornyhead",
  "Shortspine Thornyhead",
  "Arrowtooth Flounder",
  "Rex Sole",
  "Petrale Sole",
  "English Sole",
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Curlfin Sole",
  "Sand Sole"
  # "Slender Sole",
  # "Pacific Sanddab"
)


# # SPECIES WITHOUT maturity
# list_species <- c(
#   "North Pacific Spiny Dogfish",
#   "Big Skate",
#   "Longnose Skate",
#   "Spotted Ratfish",
#   "Pacific Halibut"
# )



list_regions <- c("All synoptic surveys")
# list_regions <- c(
#   "West Coast Vancouver Island",
#   "West Coast Haida Gwaii",
#   "both odd year surveys"
# )

# dir.create(file.path("html/maturity"))
for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("1-split-biomass-by-maturity.Rmd",
          params = list(
            species = list_species[spp_i],
            region = list_regions[r_h],
            split = TRUE
          ),
          output_file = paste0("html/maturity/maturity-", spp, ".html"),
          envir = env
        )
      })
    }
}

# # species <- "Copper Rockfish" # small sample
# # species <- "Shortbelly Rockfish" # small sample
# # species <- "Sandpaper Skate" # small sample
# # species <- "Brown Cat Shark" # small sample
