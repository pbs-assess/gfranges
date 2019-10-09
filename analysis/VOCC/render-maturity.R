getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

list_species <- c(
  # "Walleye Pollock",
  # "Pacific Cod",
  # "Sablefish",
  # "Lingcod",
  # "Pacific Hake",
  # "North Pacific Spiny Dogfish",
  # "Spotted Ratfish",
  # "Pacific Tomcod",
  # "Rosethorn Rockfish",
  # "Redstripe Rockfish",
  # "Yellowmouth Rockfish",
  # "Harlequin Rockfish"
   # "Bocaccio", # winter-birthing, overfished
  # "Canary Rockfish", # schooling, winter-birthing
  # "Copper Rockfish", # small sample
  # "Darkblotched Rockfish",
  # "Greenstriped Rockfish",
  # "Pacific Ocean Perch", # schooling
  # "Quillback Rockfish",
  # "Redbanded Rockfish",
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
  #"Arrowtooth Flounder",
  "Rex Sole",
  "Petrale Sole",
  "English Sole",
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Curlfin Sole",
  "Sand Sole",
  "Slender Sole",
  "Pacific Sanddab",
  "Pacific Halibut"
)

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
            region = list_regions[r_h]
          ),
          output_file = paste0("html/maturity/maturity-", spp, ".html"),
          envir = env
        )
      })
    }
}


# # Species run so far...
# species <- "Arrowtooth Flounder"
# species <- "Petrale Sole"
# species <- "English Sole"
# species <- "Dover Sole" # maturity data error for WCHG
# species <- "Southern Rock Sole"
# species <- "Flathead Sole"



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
