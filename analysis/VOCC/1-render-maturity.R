getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

# SPECIES with maturity
list_species <- c(
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  #"Shortraker Rockfish", # too few imm so ogive unreliable
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
  "Canary Rockfish", # schooling, winter-birthing
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  #"Quillback Rockfish",
  "Redbanded Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  #"Widow Rockfish", # schooling
  "Yellowtail Rockfish", # schooling
  #"Yelloweye Rockfish", # summer-birthing, overfished,
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
  # "Sand Sole"
  "Slender Sole",
  "Pacific Sanddab"
)


# # SPECIES to try setting maturity threshold to 0.05
# # Bocaccio reverted to 0.5 in June 2020
# # Shortraker not split due to tiny # of imm
# # Others seem to be left at 0.5?
# list_species <- c(
#   "Widow Rockfish",
#   "Quillback Rockfish",
#   "Bocaccio",
#   "Shortraker Rockfish",   
#   "Yelloweye Rockfish"
# )

# # # SPECIES WITHOUT maturity
# list_species <- c(
#   "Big Skate",
#   "Longnose Skate",
#   "Spotted Ratfish",
#   "Pacific Halibut"
# )

# species with small sample

### NEW ADDITIONS FOR SOPO #1
list_species <- c(
  "Pacific Hake",
  "Pacific Tomcod",
  "Rosethorn Rockfish",
  "Slender Sole",
  "Pacific Sanddab",
  "Harlequin Rockfish",
  "Copper Rockfish" ,
  "Shortbelly Rockfish",
  "Sandpaper Skate",
  "Brown Cat Shark",
  "Sand Sole",
  "Butter Sole",
  "Starry Flounder")

### NEW ADDITIONS FOR SOPO #2
list_species <- list(
  "Buffalo Sculpin",
  "Cabezon",
  # "Pacifc Staghorn Sculpin", # didn't find any records
  "Threadfin Sculpin", 
  "Red Irish Lord",
  "Sturgeon Poacher",
  "Bigmouth Sculpin",
  "Kelp Greenling",
  "Aleutian Skate",
  "Bigfin Eelpout",
  "Black Eelpout",
  "Wattled Eelpout",
  "Blackbelly Eelpout",
  "Shiner Perch",
  "Snake Prickleback",
  #Wolf Eel
  "Pacific Sand Lance",
  "Dusky Rockfish",
  "Chilipepper",
  "Pygmy Rockfish",
  "C-O Sole"
)


list_species <- c("Pacific Herring")

## NEEDS SPECIAL SETTINGS
list_species <- c("North Pacific Spiny Dogfish","Longspine Thornyhead","Sand Sole")

list_species <- c(
  "Pacific Ocean Perch"
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
            region = list_regions[r_h],
            threshold = 0.5,
            split = TRUE
          ),
          output_file = paste0("html/maturity/maturity-", spp, "0.5.html"),
          envir = env
        )
      })
    }
}

# # species <- "Copper Rockfish" # small sample
# # species <- "Shortbelly Rockfish" # small sample
# # species <- "Sandpaper Skate" # small sample
# # species <- "Brown Cat Shark" # small sample
