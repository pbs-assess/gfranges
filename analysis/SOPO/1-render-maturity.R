getwd()
setwd(here::here("/analysis/SOPO"))
env <- new.env() #parent = baseenv()

# SPECIES with maturity
list_species <- c(
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod",
  "Redstripe Rockfish",
  "Yellowmouth Rockfish",
  "Canary Rockfish", # schooling, winter-birthing
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Yellowtail Rockfish", # schooling
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
  "Sand Sole",
  "Widow Rockfish",
  "Quillback Rockfish",
  "Bocaccio",
  "Shortraker Rockfish",
  "Yelloweye Rockfish"
)


# species with small sample
list_species <- c(
  "Pacific Hake",
  "Pacific Tomcod",
  "Rosethorn Rockfish",
  "Slender Sole",
  "Pacific Sanddab",
  "Harlequin Rockfish"
  # "Copper Rockfish" ,
  # "Shortbelly Rockfish",
  # "Sandpaper Skate",
  # "Brown Cat Shark" 
)

list_species <- c( 
  "Sand Sole",
  "Butter Sole",
  "Starry Flounder")

### more species for SOPO
# yet to be retrieved
list_species <- list(
  "Buffalo Sculpin",
  "Cabezon",
  "Threadfin Sculpin",
 # "Pacifc Staghorn Sculpin",
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
  "Sand Sole",
  "Butter Sole",
  "C-O Sole"
)

list_species <- c("North Pacific Spiny Dogfish","Longspine Thornyhead","Sand Sole")

list_species <- c("Redbanded Rockfish")
list_species <- c("Pacific Ocean Perch")


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
        rmarkdown::render("1-trawl-split-by-maturity.Rmd",
          params = list(
            species = list_species[spp_i],
            region = list_regions[r_h],
            threshold = 0.05,
            split = TRUE
          ),
          output_file = paste0("html/maturity/maturity-", spp, "-0.05.html"),
          envir = env
        )
      })
    }
}


# # # SPECIES WITHOUT maturity
list_species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "Pacific Halibut"
)

## NEEDS SPECIAL SETTINGS
list_species <- c("North Pacific Spiny Dogfish","Longspine Thornyhead","Sand Sole")


# dir.create(file.path("html/maturity"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    try({
      rmarkdown::render("1-trawl-split-by-maturity.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          threshold = 0.05,
          split = FALSE
        ),
        output_file = paste0("html/maturity/maturity-", spp, ".html"),
        envir = env
      )
    })
  }
}



