getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() # parent = baseenv()

list_species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "North Pacific Spiny Dogfish",
  "Spotted Ratfish",
  # "Pacific Tomcod",
  "Walleye Pollock",
  # "Pacific Cod",
  "Sablefish",
  "Lingcod",
  # "Pacific Hake",
  # "Rosethorn Rockfish",
  "Redstripe Rockfish",
  "Yellowmouth Rockfish",
  # "Harlequin Rockfish",
  # "Bocaccio", # winter-birthing, overfished
  "Canary Rockfish", # schooling, winter-birthing
  # "Copper Rockfish", # small sample
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  # "Pacific Ocean Perch", # schooling
  "Quillback Rockfish",
  # "Redbanded Rockfish",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowtail Rockfish", # schooling
   "Longspine Thornyhead",
  "Shortspine Thornyhead",
  #"Arrowtooth Flounder",
  "Rex Sole",
  # "Petrale Sole",
  # "English Sole",
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Curlfin Sole",
  "Sand Sole",
  # "Slender Sole",
  # "Pacific Sanddab",
  # "Pacific Halibut"
)

### SUBSETS OF SPECIES
 list_species <- c(
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish",
  "Pacific Cod",
  "Pacific Halibut"
)

#   # "Arrowtooth Flounder"
#   # "Petrale Sole",
#   # "English Sole",

 
 list_species <- c(
   "Redstripe Rockfish",
   "Rougheye/Blackspotted Rockfish Complex",
   "Widow Rockfish",
   "Quillback Rockfish",
   "Bocaccio",
   "Shortraker Rockfish",
   "Yelloweye Rockfish"
 )
 
 # # NEW SPECIES WITHOUT maturity
list_species <- c(
   # "Pacific Halibut",
   "Big Skate",
   "Longnose Skate",
   "Spotted Ratfish"
)
 

### build time-varying depth models
list_regions <- c("All synoptic surveys")
# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model covariates
    # covs <- "-ssid-only"
    try({
      rmarkdown::render("2-biomass-depth-only-model.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs,
          knots = 400,
          update_model = TRUE # FALSE #
        ),
        output_file = paste0(
          "html/biomass-by-depth/biomass-by",
          covs, "-", spp, "-400.html"
        ),
        envir = env
      )
    })
  }
}

