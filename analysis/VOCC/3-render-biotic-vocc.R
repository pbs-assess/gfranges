.rs.restartR()
getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() # parent = baseenv()

list_species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "North Pacific Spiny Dogfish",
  # "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod",
  # "Pacific Hake",
  # "Rosethorn Rockfish",
  "Yellowmouth Rockfish",
  # "Harlequin Rockfish",
  "Canary Rockfish", # schooling, winter-birthing
  # "Copper Rockfish", # small sample
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish",
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
  # "Sand Sole",
  # "Slender Sole",
  # "Pacific Sanddab",
  "Pacific Halibut",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Widow Rockfish",
  "Quillback Rockfish",
  "Bocaccio",
  "Shortraker Rockfish",
  "Yelloweye Rockfish"
)

### build biotic gradients
list_regions <- c(
  "West Coast Vancouver Island",
  "West Coast Haida Gwaii",
  "both odd year surveys"
  #   "All synoptic surveys"
)

# age <- "mature"
if (age == "mature") {
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model
    reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
    try({
      rmarkdown::render("3-vocc-dist.Rmd",
        params = list(
          species = list_species[spp_i],
          # immature = TRUE,
          immature = FALSE,
          region = list_regions[r_h],
          covs = covs
        ),
        output_file = paste0(
          "html/VOCC-plots/vocc-w-do-", spp,
          # "-imm",
          covs, "-", reg, "-dvocc-decade.html"
        ),
        envir = env
      )
    })
  }
}
}

# age <- "immature"
if (age == "immature") {
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model
    reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
    try({
      rmarkdown::render("3-vocc-dist.Rmd",
        params = list(
          species = list_species[spp_i],
          immature = TRUE,
          region = list_regions[r_h],
          covs = covs
        ),
        output_file = paste0(
          "html/VOCC-plots/vocc-w-do-", spp,
          "-imm",
          covs, "-", reg, "-dvocc.html"
        ),
        envir = env
      )
    })
  }
}
}



## ALL YEARS WITH TEMP ONLY
# age <- "mature"
if age == "mature") {
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model
    reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
    try({
      rmarkdown::render("3-vocc-temp.Rmd",
        params = list(
          species = list_species[spp_i],
          # immature = TRUE,
          region = list_regions[r_h],
          covs = covs
        ),
        output_file = paste0(
          "html/biotic-vocc/temp-only-", spp,
          # "-imm",
          covs, "-", reg, "-untrimmed.html"
        ),
        envir = env
      )
    })
  }
}
}

# age <- "immature"
if age == "immature") {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      covs <- "-tv-depth-only" # string describing model
      reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
      try({
        rmarkdown::render("3-vocc-temp.Rmd",
          params = list(
            species = list_species[spp_i],
            immature = TRUE,
            region = list_regions[r_h],
            covs = covs
          ),
          output_file = paste0(
            "html/biotic-vocc/temp-only-", spp,
            "-imm",
            covs, "-", reg, "-untrimmed.html"
          ),
          envir = env
        )
      })
    }
  }
}