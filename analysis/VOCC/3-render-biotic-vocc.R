# .rs.restartR()
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

### Nov 2020 run:
# with new sdmTMB models for mature
# a few added species
# full data set for few spp with missing data
list_species <- c(
  "Big Skate",
  "Longnose Skate",
  "Spotted Ratfish",
  "North Pacific Spiny Dogfish",
  # "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod", # already rerun
  "Pacific Hake", # previously excluded
  "Rosethorn Rockfish", # previously excluded
  "Yellowmouth Rockfish",
  # "Harlequin Rockfish", # too small sample
  "Canary Rockfish", # already rerun
  # "Copper Rockfish", # too small sample
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish", # already rerun
  "Sharpchin Rockfish",
  #"Shortbelly Rockfish", # too small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Yellowtail Rockfish", # schooling
  # "Longspine Thornyhead", # too small sample
  "Shortspine Thornyhead",
  "Arrowtooth Flounder",
  "Rex Sole",
  "Petrale Sole",
  "English Sole", # already rerun
  "Dover Sole",
  "Southern Rock Sole",
  "Flathead Sole",
  "Curlfin Sole",
  # "Sand Sole",# too small sample
  "Slender Sole",# previously excluded
  "Pacific Sanddab",# previously excluded
  "Pacific Halibut",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Widow Rockfish",
  "Quillback Rockfish",
  "Bocaccio", # rerun with fewer knots
  "Shortraker Rockfish",# small sample so run with fewer knots
  "Yelloweye Rockfish"
)


### build biotic gradients
list_regions <- c(
  "West Coast Haida Gwaii",
  "West Coast Vancouver Island",
  "both odd year surveys"
  #   "All synoptic surveys"
)

age <- "mature"
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
          immature = FALSE,
          region = list_regions[r_h],
          covs = covs
        ),
        output_file = paste0(
          "html/VOCC-plots/vocc-w-do-", spp,
          covs, "-", reg, "-more2016-new.html"
        ),
        envir = env
      )
    })
  }
}
}

# ######
# # TODO: will need to rerun these imm
# ######
# list_species <- c(
#   "Rougheye/Blackspotted Rockfish Complex",
#   "Widow Rockfish",
#   "Quillback Rockfish",
#   "Yelloweye Rockfish"
# )
# 
# list_species <- c(
#   "Bocaccio"
# )

list_regions <- c(
  "West Coast Haida Gwaii",
  "West Coast Vancouver Island",
  "both odd year surveys"
  #   "All synoptic surveys"
)

age <- "immature"
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
          covs, "-", reg, "-more2016.html"
        ),
        envir = env
      )
    })
  }
}
}



# ## ALL YEARS WITH TEMP ONLY
# # age <- "mature"
# if age == "mature") {
# for (r_h in seq_along(list_regions)) {
#   for (spp_i in seq_along(list_species)) {
#     spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
#     covs <- "-tv-depth-only" # string describing model
#     reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
#     try({
#       rmarkdown::render("3-vocc-temp.Rmd",
#         params = list(
#           species = list_species[spp_i],
#           # immature = TRUE,
#           region = list_regions[r_h],
#           covs = covs
#         ),
#         output_file = paste0(
#           "html/biotic-vocc/temp-only-", spp,
#           # "-imm",
#           covs, "-", reg, "-untrimmed.html"
#         ),
#         envir = env
#       )
#     })
#   }
# }
# }
# 
# # age <- "immature"
# if age == "immature") {
#   for (r_h in seq_along(list_regions)) {
#     for (spp_i in seq_along(list_species)) {
#       spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
#       covs <- "-tv-depth-only" # string describing model
#       reg <- gsub(" ", "-", gsub("\\/", "-", tolower(list_regions[r_h])))
#       try({
#         rmarkdown::render("3-vocc-temp.Rmd",
#           params = list(
#             species = list_species[spp_i],
#             immature = TRUE,
#             region = list_regions[r_h],
#             covs = covs
#           ),
#           output_file = paste0(
#             "html/biotic-vocc/temp-only-", spp,
#             "-imm",
#             covs, "-", reg, "-untrimmed.html"
#           ),
#           envir = env
#         )
#       })
#     }
#   }
# }