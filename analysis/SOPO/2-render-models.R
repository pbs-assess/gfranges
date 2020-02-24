getwd()
setwd(here::here("/analysis/SOPO"))
env <- new.env() # parent = baseenv()


### build time-varying depth models
# list_regions <- c("All synoptic surveys")
list_regions <- c("Both odd year surveys")


list_species <- c(
  "Aleutian Skate",
  "Big Skate",
  "Longnose Skate",
  "Sandpaper Skate",
  "North Pacific Spiny Dogfish",
  "Brown Cat Shark",
  "Spotted Ratfish",
  
  "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Lingcod",
  "Pacific Hake",
  "Buffalo Sculpin",
  "Cabezon",
  #"Pacifc Staghorn Sculpin",
  "Red Irish Lord",
  "Sturgeon Poacher",
  "Bigmouth Sculpin",
  "Kelp Greenling",
  "Threadfin Sculpin",
  "Bigfin Eelpout",
  "Black Eelpout",
  "Wattled Eelpout",
  "Blackbelly Eelpout",
  "Shiner Perch",
  "Snake Prickleback",
  # "Wolf Eel"
  "Pacific Sand Lance",
  "Pacific Herring",
  "Sablefish",
  "Bocaccio",
  "Canary Rockfish",
  "Chilipepper",
  "Copper Rockfish", # small sample
  "Darkblotched Rockfish", # need predictions still
  "Dusky Rockfish",
  "Greenstriped Rockfish",
  "Harlequin Rockfish",
  "Pacific Ocean Perch",
  "Pygmy Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Rosethorn Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Shortraker Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish", # schooling
  "Yelloweye Rockfish", 
  "Longspine Thornyhead",
  "Shortspine Thornyhead",

  "Pacific Halibut",
  "Arrowtooth Flounder",
  "Butter Sole",
  "C-O Sole",
  "Curlfin Sole",
  "Dover Sole",
  "English Sole",
  "Flathead Sole",
  "Pacific Sanddab", 
  "Petrale Sole",
  "Rex Sole",
  "Southern Rock Sole",
  "Slender Sole",
  "Sand Sole",
  "Starry Flounder"
  )

# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-no-covs-300" # string describing model covariates
    # covs <- "-ssid-only"
    try({
      rmarkdown::render("2-trawl-no-covs-model.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs,
          # start_year = 2005,
          knots = 300, #250
          AR1 = TRUE,
          fixed_spatial = FALSE,
          update_model = FALSE,
          update_model_check = FALSE,
          update_predictions = FALSE,
          # update_model = TRUE,
          # update_predictions = TRUE,
          # update_model_check = TRUE,
          # update_index = TRUE
          update_index = FALSE
        ),
        output_file = paste0(
          "html/density-models/biomass-by",
          covs, "-", spp, "-new-maps.html"
        ),
        envir = env
      )
    })
  }
}



# 
# 
# list_species <- c(
#   # "Big Skate",
#   # "Longnose Skate",
#   # "Spotted Ratfish",
#   # "North Pacific Spiny Dogfish",
#   # "Spotted Ratfish",
#   # # "Pacific Tomcod",
#   # "Walleye Pollock",
#   # "Pacific Cod",
#   # "Sablefish",
#   # "Lingcod",
#   # # "Pacific Hake",
#   # # "Rosethorn Rockfish",
#   # "Pacific Ocean Perch",
#   "Redstripe Rockfish", # faild to converge?
#   "Rougheye/Blackspotted Rockfish Complex",
#   "Yellowmouth Rockfish",
#   # "Harlequin Rockfish",
#   "Canary Rockfish",
#   "Copper Rockfish", # small sample
#   "Darkblotched Rockfish", # need predictions still
#   "Greenstriped Rockfish",
#   "Quillback Rockfish",
#   "Redbanded Rockfish",
#   "Sharpchin Rockfish",
#   "Shortbelly Rockfish", # small sample
#   "Silvergray Rockfish",
#   "Splitnose Rockfish",
#   "Widow Rockfish", # schooling
#   "Yellowtail Rockfish", # schooling
#   "Yelloweye Rockfish", # ran mature only with AR1, rerunning without
#   # "Bocaccio",
#   # "Shortraker Rockfish",
#   # "Longspine Thornyhead",
#   # "Shortspine Thornyhead",
#   # "Arrowtooth Flounder",
#   # "Rex Sole", # needs predictions
#   # "Petrale Sole",
#   # "English Sole",
#   # "Dover Sole",
#   # "Southern Rock Sole",
#   # "Flathead Sole", # needs predictions
#   # "Curlfin Sole",
#   # # "Sand Sole",
#   # # "Slender Sole",
#   # # "Pacific Sanddab",
#   # "Pacific Halibut"
# )
# 
# ## FAILED
# # "Bocaccio",
# # "Shortraker Rockfish",
# 
# # list_species <- c(
# #   "Curlfin Sole",
# #   # "Sand Sole", # not converging
# #   # "Slender Sole",
# #   # "Pacific Sanddab",
# #   "Pacific Halibut"
# #   )
# 
# 
# # dir.create(file.path("html/biomass-by-depth"))
# for (r_h in seq_along(list_regions)) {
#   for (spp_i in seq_along(list_species)) {
#     spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
#     covs <- "-tv-depth-only" # string describing model covariates
#     # covs <- "-ssid-only"
#     try({
#       rmarkdown::render("2-trawl-depth-only-model.Rmd",
#         params = list(
#           species = list_species[spp_i],
#           region = list_regions[r_h],
#           covariates = "", # additional non-climate variables
#           covs = covs,
#           knots = 300,
#           AR1 = TRUE,
#           fixed_spatial = FALSE,
#           # update_model = FALSE, 
#           # update_model_check = FALSE, 
#           # update_predictions = FALSE, 
#           update_model = TRUE,
#           update_model_check = TRUE,
#           update_predictions = TRUE,
#           update_index = TRUE
#         ),
#         output_file = paste0(
#           "html/density-models/biomass-by",
#           covs, "-", spp, "-odd-reml-250.html"
#         ),
#         envir = env
#       )
#     })
#   }
# }
