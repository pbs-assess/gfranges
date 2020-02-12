getwd()
setwd(here::here("/analysis/SOPO"))
env <- new.env() # parent = baseenv()


### build time-varying depth models
# list_regions <- c("All synoptic surveys")
list_regions <- c("Both odd year surveys")


list_species <- c(
  # "Big Skate",
  # "Longnose Skate",
  # "Spotted Ratfish",
  # "North Pacific Spiny Dogfish",
  # "Pacific Tomcod",
  # "Walleye Pollock",
  # "Pacific Cod",
  "Lingcod",
  "Sablefish",
  
  # "Bocaccio",
  "Canary Rockfish",
  "Copper Rockfish", # small sample
  # "Darkblotched Rockfish", # need predictions still
  "Greenstriped Rockfish",
  "Pacific Ocean Perch",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  # "Redstripe Rockfish", # faild to converge?
  "Rougheye/Blackspotted Rockfish Complex",
  # "Sharpchin Rockfish",
  # "Shortbelly Rockfish", # small sample
  # "Shortraker Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  # "Yellowmouth Rockfish",
  # "Yellowtail Rockfish", # schooling
  # "Yelloweye Rockfish" # ran mature only with AR1, rerunning without
  # "Longspine Thornyhead",
  "Shortspine Thornyhead",

  # "Pacific Halibut",
  "Arrowtooth Flounder",
  "Curlfin Sole",
  "Dover Sole",
  "English Sole",
  # "Flathead Sole", # needs predictions
  "Petrale Sole",
  # "Rex Sole", # needs predictions
  "Southern Rock Sole"
)


list_species <- c(
  # "Pacific Hake",
  # "Pacific Tomcod",
  # "Rosethorn Rockfish",
  # "Slender Sole",
  # "Pacific Sanddab",
  # "Harlequin Rockfish",
  # "Copper Rockfish" ,
  # # "Shortbelly Rockfish",
  "Sandpaper Skate"
  # "Brown Cat Shark"
)

list_species <- c( 
#   "Sand Sole",
# "Butter Sole",
"Starry Flounder"
  )


# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-no-covs" # string describing model covariates
    # covs <- "-ssid-only"
    try({
      rmarkdown::render("2-trawl-no-covs-model.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs,
          # start_year = 2005,
          knots = 200, #250
          AR1 = TRUE,
          fixed_spatial = FALSE,
          # update_model = FALSE,
          # update_model_check = FALSE, 
          # update_predictions = FALSE,
          update_model = TRUE,
          update_predictions = TRUE,
          update_model_check = TRUE,
          update_index = TRUE
        ),
        output_file = paste0(
          "html/density-models/biomass-by",
          covs, "-", spp, "-odd-reml-200.html"
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
