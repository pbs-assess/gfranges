
getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

list_species <- c(
  "North Pacific Spiny Dogfish",
  "Spotted Ratfish",
  "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Sablefish",
  "Lingcod",
  "Pacific Hake",
  "Rosethorn Rockfish",
  "Redstripe Rockfish",
  "Yellowmouth Rockfish",
  "Harlequin Rockfish",
  "Bocaccio", # winter-birthing, overfished
  "Canary Rockfish", # schooling, winter-birthing
  "Copper Rockfish", # small sample
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

#dir.create(file.path("html/biomass-by-climate"))

# raw temperature alone for all years
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-all-temp-trawled" # string describing model covariates
    priors <- FALSE # call model that used priors?
    try({
      rmarkdown::render("3-climate-models-fixed-both.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          log_temp = FALSE, # must match covs
          model_w_do = FALSE,
          priors = priors, 
          knots = 800,
          covariates = "+trawled", # additional non-climate variables
          covs = covs 
        ),
        output_file = paste0("html/biomass-by-climate/biomass-by-climate-", spp, covs, "-priors-", priors,".html"),
        envir = env
      )
    })
  }
}

# log(temperature) alone for all years
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-log-temp-trawled" # string describing model covariates
    priors <- FALSE # call model that used priors?
    try({
      rmarkdown::render("3-climate-models-fixed-both.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          priors = priors, 
          log_temp = TRUE, # must match covs
          model_w_do = FALSE,
          covariates = "+trawled", # additional non-climate variables
          covs = covs 
        ),
        output_file = paste0("html/biomass-by-climate/biomass-by-climate-", spp, covs, "-priors-", priors,".html"),
        envir = env
      )
    })
  }
}


# log(temperature) 
for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-log-both-trawled" # string describing model covariates
    priors <- FALSE # call model that used priors?
    try({
      rmarkdown::render("3-climate-models-fixed-both.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          priors = priors, 
          log_temp = TRUE, # must match covs
          covariates = "+trawled", # additional non-climate variables
          covs = covs 
        ),
        output_file = paste0("html/biomass-by-climate/biomass-by-climate-", spp, covs, "-priors-", priors,".html"),
        envir = env
      )
    })
  }
}

# scaled raw temp WITHOUT priors

for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-log-do-trawled" # string describing model covariates
    priors <- FALSE # call model that used priors?
    try({
      rmarkdown::render("3-climate-models-fixed-both.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          priors = priors, 
          log_temp = FALSE, # must match covs
          covariates = "+trawled", # additional non-climate variables; must match covs
          covs = covs 
        ),
        output_file = paste0("html/biomass-by-climate/biomass-by-climate-", spp, covs, "-priors-", priors,".html")
      )
    })
  }
}


## Plots of thresholds
dir.create(file.path("html/threshold-plots"))

# # scaled raw temp with priors
# for (spp_i in seq_along(list_species)) {
#     spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
#     covs <- "-log-do-trawled"
#     priors <- TRUE
#     try({
#       rmarkdown::render("3-fixed-var-response-plots.Rmd",
#         params = list(
#           species = list_species[spp_i],
#           priors = priors, # call model that used priors?
#           log_temp = FALSE,
#           covs = covs, # covariate string used to name model rds
#           cov_number = 2 # how many climate covariates 
#         ),
#         output_file = paste0("html/threshold-plots/threshold-plots-", spp, covs, "-priors-", priors, ".html")
#       )
#     })
# }


# log(temperature) and no priors version

for (spp_i in seq_along(list_species)) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
  covs <- "-log-both-trawled" # covariate string used to name model rds
  priors <- FALSE # call model that used priors?
  try({
    rmarkdown::render("3-fixed-var-response-plots.Rmd",
      params = list(
        species = list_species[spp_i],
        priors = priors, 
        log_temp = TRUE,
        covs = covs, 
        cov_number = 2 # how many climate covariates 
      ),
      output_file = paste0("html/threshold-plots/threshold-plots-", spp, covs, "-priors-", params$priors, ".html")
    )
  })
}


# scaled raw temp WITHOUT priors

for (spp_i in seq_along(list_species)) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
  covs <- "-log-do-trawled"
  priors <- FALSE
  try({
    rmarkdown::render("3-fixed-var-response-plots.Rmd",
      params = list(
        species = list_species[spp_i],
        priors = priors, # call model that used priors?
        log_temp = FALSE,
        covs = covs, # covariate string used to name model rds
        cov_number = 2 # how many climate covariates 
      ),
      output_file = paste0("html/threshold-plots/threshold-plots-", spp, covs, "-priors-", priors, ".html")
    )
  })
}

# log(temperature) WITHOUT DO but with all years
dir.create(file.path("html/threshold-plots/all-log-temp-trawled"))

for (spp_i in seq_along(list_species)) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
  covs <- "-log-temp-trawled" # covariate string used to name model rds
  priors <- FALSE # call model that used priors?
  try({
    rmarkdown::render("3-fixed-var-response-plots.Rmd",
      params = list(
        species = list_species[spp_i],
        priors = priors, 
        log_temp = TRUE,
        model_w_do = FALSE,
        covs = covs, 
        cov_number = 2 # how many climate covariates 
      ),
      output_file = paste0("html/threshold-plots/all", covs, "/threshold-plots-", spp, covs, "-priors-", priors, ".html")
    )
  })
}

# # Species run so far...
# species <- "Arrowtooth Flounder" # temp good, not DO, imm moving deeper?

## Sebastes
# 
# ## TRY BOTH - not schooling
#  species <- "Yelloweye Rockfish" # beautiful curves, with depth change ***
#  species <- "Bocaccio" # good curves, no depth change ... most survey-specific models fail to find minimum ***
#  species <- "Sharpchin Rockfish" # beautiful curves, imm depth change ***
#  species <- "Greenstriped Rockfish" # beautiful curves for adults, no depth change ***

# ## TRY BOTH - schooling
#  species <- "Pacific Ocean Perch" # beautiful curves, no depth change, schooling ***
#  species <- "Silvergray Rockfish" # beautiful curves, no depth change, schooling ***

# ## TRY JUST TEMPERATURE 
# # species <- "Rougheye/Blackspotted Rockfish Complex" # good temp, not DO
# # species <- "Redbanded Rockfish" # good temp, not DO, maybe moving shallower
#  species <- "Splitnose Rockfish" # good temp, survey 4 DO ok


# # species <- "Yellowtail Rockfish" #  good temp for survey 4 only, no DO, schooling
# # species <- "Quillback Rockfish" # like warm = overall a bit flat... territorial! 
# # species <- "Canary Rockfish" # schooling, winter-birthing, and depth change!, but curves at edges... adult1 looks ok?
#  species <- "Widow Rockfish" # schooling, and depth change, but curves at edges

# species <- "Longspine Thornyhead" # too deep to produce curves
# species <- "Shortspine Thornyhead"
# species <- "Sablefish" # temp might work, no DO, no consistant depth change
# species <- "Lingcod" # curves ok, overall a bit flat
# # species <- "Pacific Hake" # not useful

# species <- "North Pacific Spiny Dogfish" # too flat, but with depth change! note: pooled maturity 
# species <- "Longnose Skate" # do too flat, temp from 4 might be useful, seems to go deeper?
# species <- "Big Skate"
# species <- "Sandpaper Skate"
# species <- "Brown Cat Shark"
# species <- "Spotted Ratfish" # curves not useful because to abundant in shallow

# covariates <- "+muddy+any_rock"
# covariates <- ""
# covariates <- "+as.factor(ssid)"
# covariates <- "+trawled"
# covariates <- "+muddy+mixed+rocky"
# covariates <- "+mixed+rocky"
# covariates <- "+trawled+muddy+rocky+mixed"
# covariates <- "+trawled+mixed+rocky"
# covariates <- "+trawled+mixed"
# covs <- gsub("\\+", "-", covariates)
# covs <- "-both-tv-depth-ssid"
# covs <- "-both-tv-depth"
# covs <- "-log-do"
# covs <- "-log-do-ssid"
# covs <- "-log-both-trawled"