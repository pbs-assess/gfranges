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
  "Pacific Cod",
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
  "Pacific Ocean Perch", # schooling
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Sharpchin Rockfish",
  "Shortbelly Rockfish", # small sample
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowtail Rockfish", # schooling
   # "Longspine Thornyhead",
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
  "Pacific Halibut"
)

### SUBSETS OF SPECIES
 list_species <- c(
  "Pacific Ocean Perch", # schooling
  "Redbanded Rockfish",
  "Pacific Cod",
  "Pacific Halibut"
)


 
 list_species <- c(
   # "Redstripe Rockfish",
   "Rougheye/Blackspotted Rockfish Complex",
   # "Widow Rockfish",
   # "Quillback Rockfish",
   # "Bocaccio",
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

list_species <- c(
  "Bocaccio"
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
          knots = 300, # 500 for most 
          update_model = TRUE, # FALSE #
          update_predictions = FALSE, 
          update_model_check = FALSE 
        ),
        output_file = paste0(
          "html/biomass-by-depth/biomass-by",
          covs, "-", spp, "-300.html"
        ),
        envir = env
      )
    })
  }
}


### save gradients from models run before July 27 2020
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
          knots = 500,
          update_model = FALSE, #
          update_predictions = FALSE, 
          update_model_check = FALSE 
        ),
        output_file = paste0(
          "html/save-grad",
          covs, "-", spp, "-500.html"
        ),
        envir = env
      )
    })
  }
}



getwd()
setwd(here::here("/analysis/VOCC"))
env <- new.env() # parent = baseenv()

# all species
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
  # "Longspine Thornyhead",
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

### check biomass model convergence 
list_regions <- c("All synoptic surveys")
# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model covariates
    try({
      rmarkdown::render("2-check-biomass-grads.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs
        ),
        output_file = paste0(
          "html/check-grads-oct-2020",
          covs, "-", spp, ".html"
        ),
        envir = env
      )
    })
  }
}


### attempt to improve biomass model convergence 
list_regions <- c("All synoptic surveys")
# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model covariates
    try({
      rmarkdown::render("2-improve-biomass-models.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs
        ),
        output_file = paste0(
          "html/improve-grad",
          covs, "-", spp, ".html"
        ),
        envir = env
      )
    })
  }
}


# # double check these
# list_species <- c(
# "Shortbelly Rockfish", # removed as not converging
# # "Redstripe Rockfish"
#   "Bocaccio" # remove immature Bocaccio because recent explosion not modelable
# )

### rebuild predictions from better models
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
          knots = 300,
          update_model = FALSE,
          update_predictions = TRUE, 
          update_model_check = TRUE 
        ),
        output_file = paste0(
          "html/biomass-by-depth/biomass-by",
          covs, "-", spp, "-predict-only.html"
        ),
        envir = env
      )
    })
  }
}

# 
# ### rebuild imm and total models without AR1
# list_regions <- c("All synoptic surveys")
# # dir.create(file.path("html/biomass-by-depth"))
# for (r_h in seq_along(list_regions)) {
#   for (spp_i in seq_along(list_species)) {
#     spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
#     covs <- "-tv-depth-only" # string describing model covariates
#     # covs <- "-ssid-only
#     try({
#       rmarkdown::render("2-biomass-depth-only-model2.Rmd",
#         params = list(
#           species = list_species[spp_i],
#           region = list_regions[r_h],
#           covariates = "", # additional non-climate variables
#           covs = covs,
#           knots = 400,
#           update_model = TRUE,
#           update_predictions = TRUE, 
#           update_model_check = TRUE 
#         ),
#         output_file = paste0(
#           "html/biomass-by-depth/biomass-by",
#           covs, "-", spp, "-no-AR1-400.html"
#         ),
#         envir = env
#       )
#     })
#   }
# }
# 


list_species <- c(
  "Bocaccio"
)

list_regions <- c("All synoptic surveys")
# dir.create(file.path("html/biomass-by-depth"))
for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model covariates
    # covs <- "-ssid-only
    try({
      rmarkdown::render("2-biomass-depth-only-model2.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          covariates = "", # additional non-climate variables
          covs = covs,
          knots = 250,
          update_model = TRUE,
          update_predictions = TRUE, 
          update_model_check = TRUE 
        ),
        output_file = paste0(
          "html/biomass-by-depth/biomass-by",
          covs, "-", spp, "-no-AR1-250.html"
        ),
        envir = env
      )
    })
  }
}



#### check data used in all older models

checkdata <- list()
for (spp_i in seq_along(list_species)) {
    rm(m)
    rm(spp)
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    covs <- "-tv-depth-only" # string describing model covariates
    try({
      m <- readRDS(paste0("data/", spp,
        "/mod-mat-biomass-", spp, covs, "-1n3n4n16-prior-FALSE.rds"))
      # adult_biomass <- sdmTMB:::update_model(adult_biomass)
      checkdata[[spp_i]] <- data.frame(
        species = spp, 
        maturity = "mature", 
        rowcount = nrow(m$data), 
        start_year = min(unique(m$data$year))) 
    })
}
matdata <- do.call(rbind, checkdata) %>% filter(species != "shortbelly-rockfish")



checkimmdata <- list()
for (spp_i in seq_along(list_species)) {
  rm(m)
  rm(spp)
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
  covs <- "-tv-depth-only" # string describing model covariates
  try({
    m <- readRDS(paste0("data/", spp,
      "/mod-imm-biomass-", spp, covs, "-1n3n4n16.rds"))
    # adult_biomass <- sdmTMB:::update_model(adult_biomass)
    checkimmdata[[spp_i]] <- data.frame(
      species = spp, 
      maturity = "immature", 
      rowcount = nrow(m$data), 
      start_year = min(unique(m$data$year))) 
  })
}

immdata <- do.call(rbind, checkimmdata)

checktdata <- list()
for (spp_i in seq_along(list_species)) {
  rm(m)
  rm(spp)
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
  covs <- "-tv-depth-only" # string describing model covariates
  try({
    m <- readRDS(paste0("data/", spp,
      "/model-total-biomass-", spp, covs, "-1n3n4n16.rds"))
    # adult_biomass <- sdmTMB:::update_model(adult_biomass)
    checktdata[[spp_i]] <- data.frame(
      species = spp, 
      maturity = "combind", 
      rowcount = nrow(m$data), 
      start_year = min(unique(m$data$year))) 
  })
}

totdata <- do.call(rbind, checktdata)


m1 <- readRDS(paste0("data/canary-rockfish/mod-mat-biomass-canary-rockfish-tv-depth-only-1n3n4n16-prior-FALSE.rds"))
View(m1$data)
