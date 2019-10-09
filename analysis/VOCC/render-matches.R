# Render matches for BACI

getwd()
# setwd(here::here("/analysis/VOCC"))
env <- new.env() #parent = baseenv()

list_species <- c(
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
  "Shortspine Thornyhead"
)

list_species <- c(
  # # "Walleye Pollock",
  # # "Pacific Cod",
  # # "Sablefish",
  # # "Lingcod",
  # # # "Pacific Hake",
  # # "North Pacific Spiny Dogfish",
  # # # "Spotted Ratfish",
  # # "Arrowtooth Flounder",
  # # "Petrale Sole",
  # # "English Sole",
  # # "Dover Sole",
  # # "Southern Rock Sole",
  # # "Flathead Sole",
  # # "Bocaccio", # winter-birthing, overfished
  # # "Canary Rockfish", # schooling, winter-birthing
  # # # "Copper Rockfish", # small sample
  # # # "Darkblotched Rockfish",
  # "Greenstriped Rockfish",
  # "Pacific Ocean Perch", # schooling
  # "Quillback Rockfish",
  # "Redbanded Rockfish",
  # "Rougheye/Blackspotted Rockfish Complex",
  # "Sharpchin Rockfish",
  # # "Shortbelly Rockfish", # small sample
  # "Silvergray Rockfish",
  # "Splitnose Rockfish",
  # "Widow Rockfish", # schooling
  # "Yellowtail Rockfish", # schooling
  # "Yelloweye Rockfish", # summer-birthing, overfished,
  # # "Longspine Thornyhead",
  # "Shortspine Thornyhead",
  "Rex Sole",
  "Curlfin Sole",
  "Sand Sole",
  "Slender Sole",
  "Pacific Sanddab",
  "Pacific Halibut",
  "Pacific Tomcod",
  "Rosethorn Rockfish",
  "Redstripe Rockfish",
  "Yellowmouth Rockfish",
  "Harlequin Rockfish"
)


list_regions <- c(
  "West Coast Vancouver Island",
  "West Coast Haida Gwaii",
  "both odd year surveys"
)

ages <- c("mature", "imm")

## DO matches

climate <- "do"
biomass_threshold <- "50"
threshold <- 0.25

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ),
          envir = env
        )
      })
    }
  }
}


climate <- "do"
biomass_threshold <- "50"
threshold <- 0.5

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ),
          envir = env
        )
      })
    }
  }
}


climate <- "do"
biomass_threshold <- "25"
threshold <- 0.25

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}


climate <- "do"
biomass_threshold <- "25"
threshold <- 0.5

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}



## Temperature matches
climate <- "temperature"
biomass_threshold <- "50"
threshold <- 0.25


for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}

#### start again here?
climate <- "temperature"
biomass_threshold <- "50"
threshold <- 0.5

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}


climate <- "temperature"
biomass_threshold <- "50"
threshold <- 0.75

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}


climate <- "temperature"
biomass_threshold <- "50"
threshold <- 1

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = c(1)
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}


climate <- "temperature"
biomass_threshold <- "25"
threshold <- 0.25


for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}


climate <- "temperature"
biomass_threshold <- "25"
threshold <- 0.5

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}

climate <- "temperature"
biomass_threshold <- "25"
threshold <- 0.75

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = biomass_threshold,
            climate = climate,
            threshold = threshold
          ),
          output_file = paste0(
            "html/matches/match", spp, "-",
            climate, "-", biomass_threshold, "-", threshold, ".html"
          ), envir = env
        )
      })
    }
  }
}
