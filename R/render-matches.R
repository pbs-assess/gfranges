
getwd()
# setwd(here::here("/analysis/VOCC"))

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

list_regions <- c(
  "West Coast Vancouver Island",
  "West Coast Haida Gwaii",
  "both odd year surveys"
)

ages <- c("mature", "imm")

## DO matches

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "do",
            threshold = c(0.25)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "do",
            threshold = c(0.5)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "25",
            climate = "do",
            threshold = c(0.25)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "25",
            climate = "do",
            threshold = c(0.5)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

## Temperature matches


for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "temperature",
            threshold = c(0.25)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "temperature",
            threshold = c(0.5)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "temperature",
            threshold = c(0.75)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
      )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "50",
            climate = "temperature",
            threshold = c(1)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
      )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "25",
            climate = "temperature",
            threshold = c(0.25)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "25",
            climate = "temperature",
            threshold = c(0.5)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
      )
      })
    }
  }
}

for (a in seq_along(ages)) {
  for (r_h in seq_along(list_regions)) {
    for (spp_i in seq_along(list_species)) {
      try({
        rmarkdown::render("4-vocc-BACI-multiyear.Rmd",
          params = list(
            species = list_species[spp_i],
            age = ages[a],
            region = list_regions[r_h],
            biomass_threshold = "25",
            climate = "temperature",
            threshold = c(0.75)
          ),
          output_file = paste0("html/match", list_species[spp_i], climate, biomass_threshold, threshold, ".html")
        )
      })
    }
  }
}


