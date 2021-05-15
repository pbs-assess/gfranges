
# select time-varying deoth plots for Sean (May 2021)
library(gfranges)
library(tidyverse)
library(patchwork)


plot_tv_depth <- function(
  species, 
  covs = "-tv-depth-only"
){
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  rm(adult_biomass)
  
  try({
    adult_biomass <- readRDS(paste0(
      "data/", spp,
      "/mod-mat-biomass-", spp, covs, "-1n3n4n16-new.rds"
    ))
  })
  
  if (!exists("adult_biomass")) {
    try({
      adult_biomass <- readRDS(paste0(
        "data/", spp,
        "/model-total-biomass-", spp, covs, "-1n3n4n16.rds"
      ))
      # adult_biomass<-sdmTMB:::update_model(adult_biomass)
    })
  }
  
  d <- time_varying_density(adult_biomass, predictor = "depth")
  d$x <- exp(d$x)
  p <- plot_mountains(d,
    variable_label = "Depth", 
    peaklines = F,
    mountains_only = F,
    xlimits = c(0, 605)) +
    ggtitle(paste(species))
  p
}


(p1 <- plot_tv_depth("Spotted Ratfish")+ 
    labs(tag = "a."))
(p2 <- plot_tv_depth("North Pacific Spiny Dogfish")+ 
    labs(tag = "b."))
(p3 <- plot_tv_depth("Lingcod")+ 
    labs(tag = "c."))
(p4 <- plot_tv_depth("Greenstriped Rockfish")+ 
    labs(tag = "d."))

# make y axis label
p0 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

# remove some axis labels
p1$labels$y <- p2$labels$y <- p3$labels$y <- p4$labels$y <- " "
p1$labels$x <- p2$labels$x <- " "

# p0 + p1 + p2 + p3 + p4 + 
p0 + (p1 | p2) / (p3 | p4) +
  plot_layout(
    widths = c(1, 40),
    # design = layout,
    # ncol = 3,
    guides = 'collect')&
  theme(plot.tag.position = c(.03, .95),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

ggsave("time-vary-depths.png")  
