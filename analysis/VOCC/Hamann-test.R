# EXAMPLE DATA FROM Hamann et al. 2015
# install.packages("SDMTools")     # install package to read and write ESRI ASCII grids

var1_s <- SDMTools::asc2dataframe("eg_data/PC1-6190.asc")# principal component grids
var2_s <- SDMTools::asc2dataframe("eg_data/PC2-6190.asc")
var1_e  <- SDMTools::asc2dataframe("eg_data/PC1-2020s.asc")
var2_e  <- SDMTools::asc2dataframe("eg_data/PC2-2020s.asc")

start_data2 <- list(var1 = var1_s, var2 = var2_s)
end_data2 <- list(var1 = var1_e, var2 = var2_e)


library(dplyr)
glimpse(start_data)

out <- dist_based_vocc(
  start_data = start_data2,
  end_data = end_data2,
  x = "x",
  y = "y",
  variable_names = c("var.1", "var.1"),
  thresholds = c(0.13, 0.13),
  cell_size = 1,
  max_dist = 10000,
  delta_t = 50,
  raster = FALSE,
  kNN = TRUE
)

# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.


hist(log10(out$distance))
hist(log10(out$speed))

