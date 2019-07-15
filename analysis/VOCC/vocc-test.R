start_data <- list(
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), do_est = c(1, 1, 1, 1)),
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), temp = c(1, 1, 1, 1))
)

end_data <- list(
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), do_est = c(0.45, 1, 1, 10)),
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), temp = c(1, 1.45, 1, 1))
)


dist_based_vocc(
  start_data = start_data,
  end_data = end_data,
  x = "x",
  y = "y",
  variable_names = c("do_est", "temp"), # what the layer within each element is called
  thresholds = c(0.5, 0.5),
  max_thresholds = c(Inf, Inf),
  #match_logic = c("==", "=="), # will impliment rounding and max_thresholds will be ignored
  cell_size = 1,
  delta_t = 1,
  raster = FALSE
)
