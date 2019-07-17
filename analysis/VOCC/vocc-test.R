start_data <- list(
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4),   do = c(1.50, 1.01, 1.01, 1.01)),
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), temp = c(1.01, 1.01, 1.01, 1.01))
)

end_data <- list(
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4),   do = c(0.55, 1.00, 1.00, 10.0)),
  data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4), temp = c(1.00, 1.45, 1.00, 1.00))
)


# using rounding and match_logic a difference of 1.5 - 0.55 is identified as > 1.0
dist_based_vocc(
  start_data = start_data,
  end_data = end_data,
  x = "x",
  y = "y",
  variable_names = c("do", "temp"), # what the layer within each element is called
  plus_minus = c(0.5, 0.5), # vector of plus/minus threshold(s) to define climate match
  match_logic = c("==", "=="), # will impliment rounding and max_thresholds will be ignored
  cell_size = 1,
  delta_t = 1,
  raster = FALSE
)


# testing with flexible thresholds but same symmetrical values as above
dist_based_vocc(
  start_data = start_data,
  end_data = end_data,
  x = "x",
  y = "y",
  variable_names = c("do", "temp"),
  round_fact = 10,
  min_thresholds = c(1, 1), # vectors of actual lower threshold values plus_minus*2
  max_thresholds = c(1, 1), # vectors of actual higher threshold values plus_minus*2
  cell_size = 1,
  delta_t = 1,
  raster = FALSE
)


# testing asymmetrical thresholds
dist_based_vocc(
  start_data = start_data,
  end_data = end_data,
  x = "x",
  y = "y",
  variable_names = c("do", "temp"),
  round_fact = 10,
  min_thresholds = c(1, 1),
  max_thresholds = c(Inf, 1),
  cell_size = 1,
  delta_t = 1,
  raster = FALSE
)
