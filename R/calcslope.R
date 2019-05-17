#' Calculate trend in varible for pixels in a raster brick
#'
#' @param rx Raster brick containing layers for multiple years.
#' @param delta_t Number of time steps between layers, if not 1 allows slope to be calculated for a single unit of time.
#' @param na.rm Logical for if NAs are removed.
#'
#' @export
calcslope <- function(rx, delta_t_step = 2, na.rm = TRUE) {

  # this function is a modified version of Chris Brown's (https://github.com/cbrown5/vocc/blob/master/R/calcslope.R)
  # gives same result in intial tests, but may be slower
  # use vocc::calcslope from devtools::install_github("seananderson/vocc") to apply his method instead

  icell <- seq(1, raster::ncell(rx))
  coord <- raster::xyFromCell(rx, icell)
  y <- t(raster::getValues(rx))
  t <- row(y) # matrix of times for all cells ... assumes equal time between slices
  # FIXME: Can we make this robust to uneven timesteps?
  # browser()
  x1 <- y
  x1[!is.na(x1)] <- 1 # matrix of 1s for cells with y data
  N <- apply(x1, 2, sum, na.rm = na.rm) # total time slices for each cell ("2" means by column)
  x <- (t - 1) * delta_t_step * x1 # t * x1 if sampled every year = matrix with NaN for squares and times without data
  # sumx <- apply(x, 2, sum, na.rm = na.rm)
  meanx <- apply(x, 2, sum, na.rm = na.rm) / N # mean time for each cell
  # subtract meanx values (from vector) from each value in the corresponding column
  deltax <- sweep(x, 2, meanx, `-`)
  # sumy <- apply(y, 2, sum, na.rm = na.rm)
  meany <- apply(y, 2, sum, na.rm = na.rm) / N # mean y for each cell
  # subtract meany values (from vector) from each value in the corresponding column
  deltay <- sweep(y, 2, meany, `-`)
  xy <- deltax * deltay # matrix of values for numerator
  numerator <- apply(xy, 2, sum, na.rm = na.rm)
  deltax2 <- deltax^2 # matrix of values for denominator
  denom <- apply(deltax2, 2, sum, na.rm = na.rm)
  slope <- numerator / denom
  data.frame(slope = slope, N = N, time_step = delta_t_step, coord, icell)
}
