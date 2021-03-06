% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-coefs.R
\name{plot_coefs}
\alias{plot_coefs}
\title{Plot coefficients from vocc_regression}
\usage{
plot_coefs(
  coloured_coefs,
  grouping_taxa = "species",
  order_by_trait = FALSE,
  order_by = "temp_vel",
  increasing = F,
  grid_facets = F,
  add_grey_bars = F,
  fixed_scales = TRUE
)
}
\arguments{
\item{coloured_coefs}{Coefficient dataframe with colours column.}

\item{order_by}{Coefficient by which to order species in plot.}

\item{manipulate}{Logical to allow manipulation in R studio.}
}
\description{
Plot coefficients from vocc_regression
}
