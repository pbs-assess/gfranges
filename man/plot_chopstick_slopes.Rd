% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-vocc-interaction.R
\name{plot_chopstick_slopes}
\alias{plot_chopstick_slopes}
\title{Plot slopes from chopstick plots}
\usage{
plot_chopstick_slopes(
  slopedat,
  type = NULL,
  x_variable = "temp_trend_scaled",
  legend_position = c(0.7, 0.95),
  hack = F,
  imm_slopes = NULL,
  add_global = T,
  global_col = "gray30",
  point_size = 0.75,
  alpha_range = c(0.4, 0.99),
  add_grey_bars = F,
  name_chop_type = T,
  order_by_chops = NULL,
  colours = NULL
)
}
\arguments{
\item{slopedat}{Df from chopstick_slopes function}

\item{type}{Select type if more than one interaction in model}

\item{x_variable}{Which variable on x-axis}

\item{legend_position}{Give legend position}

\item{hack}{Logical for comparing with hacked slope estimate w interaction SE}

\item{imm_slopes}{Add immature data}

\item{add_global}{Logical for including global slope estimates}

\item{global_col}{Set colour for global slopes}

\item{point_size}{Change point size}

\item{alpha_range}{Set alapha values based on slope significance}

\item{add_grey_bars}{Add grey stripes to alternating species}

\item{name_chop_type}{Add type to chopstick name in legend}

\item{colours}{Add custom colours}
}
\description{
Plot slopes from chopstick plots
}
