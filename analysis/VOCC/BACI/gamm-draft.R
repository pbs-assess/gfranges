
mi <- mgcv::gamm(log_density~ as.factor(after) + cell_type + after:cell_type + s(std_temp) + s(distance) + s(as.factor(idata$matchobs), bs="re")
  + s(X,Y, bs="re"), data=idata)
summary(mi)


mi <- mgcv::gamm(log_density~ as.factor(after) * cell_type + s(std_temp) + s(distance) + s(as.factor(idata$matchobs), bs="re"), data=idata) 

+ s(X,Y, bs="re")



### REMOVED DOCUMENTATION
# @import mgcv
# @import stats
# @import grDevices
# @import graphics
# @import plotfunctions

#' @description Plots a smooth from a \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}} model based on predictions.
#' In contrast with the default \code{\link[mgcv]{plot.gam}}, this function
#' plots the summed effects and optionally removes the random effects.
#' 
#' @param x A gam object, produced by \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
#' @param pred A named list of the values to use for the predictor terms
#' to plot.
#' @param cond A named list of the values to use for the other predictor terms
#' (not in view). Used for choosing between smooths that share the same view
#' predictors.
#' @param parametricOnly Logical: whether or not to cancel out all smooth
#' terms and only use the predictors in the parametric summary.
#' @param rm.ranef Logical: whether or not to remove random effects.
#' Default is TRUE.
#' @param col The colors for the lines and the error bars of the plot.
#' @param se If less than or equal to zero then only the predicted surface is
#' plotted, but if greater than zero, then the predicted values plus
#' confidence intervals are plotted. The value of se will be multiplied with
#' the standard error (i.e., 1.96 results in 95\%CI and 2.58).
#' @param print.summary Logical: whether or not to print summary.
#' Default set to the print info messages option
#' (see \code{\link{infoMessages}}).
#' @param main Changing the main title for the plot, see also title.
#' @param xlab Changing the label for the x axis,
#' defaults to a description of x.
#' @param ... other options to pass on to \code{\link{dotplot_error}},
#' see \code{\link[graphics]{par}}
#' @section Warning:
#' Use \code{parametricOnly} with care! When set to TRUE, all smooth
#' predictors are set to 0. Note that this might result in strange
#' predictions, because a value of 0 does not always represents a realistic
#' situation (e.g., body temperature of 0 is highly unlikely).
#' Note that linear slopes are not set to zero, because they are
#' considered as parametric terms. If \code{cond} does not specify a value for
#' these continuous predictors, the closes value to the mean is automatically
#' selected.

