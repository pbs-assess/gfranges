#' Title
#'
#' @param model 
#' @param pred 
#' @param pred.values 
#' @param centered 
#' @param plot.points 
#' @param interval 
#' @param data 
#' @param at 
#' @param int.type 
#' @param int.width 
#' @param outcome.scale 
#' @param robust 
#' @param cluster 
#' @param vcov 
#' @param set.offset 
#' @param x.label 
#' @param y.label 
#' @param pred.labels 
#' @param main.title 
#' @param colors 
#' @param line.thickness 
#' @param point.size 
#' @param point.alpha 
#' @param point.color 
#' @param jitter 
#' @param rug 
#' @param rug.sides 
#' @param force.cat 
#' @param cat.geom 
#' @param cat.interval.geom 
#' @param cat.pred.point.size 
#' @param partial.residuals 
#' @param color.class 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
effect_plot <- function (model, pred, pred.values = NULL, centered = "all", 
  plot.points = FALSE, interval = FALSE, data = NULL, at = NULL, 
  int.type = c("confidence", "prediction"), int.width = 0.95, 
  outcome.scale = "response", robust = FALSE, cluster = NULL, 
  vcov = NULL, set.offset = 1, x.label = NULL, y.label = NULL, 
  pred.labels = NULL, main.title = NULL, colors = "black", 
  line.thickness = 1.1, point.size = 1.5, point.alpha = 0.6, 
  point.color = "black", point.color.var = NULL, 
  jitter = 0, rug = FALSE, rug.sides = "lb", 
  force.cat = FALSE, cat.geom = c("point", "line", "bar"), 
  cat.interval.geom = c("errorbar", "linerange"), cat.pred.point.size = 3.5, 
  partial.residuals = FALSE, color.class = colors,  ...) 
{
  # if (!require(Hmisc)) install.packages("Hmisc")
  if (!require(jtools)) install.packages("jtools")
  # library(Hmisc)
  library(jtools)
  pred <- quo_name(enexpr(pred))
  if ("interval" %nin% names(match.call())[-1] & !(is.numeric(jtools::get_data(model, 
    warn = FALSE)[[pred]]) & force.cat == FALSE)) {
    interval <- TRUE
  }
  if (force.cat == TRUE & is.null(pred.values)) {
    if (is.null(data)) {
      data <- jtools::get_data(model)
    }
    pred.values <- sort(unique(suppressMessages(data[[pred]])))
  }
  if (!all(color.class == colors)) 
    colors <- color.class
  pred_out <- jtools::make_predictions(model, pred = pred, pred.values = pred.values, 
    at = NULL, center = centered, interval = interval, int.type = int.type, 
    outcome.scale = outcome.scale, robust = robust, cluster = cluster, 
    vcov = vcov, set.offset = set.offset, return.orig.data = TRUE, 
    partial.residuals = partial.residuals, data = data, 
    ...)
  pm <- pred_out[[1]]
  d <- pred_out[[2]]
  
  if (is.numeric(d[[pred]]) & force.cat == FALSE) {
    plot_effect_continuous(predictions = pm, pred = pred, 
      plot.points = plot.points | partial.residuals, interval = interval, 
      data = d, x.label = x.label, y.label = y.label, 
      pred.labels = pred.labels, main.title = main.title, 
      colors = colors, line.thickness = line.thickness, 
      jitter = jitter, resp = get_response_name(model), 
      weights = get_weights(model, d)$weights_name, rug = rug, 
      rug.sides = rug.sides, point.size = point.size, 
      point.alpha = point.alpha, point.color = point.color, 
      point.color.var = point.color.var)
  }
  else {
    jtools:::plot_cat(predictions = pm, pred = pred, data = d, geom = cat.geom, 
      pred.values = pred.values, interval = interval, 
      plot.points = plot.points | partial.residuals, pred.labels = pred.labels, 
      x.label = x.label, y.label = y.label, main.title = main.title, 
      colors = colors, weights = get_weights(model, d)$weights_name, 
      resp = get_response_name(model), jitter = jitter, 
      interval.geom = cat.interval.geom, line.thickness = line.thickness, 
      point.size = point.size, pred.point.size = cat.pred.point.size, 
      point.alpha = point.alpha, point.color = point.color)
  }
}


#' Title
#'
#' @param predictions 
#' @param pred 
#' @param plot.points 
#' @param interval 
#' @param data 
#' @param x.label 
#' @param y.label 
#' @param pred.labels 
#' @param main.title 
#' @param colors 
#' @param line.thickness 
#' @param jitter 
#' @param resp 
#' @param weights 
#' @param rug 
#' @param rug.sides 
#' @param point.size 
#' @param point.alpha 
#' @param point.color 
#'
#' @return
#' @export
#'
#' @examples
plot_effect_continuous <- function (predictions, pred, 
  plot.points = FALSE, 
  interval = FALSE, 
  data = NULL, x.label = NULL, y.label = NULL, pred.labels = NULL, 
  main.title = NULL, colors = NULL, line.thickness = 1.1, 
  jitter = 0.1, resp = NULL, weights = NULL, rug = FALSE, 
  rug.sides = "b", point.size = 1, point.alpha = 0.6, point.color = "black", 
  point.color.var = point.color.var) 
{
  pm <- predictions
  d <- data
  if (is.null(x.label)) {
    x.label <- pred
  }
  if (is.null(y.label)) {
    y.label <- resp
  }
  pred <- sym(pred)
  resp <- sym(resp)
  if (!is.null(weights)) {
    weights <- sym(weights)
  }
  if (length(jitter) == 1) {
    jitter <- rep(jitter, 2)
  }
  p <- ggplot(pm, aes(x = !!pred, y = !!resp))
  
  if (plot.points == TRUE) {
    constants <- list(alpha = point.alpha)
    
    if (is.null(weights)) {
      constants$size <- point.size
    }
    
    if (is.null(point.color.var)) {
    constants$colour <- point.color
    
    p <- p + 
      layer(geom = "point", 
        data = d, stat = "identity", 
        inherit.aes = FALSE, show.legend = FALSE, 
        mapping = aes(x = !!pred, y = !!resp, size = !!weights), 
        position = position_jitter(width = jitter[1], 
          height = jitter[2]), params = constants) + 
      scale_size(range = c(1 * 
              point.size, 5 * point.size))
    } else {
      # browser()
      p <- p + 
        layer(geom = "point", 
          data = d, stat = "identity", 
          inherit.aes = FALSE, show.legend = FALSE, 
          mapping = aes(x = !!pred, y = !!resp, size = !!weights, colour = !!point.color.var), 
          position = position_jitter(width = jitter[1], 
            height = jitter[2]), params = constants) + 
        scale_size(range = c(1 * 
            point.size, 5 * point.size))
    }
  }
  
  p <- p + geom_path(size = line.thickness)
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes(ymin = !!sym("ymin"), 
      ymax = !!sym("ymax")), fill = point.color, alpha = 1/5, show.legend = FALSE)
  }
  if (rug == TRUE) {
    p <- p + geom_rug(data = d, mapping = aes(x = !!pred, 
      y = !!resp), alpha = 0.6, position = position_jitter(width = jitter[1]), 
      sides = rug.sides, inherit.aes = TRUE)
  }
  p <- p + theme_nice(legend.pos = "right")
  p <- p + labs(x = x.label, y = y.label)
  if (length(unique(d[[pred]])) == 2) {
    brks <- sort(unique(d[[pred]]), decreasing = FALSE)
    if (is.null(pred.labels)) {
      p <- p + scale_x_continuous(breaks = brks)
    }
    else {
      if (length(pred.labels) == 2) {
        p <- p + scale_x_continuous(breaks = brks, labels = pred.labels)
      }
      else {
        warning("pred.labels argument has the wrong length. It won't be used")
        p <- p + scale_x_continuous(breaks = brks)
      }
    }
  }
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }
  return(p)
}


#' Title
#'
#' @param p 
#' @param hjust 
#'
#' @return
#' @export
#'
#' @examples
align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}
