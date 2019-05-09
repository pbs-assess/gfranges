#' Quiver plots for ggplot2
#'
#' @description
#' Displays the direction and length of vectors on a graph.
#'
#' @inheritParams ggplot2::layer
#' @param center If \code{FALSE} (the default), the vector lines will start at the specified x and y. If \code{TRUE}, the arrows will be centered about x and y.
#' @param rescale If \code{FALSE} (the default), the vectors will not be rescaled. If \code{TRUE}, the vectors given by (u, v) will be rescaled using the \code{scale} function.
#' @param vecsize By default (NULL), vectors sizing is automatically determined. If a grid can be identified, they will be scaled to the grid, if not, the vectors will not be scaled. By specifying a numeric input here, the length of all arrows can be adjusted. Setting vecsize to zero will prevent scaling the arrows.
#'
#' @examples
#' library(ggplot2)
#' # Quiver plots of mathematical functions
#' expand.grid(x = seq(0, pi, pi / 12), y = seq(0, pi, pi / 12)) %>%
#'   ggplot(aes(x = x, y = y, u = cos(x), v = sin(y))) +
#'   geom_quiver()
#' 
#' # Removing automatic scaling
#' ggplot(seals, aes(x = long, y = lat, u = delta_long, v = delta_lat)) +
#'   geom_quiver(vecsize = NULL) +
#'   borders("state")
#' \dontrun{
#' # Centering arrows is useful for plotting on maps.
#' library(dplyr)
#' library(ggmap)
#' wind_data <- wind %>% filter(between(lon, -96, -93) & between(lat, 28.7, 30))
#' qmplot(lon, lat, data = wind_data, extent = "panel", geom = "blank", zoom = 8, maptype = "toner-lite") +
#'   geom_quiver(aes(u = delta_lon, v = delta_lat, colour = spd), center = TRUE)
#' }
#' 
#' @importFrom ggplot2 layer
#'
#' @export
geom_quiver <- function(mapping = NULL, data = NULL,
                        stat = "quiver", position = "identity",
                        center = FALSE,
                        rescale = FALSE,
                        vecsize = NULL,
                        #head_size = sqrt((x - xend)^2 + (y - yend)^2) * 0.5,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuiver,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      center = center,
      rescale = rescale,
      vecsize = vecsize,
      #head_size = head_size,
      ...
    )
  )
}

#' @rdname geom_quiver
#'
#' @export
GeomQuiver <- ggproto(
  "GeomQuiver", ggplot2::GeomSegment,
  draw_panel = function(
                          data,
                          panel_params,
                          coord,
                          arrow = NULL,
                          #head_size,
                          lineend = "butt",
                          na.rm = FALSE) {
    trans <- CoordCartesian$transform(data, panel_params) %>%
      mutate(arrowsize = 0.015) #sqrt((x - xend)^2 + (y - yend)^2) * 0.1 + 0.01)
    grid::segmentsGrob(
      trans$x, trans$y, trans$xend, trans$yend,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(trans$colour, trans$alpha),
        lwd = trans$size * .pt,
        lty = trans$linetype,
        lineend = lineend
      ),
      arrow = grid::arrow(
        length = grid::unit(trans$arrowsize, "npc"),
        ends = "last" #, type = "closed"
      )
    )
  }
)


#' @rdname geom_quiver
#' @inheritParams ggplot2::stat_identity
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @section Computed variables:
#' \describe{
#' \item{x}{centered x start position for velocity arrow}
#' \item{y}{centered y start position for velocity arrow}
#' \item{xend}{centered x end position for velocity arrow}
#' \item{yend}{centered y end position for velocity arrow}
#' }
#' @export
stat_quiver <- function(mapping = NULL, data = NULL,
                        geom = "quiver", position = "identity",
                        center = FALSE,
                        rescale = FALSE,
                        vecsize = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuiver,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      center = center,
      rescale = rescale,
      vecsize = vecsize,
      ...
    )
  )
}


#' @rdname geom_quiver
#'
#' @importFrom dplyr %>% filter mutate
#'
#' @export
StatQuiver <- ggplot2::ggproto(
  "StatQuiver", ggplot2::Stat,
  required_aes = c("u", "v"),

  compute_panel = function(self, data, scales, center = FALSE, rescale = FALSE, vecsize = NULL, na.rm = FALSE) {
    if (rescale) {
      data <- data %>%
        mutate(u = as.numeric(scale(u)), v = as.numeric(scale(v)))
    }
    gridpoints <- c(abs(diff(sort(unique(data$x)))), abs(diff(sort(unique(data$y)))))
    gridsize <- min(gridpoints, na.rm = TRUE)
    if (is.null(vecsize)) {
      # Detect if x and y form a grid
      vecsize <- if (length(unique(round(gridpoints, 2))) > 2) 0 else 1
    }
    center <- if (center) 0.5 else 0
    data %>%
      filter(u != 0 | v != 0) %>%
      mutate(
        veclength = sqrt(u^2 + v^2),
        vectorsize = if (vecsize == 0) {
          1
        } else {
          gridsize / max(veclength, na.rm = TRUE) * vecsize
        },
        xend = x + (1 - center) * u * vectorsize, yend = y + (1 - center) * v * vectorsize,
        x = x - center * u * vectorsize, y = y - center * v * vectorsize
      )
  }
)
