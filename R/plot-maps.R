#' Plot climate change vectors
#'
#' @param df Dataframe produced by dist_based_vocc function.
#' @param fill_col Vector name for colouring raster grid cells.
#' @param fill_label Label for legend for raster grid cell colour.
#' @param raster_alpha Raster transparency.
#' @param vec_aes Vector name for any plotting variation.
#' @param vec_lwd_range Range in vector line widths.
#' @param vec_alpha Vector transparency.
#' @param max_vec_plotted Upper limit for vector lengths.
#'   Cells with no match less than this distance will be labeled with [NA_label].
#' @param min_vec_plotted Lower limit for vector lengths actually plotted.
#' @param NA_label Symbol used to indicate cells with no analog. Default is "NA".
#' @param low_fill Colour of negative values if raster values span zero.
#' @param mid_fill Colour of zero value raster cells.
#' @param high_fill Colour of positive values if raster values span zero.
#' @param vec_col Colour of vectors.
#' @param coast Coast polygons where (x = "X", y = "Y", group = "PID").
#'    If NULL, will attempt to create them for xy values in df.
#' @param contours Polygons of contour lines where (x = "X", y = "Y", group = "paste(PID, SID)").
#'    If NULL, will attempt to create bathymetry layer for xy values in df using gfplot.
#' @param arrowhead_size Changes head size for custom geom_quiver function.
#' @param axis_lables Logical for inclusion of axis labels.
#' @param viridis_option Change between viridis colormap options available in ggplot.
#' @param viridis_dir Option to flip scale by giving value of -1.
#' @param transform_col Apply transformation to colour scale.
#'    Accepts standard options (e.g. "sqrt") or unquoted custom transformations
#'    defined using scales::trans_new (e.g. fourth_root_power).
#'    Default is to apply no transformation (no_trans).
#' @param white_zero If TRUE, will always plot on custom fill scale.
#'    Default will plot on this scale only if raster has negative values.
#' @param raster_limits Range of values to plot; those in excess will be red. Default of "NULL" plots full range.
#' @param legend_position Vector of coordinates for legend placement. Or "none" to remove legend.
#' @param raster_cell_size Raster cell width. Used to centre NA_label.
#'
#' @export
#'
plot_vocc <- function(df,
                      fill_col = NULL,
                      fill_label = NULL,
                      raster_alpha = 1,
                      vec_aes = "distance",
                      arrowhead_size = 0.015,
                      vec_lwd_range = c(0.7, 0.8),
                      vec_alpha = 1,
                      max_vec_plotted = max(df$distance),
                      min_vec_plotted = 4,
                      NA_label = "NA",
                      white_zero = min(fill, na.rm = TRUE) < 0,
                      low_fill = "Steel Blue 4",
                      mid_fill = "white",
                      high_fill = "Red 3",
                      vec_col = "grey37",
                      coast = NULL,
                      contours = NULL,
                      axis_lables = FALSE,
                      viridis_option = "D",
                      viridis_dir = 1,
                      transform_col = no_trans,
                      raster_limits = NULL,
                      raster_cell_size = 2,
                      legend_position = c(0.2, 0.25)) {

  if (!is.null(vec_aes)) {
  # order so smaller vectors are on top?
  df <- df[order(df$distance), ]
  df[df$distance < min_vec_plotted, ]$target_X <- NA
  df[df$distance < min_vec_plotted, ]$target_Y <- NA
  if (max(df$distance) > max_vec_plotted) {
    df[df$distance > max_vec_plotted, ]$target_X <- NA
    df[df$distance > max_vec_plotted, ]$target_Y <- NA
  }
  }
  
  # Set plot boundaries so that dimensions are close to square
  width_X <- max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE)
  width_Y <- max(df$y, na.rm = TRUE) - min(df$y, na.rm = TRUE)
  diffxy <- width_X - width_Y
  if (diffxy < -6) {
    buffer_X <- c(-(abs(diffxy) / 2), abs(diffxy) / 2)
  } else {
    buffer_X <- c(-3, 3)
  }
  if (diffxy > 6) {
    buffer_Y <- c(-(abs(diffxy) / 2), abs(diffxy) / 2)
  } else {
    buffer_Y <- c(-3, 3)
  }


  if (isFALSE(axis_lables)) {
    gvocc <- ggplot2::ggplot(df, aes(x, y)) +
      coord_fixed(xlim = range(df$x) + buffer_X, ylim = range(df$y) + buffer_Y) +
      gfplot::theme_pbs() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  } else {
    gvocc <- ggplot2::ggplot(df, aes(x, y)) +
      coord_fixed(xlim = range(df$x) + buffer, ylim = range(df$y) + c(-3, 3)) +
      gfplot::theme_pbs() + xlab("UTM") + ylab("UTM")
  }


  #### Add fill ####
  if (!is.null(fill_col)) {
    fill <- df[[fill_col]]

    breaks <- scales::trans_breaks(transform_col[["transform"]],
      transform_col[["inverse"]],
      n = 6
    )


    labels <- function(x) {
      format(x, digits = 2, scientific = FALSE)
    }

    if (white_zero) {
      gvocc <- gvocc +
        geom_tile(aes(fill = fill), alpha = raster_alpha) +
        scale_fill_gradient2(
          low = low_fill, mid = mid_fill, high = high_fill,
          trans = transform_col, breaks = breaks, labels = labels,
          limits = raster_limits
        ) +
        labs(fill = fill_label) +
        theme(legend.position = legend_position)
    } else {
      gvocc <- gvocc +
        geom_tile(aes(fill = fill), alpha = raster_alpha) +
        scale_fill_viridis_c(
          direction = viridis_dir,
          option = viridis_option, na.value = "red",
          trans = transform_col, breaks = breaks, labels = labels, 
          limits = raster_limits
        ) +
        labs(fill = fill_label) +
        theme(legend.position = legend_position)
    }
  }

  #### Add bathymetry ####
  if (!is.null(contours)) {
    # add premade contour layers (will all be same colour and must be in utms)
    gvocc <- gvocc +
      geom_path(
        data = contours,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)"
        ),
        inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey55"
      )
  } else {
    # add bathymetry layer from gfplot
    try({
      # convert coordinate data to lat lon
      df <- df %>%
        dplyr::mutate(X = x, Y = y) %>%
        gfplot:::utm2ll(., utm_zone = 9)

      # select bathymetry lines (output in utm) for area defined in lat lon
      isobath <- gfplot:::load_isobath(
        range(df$X) + c(-5, 5),
        range(df$Y) + c(-5, 5),
        bath = c(100, 200, 300, 400, 500),
        utm_zone = 9
      )
      isobath$PID2 <- as.factor(isobath$PID)

      # add lines to plot
      # TODO: should functionize so that colours change automatically with contour PID
      gvocc <- gvocc +
        geom_path(
          data = isobath[isobath$PID == 100, ],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.2, alpha = 0.4, colour = "grey85"
        ) +
        geom_path(
          data = isobath[isobath$PID == 200, ],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.2, alpha = 0.4, colour = "grey70"
        ) +
        geom_path(
          data = isobath[isobath$PID == 300, ],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.2, alpha = 0.4, colour = "grey55"
        ) +
        geom_path(
          data = isobath[isobath$PID == 400, ],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.2, alpha = 0.4, colour = "grey40"
        ) +
        geom_path(
          data = isobath[isobath$PID == 500, ],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.2, alpha = 0.4, colour = "grey30"
        )

      gvocc
    }, silent = TRUE)
  }


  #### Add coast ####
  if (!is.null(coast)) {
    # add premade coast polygons layer (must be in utms)
    gvocc <- gvocc +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2
      )
  } else {
    # add coast polygons from gfplot
    try({
      # convert coordinate data to lat lon
      df <- df %>%
        dplyr::mutate(X = x, Y = y) %>%
        gfplot:::utm2ll(., utm_zone = 9)

      # creates coast lines for area defined in lat lon
      coast <- gfplot:::load_coastline(
        range(df$X) + c(-1, 1),
        range(df$Y) + c(-1, 1),
        utm_zone = 9
      )

      # add polygons to plot
      gvocc <- gvocc +
        geom_polygon(
          data = coast, aes_string(x = "X", y = "Y", group = "PID"),
          fill = "grey87", col = "grey70", lwd = 0.2
        )
      gvocc
    }, silent = TRUE)
  }


  ####  Add arrows indicating target cells ####
  if (!is.null(vec_aes)) {
    vector <- 0.25 #as.vector(na.omit(df[[vec_aes]]))

    gvocc <- gvocc +
      geom_quiver(aes(x, y,
        u = target_X - x, v = target_Y - y,
        size = vector
      ),
      colour = vec_col, vecsize = 0,
      arrowhead_size = arrowhead_size,
      alpha = vec_alpha,
      inherit.aes = FALSE
      ) +
      scale_size_continuous(range = vec_lwd_range)


    # add NAs to plot where grid cells have no target cell within the distance range of 'max_vec_plotted'

    gvocc <- gvocc +
      guides(colour = "none", size = "none") +
      geom_text(
        data = df[df$distance > max_vec_plotted, ], 
        aes(x = x , y = y + raster_cell_size/2), inherit.aes = FALSE,
        size = 2, colour = vec_col,
        alpha = 0.75, label = NA_label
      )
   
  }

  gvocc
}


#' Raster map with year facets
#'
#' @param df Dataframe.
#' @param column Name column to be plotted.
#'
#' @export
plot_facet_map <- function(df, column = "est",
                           X = "X", Y = "Y",
                           viridis_option = "C",
                           transform_col = no_trans,
                           raster_limits = NULL,
                           legend_position = "right") {
  breaks <- scales::trans_breaks(transform_col[["transform"]],
    transform_col[["inverse"]],
    n = 5
  )

  labels <- function(x) {
    format(x, digits = 1)
  }

  width_X <- max(df$X, na.rm = TRUE) - min(df$X, na.rm = TRUE)
  width_Y <- max(df$Y, na.rm = TRUE) - min(df$Y, na.rm = TRUE)
  diffxy <- width_X - width_Y

  if (diffxy < -6) {
    buffer_X <- c(-(abs(diffxy) / 2), abs(diffxy) / 2)
  } else {
    buffer_X <- c(-3, 3)
  }
  if (diffxy > 6) {
    buffer_Y <- c(-(abs(diffxy) / 2), abs(diffxy) / 2)
  } else {
    buffer_Y <- c(-3, 3)
  }

  anno <- data.frame(year = unique(df$year))
  anno$x <- max(df$X) - (width_X * 0.1)
  anno$y <- max(df$Y) - (width_Y * 0.1)

  gfacet <- ggplot(df, aes_string(X, Y, fill = column)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = viridis_option, na.value = "red",
      trans = transform_col, breaks = breaks, labels = labels, limits = raster_limits
    ) +
    facet_wrap(~year) +
    coord_fixed(
      xlim = range(df$X) + buffer_X,
      ylim = range(df$Y) + buffer_Y
    ) +
    gfplot::theme_pbs() +
    theme(
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank(), 
      axis.ticks = element_blank(),
      legend.position = legend_position, strip.text = element_blank()
    )

  # convert coordinate data to lat lon
  df <- df %>%
    dplyr::mutate(X = X, Y = Y) %>%
    gfplot:::utm2ll(., utm_zone = 9)

  #### Add bathymetry ####
  # add bathymetry layer from gfplot
  # select bathymetry lines (output in utm) for area defined in lat lon
  isobath <- gfplot:::load_isobath(
    range(df$X) + c(-5, 5),
    range(df$Y) + c(-5, 5),
    bath = c(100, 200, 300, 400, 500),
    utm_zone = 9
  )
  isobath$PID2 <- as.factor(isobath$PID)

  # add lines to plot
  # TODO: should functionize so that colours change automatically with contour PID
  gfacet <- gfacet +
    geom_path(
      data = isobath[isobath$PID == 100, ],
      aes_string(
        x = X, y = Y,
        group = "paste(PID, SID)"
      ),
      inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey85"
    ) +
    geom_path(
      data = isobath[isobath$PID == 200, ],
      aes_string(
        x = X, y = Y,
        group = "paste(PID, SID)"
      ),
      inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey70"
    ) +
    geom_path(
      data = isobath[isobath$PID == 300, ],
      aes_string(
        x = X, y = Y,
        group = "paste(PID, SID)"
      ),
      inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey55"
    ) +
    geom_path(
      data = isobath[isobath$PID == 400, ],
      aes_string(
        x = X, y = Y,
        group = "paste(PID, SID)"
      ),
      inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey40"
    ) +
    geom_path(
      data = isobath[isobath$PID == 500, ],
      aes_string(
        x = X, y = Y,
        group = "paste(PID, SID)"
      ),
      inherit.aes = FALSE, lwd = 0.1, alpha = 0.4, colour = "grey30"
    )

  #### Add coast ####
  # creates coast lines for area defined in lat lon
  coast <- gfplot:::load_coastline(
    range(df$X) + c(-1, 1),
    range(df$Y) + c(-1, 1),
    utm_zone = 9
  )

  # add polygons to plot
  gfacet <- gfacet +
    geom_polygon(
      data = coast, aes_string(x = X, y = Y, group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2
    )

  # add year labels
  # convert coordinate data back to utms

  gfacet <- gfacet +
    geom_text(
      data = anno, aes(label = year, x = x, y = y),
      size = 2, col = "grey40", inherit.aes = FALSE
    )
  gfacet
}


#' @export
fourth_root_power <- scales::trans_new(
  name = "fourth root power",
  transform = function(x) ifelse(x > 0, x^0.25, -(-x)^0.25),
  inverse = function(x) ifelse(x > 0, x^4, -(-x)^4),
  domain = c(Inf, Inf)
)

#' @export
no_trans <- scales::trans_new(
  name = "no trans",
  transform = function(x) x,
  inverse = function(x) x,
  domain = c(Inf, Inf)
)

#' @export
sqrt <- scales::trans_new(
  name = "sqrt",
  transform = function(x) ifelse(x > 0, sqrt(x), -sqrt(-x)),
  inverse = function(x) ifelse(x > 0, x^2, -(x^2)),
  domain = c(Inf, Inf)
)

#' @export
log10 <- scales::trans_new(
  name = "log10",
  transform = function(x) ifelse(x > 0, log10(x), -log10(-x)),
  inverse = function(x) ifelse(x > 0, 10^x, -(10^x)),
  domain = c(Inf, Inf)
)