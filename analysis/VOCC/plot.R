# PLOT CLIMATE CHANGE VECTORS
plot_vocc <- function(df,
                      vec_col = "C_per_decade",
                      fill_col = "C_per_decade",
                      fill_label = "Local\nclimate trend\n(°C/decade)",
                      raster_alpha = 1,
                      vec_lwd = "distance",
                      vec_lwd_range = c(1, 2),
                      vec_alpha = 1,
                      max_vec_plotted = 100,
                      low_col = "Steel Blue 4",
                      mid_col = "white",
                      high_col = "Red 3",
                      coast = NULL,
                      isobath = NULL) {


  df <- df[order(-df$distance), ] # order so smaller vectors are on top?

  if (max(df$distance) > max_vec_plotted) {
    df[df$distance > max_vec_plotted, ]$target_X <- NA
    df[df$distance > max_vec_plotted, ]$target_Y <- NA
  }

  colour <- df[[vec_col]]
  fill <- df[[fill_col]]
  lwd <- df[[vec_lwd]]

  # # Parameters required by theme_pbs (in case changes are needed locally)
  # base_size <- 11
  # base_family <- ""
  # text_col <- "grey20"
  # panel_border_col <- "grey70"
  # half_line <- base_size / 2

  gvocc <- ggplot2::ggplot(df, aes(x, y)) +
    geom_raster(aes(fill = fill), alpha = raster_alpha) +
    scale_fill_viridis_c(trans = "sqrt") +
    guides(colour = "none", size = "none") +
    labs(fill = fill_label) + # , colour = col_label
    xlab("UTM") + ylab("UTM") +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs()
  # # This is what theme_pbs does (in case changes are needed locally)
  # theme_light(base_size = base_size, base_family = "") +
  # theme(
  #   legend.position = c(0.2, 0.05),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.ticks.length = unit(half_line / 2.2, "pt"),
  #   strip.background = element_rect(fill = NA, colour = NA),
  #   strip.text.x = element_text(colour = text_col),
  #   strip.text.y = element_text(colour = text_col),
  #   axis.text = element_text(colour = text_col),
  #   axis.title = element_text(colour = text_col),
  #   legend.title = element_text(colour = text_col, size = rel(0.9)),
  #   panel.border = element_rect(fill = NA, colour = panel_border_col, size = 1),
  #   legend.key.size = unit(0.9, "lines"),
  #   legend.text = element_text(size = rel(0.7), colour = text_col),
  #   legend.key = element_rect(colour = NA, fill = NA),
  #   legend.background = element_rect(colour = NA, fill = NA),
  #   plot.title = element_text(colour = text_col, size = rel(1)),
  #   plot.subtitle = element_text(colour = text_col, size = rel(.85))
  # )

  gvocc <- gvocc +
    ggnewscale::new_scale_color() +
    geom_quiver(aes(x, y, # call modified internal function, head_size = distance/10,
      # ggquiver::geom_quiver(aes(x, y, # to call published package version
      u = target_X - x, v = target_Y - y,
      size = lwd,
      colour = colour
    ), vecsize = 0, alpha = vec_alpha, inherit.aes = FALSE) +
    scale_size_continuous(range = vec_lwd_range) +
    scale_colour_gradient2(low = low_col, mid = mid_col, high = high_col)

  
  # Add bathymetry

  if (!is.null(isobath)) {
    # add premade bathymetry layer (must be in utms)
    gvocc <- gvocc +
      ggnewscale::new_scale_color() +
      geom_path(
        data = isobath,
        aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)", colour = "PID"
        ),
        inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      ) +
      scale_colour_continuous(low = "grey80", high = "grey10")
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

      # add line to plot
      gvocc <- gvocc +
        ggnewscale::new_scale_color() +
        geom_path(
          data = isobath,
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)", colour = "PID"
          ),
          inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
        ) +
        scale_colour_continuous(low = "grey80", high = "grey10")

      gvocc
    }, silent = TRUE)
  }


  # Add coast

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

  # add NAs to plot where grid cells have no target cell within the distance range of 'max_vec_plotted'
  gvocc <- gvocc + geom_text(
    data = df[df$distance > max_vec_plotted, ], aes(x, y), inherit.aes = FALSE,
    size = 4, colour = "grey99", 
    alpha = 0.75, label = "NA"
  )
  gvocc
}
