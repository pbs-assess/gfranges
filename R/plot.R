# PLOT CLIMATE CHANGE VECTORS
plot_vocc <- function(df,
                      fill_col = "C_per_decade",
                      fill_label = "Local\nclimate trend\n(°C/decade)",
                      raster_alpha = 1,
                      vec_aes = "distance",
                      vec_lwd_range = c(1, 1.5),
                      vec_alpha = 1,
                      max_vec_plotted = 100,
                      low_col = "Steel Blue 4",
                      mid_col = "white",
                      high_col = "Red 3",
                      coast = NULL,
                      isobath = NULL) {


  df <- df[order(-df$distance), ] # order so smaller vectors are on top?
  fill <- df[[fill_col]]

  if (min(fill, na.rm = TRUE)>0) {
  gvocc <- ggplot2::ggplot(df, aes(x, y)) +
    geom_raster(aes(fill = fill), alpha = raster_alpha) +
    scale_fill_viridis_c(trans = "sqrt") +
    labs(fill = fill_label) + 
    xlab("UTM") + ylab("UTM") +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs() +
    theme(legend.position = c(0.17, 0.17))

  } else {
  gvocc <- ggplot2::ggplot(df, aes(x, y)) +
    geom_raster(aes(fill = fill), alpha = raster_alpha) +
    scale_fill_gradient2(low = low_col, mid = mid_col, high = high_col) +
    labs(fill = fill_label) + 
    xlab("UTM") + ylab("UTM") +
    coord_fixed(xlim = range(df$x) + c(-3, 3), ylim = range(df$y) + c(-3, 3)) +
    gfplot::theme_pbs() +
    theme(legend.position = c(0.17, 0.17))
  }
  
#browser()
  # Add bathymetry

  if (!is.null(isobath)) {
    # add premade bathymetry layer (must be in utms)
    gvocc <- gvocc +
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
     # browser()
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

      # add line to plot
      gvocc <- gvocc +
        geom_path(
          data = isobath[[PID==100]],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.4, alpha = 0.4, colour = "grey80"
        ) + 
        geom_path(
          data = isobath[[PID==200]],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.4, alpha = 0.4, colour = "grey65"
        ) + 
        geom_path(
          data = isobath[[PID==300]],
          aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.4, alpha = 0.4, colour = "grey50"
        ) 
      # + 
      #   geom_path(
      #     data = isobath,
      #     aes_string(
      #       x = "X", y = "Y",
      #       group = "paste(PID, SID)", colour = "PID2"
      #     ),
      #     inherit.aes = FALSE, lwd = 0.4, alpha = 0.4
      #   ) + 
      #   scale_colour_manual(values = c("grey80","grey65","grey50","grey30","grey10")) +
      #   guides(colour="none")

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

  
  # Add arrows indicating target cells 
  if (!is.null(max_vec_plotted)) {
    if (max(df$distance) > max_vec_plotted) {
      df[df$distance > max_vec_plotted, ]$target_X <- NA
      df[df$distance > max_vec_plotted, ]$target_Y <- NA
    }
    #browser()
    vector <- as.vector(na.omit(df[[vec_aes]]))
    
    gvocc <- gvocc +
      geom_quiver(aes(x, y, # call modified internal function, head_size = distance/10,
        # ggquiver::geom_quiver(aes(x, y, # to call published package version
        u = target_X - x, v = target_Y - y,
        colour = vector, 
        size = vector
      ), vecsize = 0, alpha = vec_alpha, inherit.aes = FALSE) +
      scale_size_continuous(range = vec_lwd_range) +
      scale_colour_gradient2(low = low_col, mid = mid_col, high = high_col) 
    
 
  # add NAs to plot where grid cells have no target cell within the distance range of 'max_vec_plotted'
  gvocc <- gvocc +
    guides(colour = "none", size = "none") + 
    geom_text(
    data = df[df$distance > max_vec_plotted, ], aes(x, y), inherit.aes = FALSE,
    size = 2, colour = "grey99", 
    alpha = 0.75, label = "NA"
  )
  }
  
  gvocc
}
