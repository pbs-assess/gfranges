#' Plot clusters
#'
#' @param model An object of class "partition" created by the functions
#'   [cluster::pam()] or [stats::kmeans()].
#' @param data The original data frame used in the clustering.
#' @param colour_vector A vector of character or factors to colour the points
#'   by.
#' @param colour_label A label for the colour legend.
#' @param ... Other arguments to pass2 [factoextra::fviz_cluster()].
#'
#' @return A ggplot2 object.
#' @export
#'
#' @importFrom ggplot2 aes_string scale_colour_brewer ggplot geom_point
#' geom_polygon labs
#'
#' @examples
#' df <- haddock_mod %>%
#'   dplyr::select(fo_median, ffmsy_median, bbmsy_median) %>%
#'   scale()
#'
#' # Evaluate the number of clusters
#' factoextra::fviz_nbclust(df, kmeans, method = "wss")
#' factoextra::fviz_nbclust(df, kmeans, method = "silhouette")
#' factoextra::fviz_nbclust(df, kmeans, method = "gap_stat")
#'
#' # Illustrate an example with 2 clusters:
#' m <- kmeans(df, centers = 2L)
#'
#' plot_clusters(m,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model"
#' )
#'
#' # The default is to plot the two first principal components as x and y.
#' # Instead we can pick specific axes to show:
#' plot_clusters(m,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model",
#'   choose.vars = c("bbmsy_median", "ffmsy_median")
#' )
#'
#' # Or with the more robust cluster::pam() algorithm:
#' m <- cluster::pam(df, k = 4L)
#' plot_clusters(m,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model"
#' )
plot_clusters <- function(model, data = NULL, 
                          colour_vector = NULL,
                          text_label = NULL,
                          shape_by_group = FALSE,
                          colour_label = "model", ...) {
  if (!class(model)[[1]] %in% c("pam", "kmeans")) {
    stop("Model must be of class pam or kmeans.")
  }

  g <- factoextra::fviz_cluster(model,
    data = data,
    ellipse.type = "convex",
    ...
  )
  gdat <- ggplot2::ggplot_build(g)$data

  gdat[[1]]$group <- as.factor(gdat[[1]]$group) 

  if (!is.null(colour_vector)) {
    gdat[[1]] <- data.frame(gdat[[1]], colour_vector = colour_vector)
  } else {
    gdat[[1]] <- data.frame(gdat[[1]], colour_vector = 1)
  }

  if (identical(class(model)[[1]], "pam")) {
    label_data <- gdat[[3]]
  }
  if (identical(class(model)[[1]], "kmeans")) {
    label_data <- gdat[[4]]
  }

  if (is.null(text_label)) {
   text_label <- label_data[["label"]]
  }

  gg <- ggplot(gdat[[2]], aes_string("x", "y")) +
    geom_polygon(aes_string(x = "x", y = "y", group = "group"),
      fill = NA, colour = "grey50"
    ) +
    ggplot2::theme_minimal() + 
    ggrepel::geom_text_repel(
      data = label_data,
      aes(x, y, label = text_label),
      size = 2
    ) 
  
  if(shape_by_group) {
  gg <- gg + geom_point(data = gdat[[1]], 
        aes_string(colour = "colour_vector", shape = "group"), size = 3) +
    scale_shape_discrete(solid = T) +
    guides(shape=F)+
    labs(x = g$labels$x, y = g$labels$y, colour = colour_label) +
    ggplot2::scale_color_viridis_d()
  } else {
  gg <- gg + geom_point(data = gdat[[1]], 
    aes_string(colour = "colour_vector"), size = 3) +
    labs(x = g$labels$x, y = g$labels$y, colour = colour_label) +
    ggplot2::scale_color_viridis_d()
  }
  
  gg
}
