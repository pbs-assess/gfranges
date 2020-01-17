#' Make prediction dataframe for chopsticks
#'
#' @param data The data frame used to make the model matrix.
#' @param formula The formula used to make the model matrix.
#' @param species If NULL, loops through all species in raw data.
#' @param x_variable Which variable to plot sticks on.
#' @param split_variable Which variable to plot sticks at the min and max of.
#' @param use_quantiles If TRUE, plots lines for 25th and 75th quantile of splitting
#'    varible values occupied by each species. If FALSE, use min and max values occupied by each species.
#' @param N Number of increments
#'
#' @export
interaction_df <- function(
                           data,
                           formula,
                           species = NULL,
                           x_variable = "squashed_temp_vel",
                           split_variable = "mean_temp",
                           use_quantiles = TRUE,
                           N = 10) {
  if (is.null(species)) {
    species <- unique(data$species)
  }

  nd_by_species <- purrr::map_df(species, function(spp) {
    spp_d <- filter(data, species == !!spp)

    x_range <- range((spp_d[[x_variable]]), na.rm = TRUE)

    nd <- data.frame(species = rep(unique(spp_d$species), N * 2))
    nd$genus <- spp_d$genus[1]

    if (use_quantiles) {
      split_low <- quantile(spp_d[[split_variable]], 0.025, na.rm = TRUE)
      split_high <- quantile(spp_d[[split_variable]], 0.975, na.rm = TRUE)

      nd$chopstick <- c(
        rep(paste0(
          "low ", shortener(split_variable),
          ""
        ), length.out = N),
        rep(paste0(
          "high ", shortener(split_variable),
          ""
        ), length.out = N)
      )
    } else {
      split_range <- range((spp_d[[split_variable]]), na.rm = TRUE)
      split_low <- split_range[1]
      split_high <- split_range[2]

      nd$chopstick <- c(
        rep(paste0(
          "min ", shortener(split_variable),
          ""
        ), length.out = N),
        rep(paste0(
          "max ", shortener(split_variable),
          ""
        ), length.out = N)
      )
    }

    nd[[x_variable]] <- c(
      seq(x_range[1], x_range[2], length.out = N),
      seq(x_range[1], x_range[2], length.out = N)
    )

    nd[[split_variable]] <- c(
      rep(split_low, length.out = N),
      rep(split_high, length.out = N)
    )

    for (i in colnames(spp_d)) {
      if (!i %in% c(split_variable, x_variable)) {
        nd[[i]] <- 0
      }
    }

    mm <- as.data.frame(model.matrix(formula, nd))
    mm$genus <- spp_d$genus[1]
    mm$species <- spp_d$species[1]
    mm$chopstick <- nd$chopstick
    mm
  })

  nd_by_species
}

#' Plot interactions with confidence intervals
#'
#' @param model TMB model with X_pj and pred_dat elements.
#' @param y_label Label response variable.
#' @param x_variable Variable to plot on x axis.
#' @param type Interaction type, if more than one in model.
#' @param colours Default NULL gives red for high and blue for low.
#' @param species Can plot just one species, otherwise NULL will facet.
#'
#' @examples
#' plot_fuzzy_chopsticks(model,
#'   y_label = "Predicted biomass trend",
#'   x_variable = "temp_trend_scaled",
#'   type = "temp"
#' )
#' @export
plot_fuzzy_chopsticks <- function(model,
                                  y_label = "Predicted biomass trend",
                                  x_variable = "temp_trend_scaled",
                                  type = NULL,
                                  colours = NULL,
                                  species = NULL) {
  par_est <- as.list(model$sdr, "Estimate", report = TRUE)
  par_sd <- as.list(model$sdr, "Std. Error", report = TRUE)

  pred_dat <- model$pred_dat
  pred_dat$est_p <- par_est$eta_p
  pred_dat$sd_p <- par_sd$eta_p

  if (is.null(colours)) {
    if (type == "do") {
      colours <- c("#5E4FA2", "#FDAE61")
    } else {
      colours <- c("#D53E4F", "#3288BD")
    }
  }

  if (!is.null(species)) {
    spp <- species
    pred_dat <- filter(pred_dat, species == !!spp)
  }

  if (!is.null(type)) {
    pred_dat <- filter(pred_dat, type == !!type)
  }

  p <- ggplot(pred_dat, aes_string(x_variable, "est_p")) +
    geom_line(aes(colour = chopstick)) +
    geom_ribbon(aes(
      fill = chopstick,
      ymin = est_p - 1.96 * sd_p, ymax = est_p + 1.96 * sd_p
    ), alpha = 0.3) +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = colours) +
    ylab(y_label) +
    gfplot::theme_pbs() +
    guides(colour = guide_legend(nrow = 1, ncol = 2)) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.direction = "vertical"
    )

  if (is.null(species)) {
    p <- p + facet_wrap(vars(species))
  }
  p
}

#' Plot raw chopsticks for vocc regression models
#'
#' @param model Model from vocc_regression function.
#'    Requires only named data element and coefs table.
#' @param species Species effect to be plotted.
#' @param variables List which variables are interacting.
#'
plot_raw_chopsticks <- function(model, species = NULL,
                                variables = c("mean_temp_scaled", "squashed_temp_vel_scaled"),
                                choose_x = NULL) {
  d <- model$data
  coefs <- model$coefs

  var_1 <- variables[1]
  var_2 <- variables[2]

  if (is.null(species)) {
    species <- unique(model$coefs$species)
  }
  x_variable <- c(var_1, var_1, var_1, var_1, var_2, var_2, var_2, var_2)

  effect1 <- paste0(var_1, " at min(", var_2, ")")
  effect2 <- paste0(var_1, " at max(", var_2, ")")
  effect3 <- paste0(var_2, " at min(", var_1, ")")
  effect4 <- paste0(var_2, " at max(", var_1, ")")
  marg_effect <- c(effect1, effect1, effect2, effect2, effect3, effect3, effect4, effect4)

  all_species <- purrr::map_df(species, function(spp) {
    sp_coef <- filter(coefs, species == !!spp)
    sp_coef

    spp_d <- filter(d, species == !!spp)

    x1_range <- range(spp_d[[var_1]])
    x2_range <- range(spp_d[[var_2]])
    b2 <- filter(sp_coef, coefficient == !!var_1)
    b3 <- filter(sp_coef, coefficient == !!var_1)
    interaction_name <- paste0(var_1, ":", var_2)
    b4 <- filter(sp_coef, coefficient == !!interaction_name)

    x <- c(x1_range[1], x1_range[2], x1_range[1], x1_range[2], x2_range[1], x2_range[2], x2_range[1], x2_range[2])
    x1 <- c(x1_range[1], x1_range[2], x1_range[1], x1_range[2], x1_range[1], x1_range[1], x1_range[2], x1_range[2])
    x2 <- c(x2_range[1], x2_range[1], x2_range[2], x2_range[2], x2_range[1], x2_range[2], x2_range[1], x2_range[2])

    b1 <- sp_coef$Estimate[1]

    y_hat_df <- purrr::map_df(seq_len(length(x)), function(i) {
      y_hat <- b1 + b2$Estimate * x1[i] + b3$Estimate * x2[i] + b4$Estimate * x1[i] * x2[i]
      data_frame(
        species = spp,
        x_var = x_variable[i],
        effect = marg_effect[i],
        x = x[i],
        x1 = x1[i],
        x2 = x2[i],
        y_hat = y_hat
      )
    })
    y_hat_df
  })

  if (is.null(choose_x)) {
    p <- ggplot(all_species, aes(x, y_hat, colour = effect)) + geom_line() +
      facet_wrap(~species) +
      xlab(paste0("Climate variable (scaled)")) +
      scale_colour_manual(values = c("#D53E4F", "#3288BD", "#5E4FA2", "#FDAE61")) +
      gfplot::theme_pbs()
  } else {
    if (choose_x == 1) {
      all_species <- filter(all_species, x_var == !!variables[1]) %>%
        mutate(effect = gsub(paste(var_1, "at"), "", effect))
    }
    if (choose_x == 2) {
      all_species <- filter(all_species, x_var == !!variables[2]) %>%
        mutate(effect = gsub(paste(var_2, "at"), "", effect))
    }

    label_x <- all_species$x_var[1]
    p <- ggplot(all_species, aes(x, y_hat, colour = effect)) + geom_line() +
      facet_wrap(~species) +
      xlab(paste0(label_x)) +
      scale_colour_manual(values = c("#D53E4F", "#3288BD")) +
      gfplot::theme_pbs()
  }
  p + theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.direction = "vertical"
  ) +
    guides(colour = guide_legend(nrow = 2, ncol = 2))
  p
}
