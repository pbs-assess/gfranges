#' Make prediction dataframe for chopsticks
#'
#' @param data The data frame used to make the model matrix.
#' @param formula The formula used to make the model matrix.
#' @param species If NULL, loops through all species in raw data.
#' @param x_variable Which variable to plot sticks on.
#' @param split_variable Which variable to plot sticks at the min and max of.
#' @param use_quantiles If TRUE, plots lines for 2.5th and 97.5th quantile of splitting
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
          "low"), length.out = N),
        rep(paste0(
          "high"), length.out = N)
      )
    } else {
      split_range <- range((spp_d[[split_variable]]), na.rm = TRUE)
      split_low <- split_range[1]
      split_high <- split_range[2]

      nd$chopstick <- c(
        rep(paste0(
          "min", shortener(split_variable),
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
                                  species = NULL, 
                                  order_var = "species"
  ) {
  par_est <- as.list(model$sdr, "Estimate", report = TRUE)
  par_se <- as.list(model$sdr, "Std. Error", report = TRUE)

  pred_dat <- model$pred_dat
  pred_dat$est_p <- par_est$eta_p
  pred_dat$se_p <- par_se$eta_p
  
  if (is.null(colours)) {
    if (type == "do") {
      colours <- c("#5E4FA2", "#FDAE61")
    } else {
      if (type == "mean_temp") {
        colours <- c("#5E4FA2", "#FDAE61")
      } else {
        colours <- c("#D53E4F", "#3288BD")
      }
    }
  }

  if (!is.null(species)) {
    spp <- species
    pred_dat <- filter(pred_dat, species == !!spp)
  } else {
    pred_dat <- mutate(pred_dat, order = !!order_var)
  }

  if (!is.null(type)) {
    pred_dat <- filter(pred_dat, type == !!type)
  }

  # pred_dat <- pred_dat %>% group_by(species, chopstick) %>%
  #   mutate(
  #     slope = round(lm("est_p"~x_variable)$coefficients[2], 4),
  #     mean_se = mean(se_p) 
  #   ) 
  
  p <- ggplot(pred_dat, aes_string(x_variable, "est_p")) +
    geom_line(aes(colour = chopstick)) +
    geom_ribbon(aes(
      fill = chopstick,
      ymin = est_p - 1.96 * se_p, ymax = est_p + 1.96 * se_p
    ), alpha = 0.3) +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = colours) +
    ylab(y_label) +
    gfplot::theme_pbs() +
    guides(colour = guide_legend(nrow = 1, ncol = 2)) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.direction = "vertical",
      legend.position = "top"
    )

  if (is.null(species)) {
    p <- p + facet_wrap(vars(species)) #forcats::fct_reorder(species, order)
  }
  p
}

#' Save slopes from chopstick plots
#' @export
chopstick_slopes <- function (model,
      type = NULL,
      x_variable = "temp_trend_scaled",
      species = NULL) {
  
  par_est <- as.list(model$sdr, "Estimate", report = TRUE)
  par_se <- as.list(model$sdr, "Std. Error", report = TRUE)
  
  
  #model$obj$delta_q_low
  pred_dat <- model$pred_dat
  pred_dat$est_p <- par_est$eta_p
  pred_dat$se_p <- par_se$eta_p
  
  
  if (!is.null(species)) {
    spp <- species
    pred_dat <- filter(pred_dat, species == !!spp)
  }
  
  if (!is.null(type)) {
    pred_dat <- filter(pred_dat, type == !!type)
  }
  
  slopes <- pred_dat %>% 
    rename( x = x_variable) %>% 
    group_by(species, chopstick) %>% #select(species, type, chopstick, x)%>% 
    mutate(
      slope = signif(lm(est_p~x)$coefficients[2], 2),
      est_low = est_p-(se_p*1.96),
      est_high = est_p+(se_p*1.96),
      est_max = if_else((x<quantile(x, 0.01)), est_low, 
        if_else((x>quantile(x, 0.99)), est_high, NA_real_, NA_real_)),
      est_min = if_else((x<quantile(x, 0.01)), est_high, 
        if_else((x>quantile(x, 0.99)), est_low, NA_real_, NA_real_)),
      slope_max = signif(lm(est_max~x)$coefficients[2], 2),
      slope_min = signif(lm(est_min~x)$coefficients[2], 2)) %>% 
    select(species, type, chopstick, slope, slope_max, slope_min) %>% 
    unique()
  
  SE <- model$coefs %>% filter(coefficient == x_variable) %>% 
    select(species, `Std. Error`) %>% 
    rename(SE = `Std. Error`)
  
  
  # browser()
  #delta_low 
  
  deltas <- model$deltas %>% select(species, chopstick, Estimate, `Std. Error`) %>% rename(slope_est = Estimate, slope_se = `Std. Error`)
  
  # low_slopes <- slopes %>% filter(chopstick == "low") 
  # low_slopes <- left_join(low_slopes, delta_low)
  # delta_high <- model$delta %>% select(species, Estimate, `Std. Error`) %>% rename(slope_est = Estimate, slope_se = `Std. Error`) 
  # high_slopes <- slopes %>% filter(chopstick == "high") 
  # high_slopes <- left_join(high_slopes, delta_high)
  #slopes <- rbind(low_slopes, high_slopes)
  
  slopes <- left_join(slopes, deltas)
  slopes <- left_join(slopes, SE)

  slopes

}


#' Plot slopes from chopstick plots
#' @export
plot_chopstick_slopes <- function (slopedat,
  type = NULL,
  x_variable = "temp_trend_scaled",
  legend_position = c(.7, .95),
  hack = F,
  colours = NULL) {
  
  if (!is.null(type)) {
  if (is.null(colours)) {
    if (type == "do") {
      colours <- c("#5E4FA2", "#FDAE61")
    } else {
      if (type == "mean_temp") {
        colours <- c("#5E4FA2", "#FDAE61")
      } else {
        colours <- c("#D53E4F", "#3288BD")
      }
    }
  }
  }

if(hack) { 
p <- ggplot(slopedat, aes(
  forcats::fct_reorder(species, -slope),
  slope,
  colour = chopstick, 
  ymin = (slope - SE*1.96),
  ymax = (slope + SE*1.96)
  # ymin = slope_min,
  # ymax = slope_max
)) + 
  geom_hline(yintercept = 0, colour = "darkgray") +
  scale_colour_manual(values = colours) + #, guide=T
  geom_pointrange(alpha=0.65, position = position_dodge(width=0.5)) + 
  coord_flip() +
  xlab("") + #ylab("") + # ggtitle("slopes") +
  gfplot:::theme_pbs() + theme(
    # axis.title.y = element_blank(),
    legend.position = legend_position,
    legend.title = element_blank(),
    legend.text = element_text(size = 10)#,
    # legend.direction = "vertical"
  )
} else {
  p <- ggplot(slopedat, aes(
    forcats::fct_reorder(species, -slope),
    slope_est,
    colour = chopstick, 
    ymin = (slope_est - slope_se*1.96),
    ymax = (slope_est + slope_se*1.96)
    # ymin = slope_min,
    # ymax = slope_max
  )) + 
    geom_hline(yintercept = 0, colour = "darkgray") +
    scale_colour_manual(values = colours) + #, guide=T
    geom_pointrange(alpha=0.65, position = position_dodge(width=0.5)) + 
    coord_flip() +
    xlab("") + #ylab("") + # ggtitle("slopes") +
    gfplot:::theme_pbs() + theme(
      # axis.title.y = element_blank(),
      legend.position = legend_position,
      legend.title = element_blank(),
      legend.text = element_text(size = 10)#,
      # legend.direction = "vertical"
    )
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
