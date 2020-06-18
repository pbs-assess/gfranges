#' Make prediction dataframe for chopsticks
#'
#' @param data The data frame used to make the model matrix.
#' @param formula The formula used to make the model matrix.
#' @param species If NULL, loops through all species in raw data.
#' @param x_variable Which variable to plot sticks on.
#' @param split_variable Which variable to plot sticks at the min and max of.
#' @param use_quantiles If TRUE, plots lines for 2.5th and 97.5th quantile of
#'    splitting varible values occupied by each species.
#'    If FALSE, use min and max values occupied by each species.
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
                           N = 3) {
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
          "low"
        ), length.out = N),
        rep(paste0(
          "high"
        ), length.out = N)
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
#' @param model TMB model with X_pj and pred_dat elements
#' @param y_label Label response variable
#' @param x_variable Variable to plot on x axis
#' @param type Interaction type, if more than one in model
#' @param colours Default NULL gives red for high and blue for low
#' @param choose_species Option to choose just one species
#' @param choose_age Option to choose just one age class
#' @param alpha_range Choose alpha for non-sig and sig slopes
#' @param line_size Change line thickness
#' @param global_col Set colour for global chopsticks
#' @param order_var Varible to order plots by
#' @param slopes Df of slopes from chopstick_slopes function
#' @param imm_model Add immature data from separate model
#' @param imm_slopes Add separate slope df for immatures
#' @param rug Logical for adding rug
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
                                  line_size = 0.7,
                                  alpha_range = c(0.25, 0.9),
                                  global_col = "gray30",
                                  choose_species = NULL,
                                  choose_age = NULL,
                                  order_var = "species",
                                  slopes = NULL,
                                  imm_model = NULL,
                                  imm_slopes = NULL,
                                  rug = F # might eventually replace with a histogram rug
) {
  pred_dat <- model$pred_dat

  if (is.null(imm_model)) {
    pred_dat <- pred_dat %>% mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
    pred_dat <- pred_dat %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))
  }

  if (is.null(pred_dat$est_p)) {
    est <- as.list(model$sdr, "Estimate", report = TRUE)
    se <- as.list(model$sdr, "Std. Error", report = TRUE)
    pred_dat$est_p <- est$eta_p
    pred_dat$se_p <- se$eta_p
  }

  if (!is.null(slopes)) {
    signif <- slopes %>%
      select(species, type, chopstick, sig_diff, global_slope, global_se) %>%
      unique()
    pred_dat <- left_join(pred_dat, signif) %>%
      mutate(global = if_else(sig_diff == "N",
        if_else(((abs(global_slope) - global_se * 1.96) >= 0), est_p, NA_real_),
        NA_real_
      ))
  }

  if (!is.null(imm_model)) {
    imm_pred_dat <- imm_model$pred_dat

    if (is.null(imm_pred_dat$est_p)) {
      imm_est <- as.list(imm_model$sdr, "Estimate", report = TRUE)
      imm_se <- as.list(imm_model$sdr, "Std. Error", report = TRUE)
      imm_pred_dat$est_p <- imm_est$eta_p
      imm_pred_dat$se_p <- imm_se$eta_p
    }

    if (!is.null(scale_imm)) {
      imm_pred_dat$est_p <- imm_pred_dat$est_p * scale_imm
      imm_pred_dat$se_p <- imm_pred_dat$se_p * scale_imm
    }

    if (!is.null(imm_slopes)) {
      imm_signif <- imm_slopes %>%
        select(species, type, chopstick, sig_diff, global_slope, global_se) %>%
        unique()
      imm_pred_dat <- left_join(imm_pred_dat, imm_signif) %>%
        mutate(global = if_else(sig_diff == "N",
          if_else(((abs(global_slope) - global_se * 1.96) >= 0), est_p, NA_real_),
          NA_real_
        ))
    }
  }
  
  if (is.null(colours)) {
    if (type == "DO" | type == "do") {

      # # green to blue option
      # colours = c("#3d95cc", "yellowgreen")
      
      # # cyan to gold option,
      colours = c("goldenrod1", "darkcyan") #"gold"
      
      # # blue to yellow option
      # colours <- c("#FDAE61", "#3d95cc")
      
      # # yellow to purple option
      # colours <- c("#5E4FA2", "#FDAE61")
    } else {
      if (type == "mean_temp") {
        colours <- c("#5E4FA2", "#FDAE61")
      } else {
        # purple to red option 
        # colours <- c("#cd0000", "#5E4FA2")
        # # blue and red option
        colours <- c("orangered2", "royalblue4")
        # colours <- c("Firebrick2", "royalblue4")
        # colours <- c("Red 3", "royalblue4")
        # colours <- c("#D53E4F", "royalblue4")
        # colours <- c("#D53E4F", "#3d95cc") #"#36648b") #  ,"#3288BD") #"#FF420A"  # colours <- c("#D53E4F", "#3288BD")
      }
    }
  }
  
  if (!is.null(type)) {
    pred_dat <- filter(pred_dat, type == !!type) %>% mutate(chopstick = paste(chopstick, type))

    if (!is.null(imm_model)) {
      imm_pred_dat <- filter(imm_pred_dat, type == !!type) %>% mutate(chopstick = paste(chopstick, type))
    }
  } else {

  }

  # pred_dat <- pred_dat %>% group_by(species, chopstick) %>%
  #   mutate(
  #     slope = round(lm("est_p"~x_variable)$coefficients[2], 4),
  #     mean_se = mean(se_p)
  #   )

  if (!is.null(imm_model)) {
    imm_pred_dat$age <- "immature"
    pred_dat <- rbind(pred_dat, imm_pred_dat)
  }

  if (!is.null(choose_species)) {
    spp <- choose_species
    pred_dat <- filter(pred_dat, species == !!spp)
    if (!is.null(choose_age)) {
      check_age <- choose_age
      pred_dat <- filter(pred_dat, age == !!check_age)
    }
  } else {
    pred_dat <- mutate(pred_dat, order = !!order_var)
    if (!is.null(choose_age)) {
      check_age <- choose_age
      pred_dat <- filter(pred_dat, age == !!check_age)
    }
  }

  # Shorten the one very long species name...
  pred_dat$species[pred_dat$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

  p <- ggplot(pred_dat, aes_string(x_variable, "est_p")) +
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
  p <- p + geom_hline(yintercept = 0, colour = "gray", linetype = "solid") +
    geom_ribbon(data = filter(pred_dat, age == "mature"), aes(
    fill = chopstick,
    ymin = est_p - 1.96 * se_p, ymax = est_p + 1.96 * se_p
  ), alpha = 0.2) +
    geom_ribbon(data = filter(pred_dat, age == "immature"), aes(
      fill = chopstick,
      ymin = est_p - 1.96 * se_p, ymax = est_p + 1.96 * se_p
    ), alpha = 0.2)

  if (is.null(pred_dat$sig_diff)) {
    p <- p + geom_line(aes(colour = chopstick))
  } else {
    p <- p + geom_line(aes(
      alpha = sig_diff,
      colour = chopstick, linetype = age
    ), size = line_size) +
      geom_smooth(
        method = "lm", aes_string(x_variable, "global",
          linetype = "age"
        ), size = line_size,
        colour = global_col, se = F, inherit.aes = F
      ) +
      scale_alpha_discrete(range = alpha_range, guide = F)
  }

  if (length(unique(pred_dat$age)) > 1) {
    p <- p + scale_linetype_manual(values = c("dashed", "solid"), guide = F)
    # p <- p + scale_linetype_manual(values=c("dotted", "solid"), guide = F)
  }

  if (is.null(choose_species)) {
    p <- p + facet_wrap(vars(species)) # forcats::fct_reorder(species, order)
  }

  if (rug) {
    histdat <- model$data %>%
      mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature")) %>%
      mutate(species = stringr::str_replace(species, ".*mature ", ""))

    # Shorten the one very long species name...
    histdat$species[histdat$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"

    p <- p + geom_rug(
      data = filter(histdat, age == "immature"), aes_string(x_variable),
      alpha = 0.2, inherit.aes = F
    ) +
      geom_rug(
        data = filter(histdat, age == "mature"), aes_string(x_variable),
        sides = "tl", alpha = 0.2, inherit.aes = F
      )
  }
  p
}

#' Save slopes from chopstick plots
#' @export
chopstick_slopes <- function(model,
                             type = NULL,
                             interaction_column = "temp_trend_scaled:mean_temp_scaled",
                             x_variable = "temp_trend_scaled",
                             species = NULL) {
  pred_dat <- model$pred_dat
  # if(is.null(imm_model)) {
  pred_dat <- pred_dat %>% mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
  pred_dat <- pred_dat %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))
  # }

  if (is.null(pred_dat$est_p)) {
    est <- as.list(model$sdr, "Estimate", report = TRUE)
    se <- as.list(model$sdr, "Std. Error", report = TRUE)
    pred_dat$est_p <- est$eta_p
    pred_dat$se_p <- se$eta_p
  }

  if (!is.null(model$deltas)) {
    deltas <- model$deltas %>% 
      mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
    deltas <- deltas %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))

    deltas <- deltas %>%
      select(species, age, type, chopstick, Estimate, `Std. Error`) %>%
      rename(slope_est = Estimate, slope_se = `Std. Error`)

    if (is.null(model$delta_diff)) {
      est <- as.list(model$sdr, "Estimate", report = TRUE)
      se <- as.list(model$sdr, "Std. Error", report = TRUE)

      model$delta_diff <-
        cbind(est$diff_delta_k, se$diff_delta_k)
      model$delta_diff <- as.data.frame(model$delta_diff)
      model$delta_diff$type <- type
      model$delta_diff$species <- unique(model$deltas$species)
      names(delta_diff) <- c("est", "se", "type", "species")
    }

    diffs <- model$delta_diff %>%
      rename(diff = est, diff_se = se) %>%
      mutate(min_diff = abs(diff) - diff_se * 1.96, sig_diff = if_else(min_diff <= 0, "N", "Y"))
    diffs <- diffs %>% mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
    diffs <- diffs %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))
  }

  if (!is.null(species)) {
    spp <- species
    pred_dat <- filter(pred_dat, species == !!spp)
  }

  if (!is.null(type)) {
    pred_dat <- filter(pred_dat, type == !!type)

    global_coefs <- filter(model$coefs, coefficient == !!x_variable) %>%
      select(-species_id, -coefficient) %>%
      rename(global_slope = Estimate, global_se = `Std. Error`)
    global_coefs <- global_coefs %>% mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
    global_coefs <- global_coefs %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))

    if (!is.null(model$deltas)) {
      diffs <- filter(diffs, type == !!type)
      global_slopes <- filter(diffs, sig_diff == "N") %>% select(species, age)
      global <- left_join(global_slopes, global_coefs)
      diffs <- left_join(diffs, global)
    } else {
      diffs <- filter(model$coefs, coefficient == !!interaction_column) %>%
        select(-species_id, -coefficient) %>%
        rename(diff = Estimate, diff_se = `Std. Error`) %>%
        mutate(min_diff = abs(diff) - diff_se * 1.96, sig_diff = if_else(min_diff <= 0, "N", "Y"))
      diffs <- diffs %>% mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
      diffs <- diffs %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))
      global_slopes <- filter(diffs, sig_diff == "N") %>% select(species, age)
      global <- left_join(global_slopes, global_coefs)
      diffs <- left_join(diffs, global)
    }
  }
  slopes <- pred_dat %>%
    rename(x = x_variable) %>%
    group_by(species, age, type, chopstick) %>% # select(species, type, chopstick, x)
    mutate(
      slope = signif(lm(est_p ~ x)$coefficients[2], 2), # simple slope from lm
      est_low = est_p - (se_p * 1.96),
      est_high = est_p + (se_p * 1.96),
      est_max = if_else((x < quantile(x, 0.01)), est_low,
        if_else((x > quantile(x, 0.99)), est_high, NA_real_, NA_real_)
      ),
      est_min = if_else((x < quantile(x, 0.01)), est_high,
        if_else((x > quantile(x, 0.99)), est_low, NA_real_, NA_real_)
      ),
      slope_max = signif(lm(est_max ~ x)$coefficients[2], 2), # max slope possible inside CI
      slope_min = signif(lm(est_min ~ x)$coefficients[2], 2)
    ) %>% # min slope possible inside CI
    select(species, type, chopstick, slope, slope_max, slope_min) %>%
    unique()

  # SE for interation involving x_variable
  SE <- model$coefs %>%
    filter(coefficient == interaction_column) %>%
    select(species, `Std. Error`) %>%
    rename(SE = `Std. Error`) %>%
    mutate(
      age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"),
      species = stringr::str_replace(species, ".*mature ", "")
    )

  if (!is.null(model$deltas)) {
    if (!is.null(type)) {
      deltas <- filter(deltas, type == !!type)
    }
    slopes <- left_join(slopes, deltas) # by = c("species", "type", "chopstick")
  }

  slopes <- left_join(slopes, diffs)
  slopes <- left_join(slopes, SE)
  slopes
}


#' Plot slopes from chopstick plots
#'
#' @param slopedat Df from chopstick_slopes function
#' @param type Select type if more than one interaction in model
#' @param x_variable Which variable on x-axis
#' @param legend_position Given legend position
#' @param hack Logical for comparing with hacked slope estimate w interaction SE
#' @param imm_slopes Add immature data
#' @param add_global Logical for including global slope estimates
#' @param global_col Set colour for global slopes
#' @param alpha_range Set alapha values based on slope significance
#' @param colours Add custom colours
#' @param point_size Change point size
#' @param add_grey_bars Add grey stripes to alternating species
#' @param name_chop_type Add type to chopstick name in legend
#'
#' @export
plot_chopstick_slopes <- function(slopedat,
                                  type = NULL,
                                  x_variable = "temp_trend_scaled",
                                  legend_position = c(.7, .95),
                                  hack = F,
                                  imm_slopes = NULL,
                                  add_global = T,
                                  global_col = "gray30",
                                  point_size = 0.75,
                                  alpha_range = c(0.4, 0.99),
                                  add_grey_bars = F,
                                  name_chop_type = T, 
                                  colours = NULL) {
  if (!is.null(imm_slopes)) {
    imm_slopes$age <- "immature"
    if (!is.null(scale_imm)) {
      imm_slopes$slope_est <- imm_slopes$slope_est * scale_imm
      imm_slopes$slope_se <- imm_slopes$slope_se * scale_imm
      imm_slopes$global_slope <- imm_slopes$global_slope * scale_imm
      imm_slopes$global_se <- imm_slopes$global_se * scale_imm
    }
    slopedat <- rbind(slopedat, imm_slopes)
  }

  if (!is.null(type)) {
    slopedat <- filter(slopedat, type == !!type) %>% ungroup()
    
    if (name_chop_type) {
    slopedat <- slopedat %>% mutate(chopstick = paste(chopstick, type))
    }
    
    if (is.null(colours)) {
      

      if (type == "DO" | type == "do") {
        # # green to blue option
        # colours = c("#3d95cc", "yellowgreen")

        # # cyan to gold option,
        colours = c("goldenrod1", "darkcyan") #"gold"

        # # blue to yellow option
        # colours <- c("#FDAE61", "#3d95cc")
        # # yellow to purple option
        # colours <- c("#5E4FA2", "#FDAE61")
      } else {
        if (type == "mean_temp") {
          colours <- c("#5E4FA2", "#FDAE61")
        } else {
          # purple to red option
          # colours <- c("#cd0000", "#5E4FA2")
          # # blue and red option

          #colours <- c("Firebrick2", "royalblue4")
          colours <- c("Red 3", "royalblue4")
          # colours <- c("#D53E4F", "royalblue4")
          # colours <- c("#D53E4F", "#3d95cc") 
          # colours <- c("#D53E4F", "#3288BD")
       }
      }
    }
  }

  if (is.null(slopedat$sort_var)) {
    if (!is.null(slopedat$slope_est)) {
      slopedat <- slopedat %>% mutate(sort_var = slope_est)
    } else {
      slopedat <- slopedat %>% mutate(sort_var = slope)
    }
  }

  if (hack) {
    p <- ggplot(slopedat) +
      geom_hline(yintercept = 0, colour = "black", alpha = 0.7) +
      # geom_linerange(aes(forcats::fct_reorder(species, sort_var, mean, .desc=F), abs(diff),
      #   ymin = (abs(diff) - diff_se*1.96),
      #   ymax = (abs(diff) + diff_se*1.96),
      #   shape = age), alpha=0.25, colour = "gray",
      #   position = position_jitter(), #dodge.width = 1.2
      #   size = 0.4, fatten = 1.5, fill = "white") +
      scale_colour_manual(values = colours) + # , guide=T
      geom_pointrange(aes(forcats::fct_reorder(species, sort_var, mean, .desc = F),
        slope,
        colour = chopstick,
        shape = age,
        alpha = sig_diff,
        ymin = (slope - SE * 1.96), # use uncertainty from modeled interaction
        ymax = (slope + SE * 1.96) # use uncertainty from modeled interaction
        # ymin = slope_min, # min slope possible inside CI
        # ymax = slope_max # max slope possible inside CI
      ),
      position = position_jitterdodge(width = 0.25), # dodge.width = 1.2
      size = point_size, fatten = 1, fill = "white"
      ) +
      geom_pointrange(aes(species,
        global_slope,
        ymin = global_slope - 1.96 * global_se,
        ymax = global_slope + 1.96 * global_se,
        linetype = age, shape = age
      ),
      position = position_dodge(width = 0.25), # alpha = 0.5, #dodge.width = 0.4
      size = point_size,
      fatten = 1.5,
      colour = global_col, fill = "white",
      inherit.aes = F
      ) +
      scale_alpha_discrete(range = c(0.0, 0.99), guide = F) +
      coord_flip() +
      xlab("") + # ylab("") + # ggtitle("slopes") +
      gfplot:::theme_pbs() + theme(
        # axis.title.y = element_blank(),
        legend.position = legend_position,
        legend.title = element_blank(),
        legend.text = element_text(size = 10) # ,
        # legend.direction = "vertical"
      )

    if (length(unique(slopedat$age)) > 1) {
      p <- p + scale_linetype_manual(values = c("solid", "solid"), guide = F) +
        scale_shape_manual(values = c(21, 19), guide = F)
    } else {
      p <- p + scale_linetype_manual(values = c("solid"), guide = F) +
        scale_shape_manual(values = c(16), guide = F)
    }
  } else {
    p <- ggplot(slopedat, aes(
      forcats::fct_reorder(species, sort_var, .desc = F),
      slope_est,
      colour = chopstick,
      alpha = sig_diff,
      shape = age,
      linetype = age,
      ymin = (slope_est - slope_se * 1.96),
      ymax = (slope_est + slope_se * 1.96)
    )) +
      geom_hline(yintercept = 0, colour = "black", alpha = 0.7) 
    
    if(add_grey_bars) {
      .n <- length(unique(slopedat$species))
      .w <- 0.5
      p <- p + annotate(
        geom = "rect", xmin = seq(1, .n, by = 2) - .w, xmax = seq(1, .n, by = 2) + .w,
        ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.2
      )
    }
    
    p <- p + scale_colour_manual(values = colours) + # , guide=T
      geom_pointrange(
        position = position_jitterdodge(), # dodge.width = 1.2
        size = point_size, fatten = 1.5, 
        fill = "white"
      ) +
      coord_flip() +
      xlab("") +
      gfplot:::theme_pbs() + theme(
        # axis.title.y = element_blank(),
        legend.position = legend_position,
        legend.title = element_blank(),
        legend.text = element_text(size = 10) # ,
        # legend.direction = "vertical"
      )

    if (add_global) {
      p <- p + scale_alpha_discrete(range = c(0.0, 0.99), guide = F) +
        geom_pointrange(aes(species,
          global_slope,
          ymin = global_slope - 1.96 * global_se,
          ymax = global_slope + 1.96 * global_se, linetype = age, shape = age
        ),
        # alpha = 0.5,
        position = position_dodge(width = 0.5), # dodge.width = 0.4
        size = point_size, fatten = 1.5,
        colour = global_col, fill = "white",
        inherit.aes = F
        )
    } else {
      p <- p + scale_alpha_discrete(range = alpha_range, guide = F)
    }

    if (length(unique(slopedat$age)) > 1) {
      # p <- p + scale_linetype_manual(values=c("dashed", "solid"), guide = F) +
      p <- p + scale_linetype_manual(values = c("solid", "solid"), guide = F) +
        scale_x_discrete(expand = expansion(mult = .02)) +
        guides(colour = guide_legend(override.aes = list(size = 0.4))) +
        scale_shape_manual(values = c(21, 19), guide = F)
    } else {
      p <- p + scale_linetype_manual(values = c("solid"), guide = F) +
        scale_x_discrete(expand = expansion(mult = .02)) +
        guides(colour = guide_legend(override.aes = list(size = 0.4))) +
        scale_shape_manual(values = c(16), guide = F)
    }
  }
  
  p
}


#' Scatterplot of slopes against traits
#'
#' @param slopes_w_traits Df of slopes merged with df of species traits
#' @param x Variable (species trait) on x-axis
#' @param slope_var Varible containing slope estimate
#' @param col_group Varible to colour by
#' @param point_size Set point size
#' @param point_alpha Set point alpha
#' @param pointrange Logical for adding 95CI bars to points. Default T
#' @param regression Logical for plotting simple linear regression line
#' @param shape_group Varible to set shape by
#' @param point_shapes Vector of shapes. Defaults to open and closed circles
#'
#' @export
slope_scatterplot <- function(slopes_w_traits, x,
                              slope_var = "slope_est",
                              col_group = "chopstick",
                              shape_group = "age",
                              point_size = 0.75,
                              point_alpha = 0.99,
                              point_shapes = c(21, 19),
                              pointrange = T,
                              regression = F) {
  p <- ggplot(slopes_w_traits, aes_string(x, slope_var, shape = shape_group, colour = col_group)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.75, linetype = "dashed") 
  if (regression) {
    p <- p + geom_smooth(method = "lm", fill = "lightgray")
  }
  
  if (pointrange) {
  p <- p + geom_pointrange(aes(ymin = (slope_est - slope_se * 1.96),
    ymax = (slope_est + slope_se * 1.96)), alpha = point_alpha, fatten = 1)
  } else {
    p <- p + geom_point(size = point_size, alpha = point_alpha) 
  }
  p <- p + scale_colour_viridis_d(begin = .8, end = .2) +
    scale_shape_manual(values = point_shapes) +
    gfplot:::theme_pbs()
  p
}


#' Plot raw chopsticks for vocc regression models
#'
#' @param model Model from vocc_regression function.
#'    Requires only named data element and coefs table.
#' @param species Species effect to be plotted.
#' @param variables List which variables are interacting.
#' @param choose_x Which varible from list is on x-axis (either 1 or 2)
#'
plot_raw_chopsticks <- function(model,
                                species = NULL,
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
