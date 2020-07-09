#' Predicted curves for parameters in sdmTMB models
#'
#' @param dat Dataframe of density estimates containing x, y_hat, year, and type
#'     Create using density functions: fixed_density or time_varying_density 
#' @param time_varying Logical for if the parameter of interest is time-varying
#' @param variable_label Descriptive label for parameter "x"
#' @param xlimits X-axis limits. Default is c(0, max(dat$x))
#' @param x_breaks Give breaks. Default is the ggplot default
#' @param mountains_only Logical for whether to exclude non-"sad-face" type curves. 
#'
#' @export
#'
plot_mountains <- function(dat, 
  time_varying = TRUE, 
  variable_label = "Depth", 
  xlimits = c(0, max(dat$x)), 
  x_breaks = waiver(),
  mountains_only = F
  ) {

  if (time_varying) {
    # mountain_scaler <- max(dat$y_hat)  
    # #mountain_scaler <- mountain_scaler * 10
    dat <- dat %>% group_by(year) %>% mutate(max_y = max(y_hat), xintercept = x[y_hat == max_y])
    dat$year <- as.factor(dat$year)
    
    
    # make y axis limited by mountain heights
    if (any(names(dat) == "type")) {
    dat1 <- filter(dat, type == "sad") 
    } else { dat1 <- dat }
    
    ymaximum <- max(dat1$y_hat[dat1$x > xlimits[1] & dat1$x < xlimits[2]])
    
    if (mountains_only) {
      dat <- filter(dat, type == "sad") 
    }
    
    # ALTERNATIVE METHOD FOR DETERMINING YMAX. Way worse than above!
    #rangex <- max(dat$x) - min(dat$x)
    #edge_buffer <- rangex*ymax_trim_fact # @param ymax_trim_fact Proportion of extreme x values to exclude before setting ymax.
    #ymaximum <- max(dat$y_hat[dat$x > (min(dat$x)+edge_buffer) & dat$x < (max(dat$x)-edge_buffer )])
    
    ggplot(dat, aes_string(x = "x", 
      y="y_hat*10000",
      # ymax = "10*y_hat + year*mountain_scaler", ymin = "0 + year*mountain_scaler",
      # # ymax = "y_hat*mountain_scaler", ymin = "0",
      group = "year", colour = "year", fill = "year"
    )) +
      # # geom_ribbon(lwd = 0.75, alpha = 0.01) +
      geom_line(lwd = 0.75, alpha = 0.8) +
      geom_vline(aes_string(xintercept = "xintercept", group = "year", colour = "year"), linetype =2) +
      scale_y_continuous(limits = c(0, ymaximum*10000 )) +
      #scale_y_continuous(limits = c(0, median(dat$y_hat*100)*20)) +
      scale_x_continuous(expand = c(0,0), limits = xlimits, breaks = x_breaks) +
      xlab(variable_label) +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      ylab("Predicted density kg/ha ")+
      gfplot::theme_pbs() 
    
  } else {
    #browser()
    dat$year <- as.factor(dat$year)
    
    ggplot(dat, aes_string(x = "x",       
      y="y_hat*10000",
      group = "year", colour = "year", fill = "year"
      )) +
      geom_line(lwd = 0.75, alpha = 0.8) +
      #scale_y_continuous(expand = c(0,0), limits = c(0, max(dat$y_hat)*110)) +
      #scale_x_continuous(expand = c(0,0)) +
      xlab(variable_label) +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      ylab("Predicted density kg/ha") +
      gfplot::theme_pbs()
  }
}


#' Calculate density curve for time-varying parameters
#'
#' @param m Output of sdmTMB model
#' @param predictor Prefix for scaled parameter in model
#'
#' @export
time_varying_density <- function(m, predictor = "depth") {
  
  get_y_hat <- function(b0, b1, b2, year, 
    predictor, mean_column, sd_column
  ) {
 
   x_pred <- seq(min(m$data[[predictor]], na.rm = TRUE), 
     max(m$data[[predictor]], na.rm = TRUE), length.out = 300)
   
   # exclude curves that are increasing at extremes and therefore not capturing maxima
   maxvalue <- exp(b0 + b1 * max(x_pred) + b2 * (max(x_pred)^2))
   nearmax <- exp(
     b0 + b1 * (max(x_pred) - m$data[[sd_column]][[1]]/10) + 
       b2 * ((max(x_pred) - m$data[[sd_column]][[1]]/10)^2 )
   )
   
   minvalue <- exp(b0 + b1 * min(x_pred) + b2 * (min(x_pred)^2))
   nearmin <- exp(
     b0 + b1 * (min(x_pred) + m$data[[sd_column]][[1]]/10) + 
       b2 * ((min(x_pred) + m$data[[sd_column]][[1]]/10)^2 )
   )

   #browser()
   
   if (nearmax > maxvalue & nearmin > minvalue) {
       
   data.frame(
      # if depth, actually is log_depth so must exp(x) for raw depth
      x = (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]]), 
      # leave these values as predicted for un-trawled zone
      y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2), 
      year = year,
     type = "sad"
    )
       
  } else {
    data.frame(
      # if depth, actually is log_depth so must exp(x) for raw depth
      x = (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]]), 
      # leave these values as predicted for un-trawled zone
      y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2), 
      year = year,
      type = "other"
    )
  }
  }
 
  #r <- m$tmb_obj$report()
  # browser()
  coefs <- as.data.frame(summary(m$sd_report))
  coefs$name <- rownames(summary(m$sd_report))
  b_j <- c(coefs[coefs$name == "b_j", 1 ])
  b_rw_t <- c(coefs[coefs$name == "b_rw_t", 1 ])
  #b_j <- m$model$par
  #n_t <- nrow(r$b_rw_t)
  yrs <- sort(unique(m$data$year))
  n_t <- length(yrs)
  ssid <- m$data$ssid
  
  if(length(b_j)<n_t) { b_j <- rep(0,  n_t)}
  
  pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
    get_y_hat(
      b0 = b_j[.t],
      b1 = b_rw_t[.t],
      b2 = b_rw_t[n_t + .t],
      year = yrs[.t],
      sd_column = paste0(predictor, "_sd"),
      mean_column = paste0(predictor, "_mean"),
      predictor = paste0(predictor, "_scaled")
    )
  })
  
  pred_density
}


#' Calculate density curve for non-time-varying, scaled, quadratic parameters
#'
#' @param m Output of sdmTMB model
#' @param predictor Prefix for scaled parameter in model
#' @param time Variable containing time_steps with separate intercepts
#' @param fixed_params 
#' @param quadratics_only If True doesn't plot 'happy face' quadratics.
#'
#' @export
fixed_density <- function(m, 
  predictor = "temp", 
  time = "year",
  fixed_params = 1, 
  quadratics_only = TRUE 
  ) {
  
  get_y_hat <- function(b0, b1, b2, year, 
    predictor, mean_column, sd_column
  ) {
    
    x_pred <- seq(min(m$data[[predictor]], na.rm = TRUE), 
      max(m$data[[predictor]], na.rm = TRUE), length.out = 300)
    
    if (quadratics_only) {
      
    # exclude curves that are increasing at extremes and therefore not capturing maxima
    maxvalue <- exp(b0 + b1 * max(x_pred) + b2 * (max(x_pred)^2))
    nearmax <- exp(
      b0 + b1 * (max(x_pred) - m$data[[sd_column]][[1]]/10) + 
        b2 * ((max(x_pred) - m$data[[sd_column]][[1]]/10)^2 )
    )
    
    minvalue <- exp(b0 + b1 * min(x_pred) + b2 * (min(x_pred)^2))
    nearmin <- exp(
      b0 + b1 * (min(x_pred) + m$data[[sd_column]][[1]]/10) + 
        b2 * ((min(x_pred) + m$data[[sd_column]][[1]]/10)^2 )
    )
    
    if (nearmax > maxvalue & nearmin > minvalue) {
      
      data.frame(
        # if depth, actually is log_depth so must exp(x) for raw depth
        x = (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]]), 
        # leave these values as predicted for un-trawled zone
        y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2), 
        year = year
      )
      
    } else {
      NULL
    }
    } else {
     
    data.frame(
      # if depth, actually is log_depth so must exp(x) for raw depth
      x = (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]]), 
      # leave these values as predicted for un-trawled zone
      y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2), 
      year = year
    )
    #  browser()  
    }
    
  }

  #r <- m$tmb_obj$report()
  coefs <- as.data.frame(summary(m$sd_report))
  coefs$name <- rownames(summary(m$sd_report))
  b_j <- c(coefs[coefs$name == "b_j", 1 ])
  time_steps <- sort(unique(m$data[[time]]))
  n_t <- length(time_steps)
  n <- 0
  if (fixed_params>1) {
    n <- (fixed_params-1)*2
  }
  b1 <- b_j[n_t + 1 + n]
  b2 <- b_j[n_t + 2 + n]
  
  pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
    get_y_hat(
      b0 = b_j[.t],
      b1 = b1,
      b2 = b2,
      year = time_steps[.t],
      sd_column = paste0(predictor, "_sd"),
      mean_column = paste0(predictor, "_mean"),
      predictor = paste0(predictor, "_scaled")
    )
  })
  pred_density
}

#' Find optimal value from density curve
#'
#' @param dat Dataframe resulting from any of the above density functions
#' @param xlimits Set prior for possible values
#'
#' @export
get_optimal_value <- function(dat, xlimits = c(0, max(dat$x))) {
  dat <- dat %>% filter(x > xlimits[1] & x < xlimits[2])
  dat <- dat %>% mutate(max_y = max(y_hat)*10000, xintercept = x[y_hat*10000 == max_y])
  dat$xintercept[1]
}

#' Find roots of quadratic fixed effect curves
#'
#' @param m Output of sdmTMB model
#' @param predictor Prefix for scaled parameter in model
#' @param fixed_param Number in sequence if model contains multiple quadratic fixed effects
#' @param threshold What proportion of total y to calculate roots for
#' @param plot Logical for plotting quadratic
#'
#' @export
get_quadratic_roots <- function(m, predictor = "do_mlpl", fixed_param = 1, threshold, plot = FALSE, quadratic_only = TRUE) {
  
  sd_column <- paste0(predictor, "_sd")
  mean_column <- paste0(predictor, "_mean")
  scaled <- paste0(predictor, "_scaled")
  x_pred <- seq(min(m$data[[scaled]], na.rm = TRUE), 
    max(m$data[[scaled]], na.rm = TRUE), length.out = 300)
  
  #b_j <- m$model$par
  coefs <- as.data.frame(summary(m$sd_report))
  coefs$name <- rownames(summary(m$sd_report))
  b_j <- c(coefs[coefs$name == "b_j", 1 ])
  
  n <- 0
  if (fixed_param>1) {
    n <- (fixed_param-1)*2
  }
  n_t <- length(unique(m$data$year))
  b <- b_j[[n_t + 1 + n]]
  a <- b_j[[n_t + 2 + n]]
  c <- 1 # intercept doesn't matter; setting to an arbitrary value
  
  
  if (quadratic_only) {
    #browser()
    maxvalue <- exp(c + max(x_pred) * b + max(x_pred)^2 * a) 
    nearmax <- exp(c + (max(x_pred) - m$data[[sd_column]][[1]]/10) * b + (max(x_pred) - m$data[[sd_column]][[1]]/10)^2 * a) 
    minvalue <- exp(c + min(x_pred) * b + min(x_pred)^2 * a) 
    nearmin <- exp(c + (min(x_pred) + m$data[[sd_column]][[1]]/10) * b + (min(x_pred) + m$data[[sd_column]][[1]]/10)^2 * a) 
    
    # exclude curves that are increasing at extremes and therefore not capturing maxima
    if (nearmax < maxvalue | nearmin < minvalue) {
      return(  list(optimal = NA_real_, lower_threshold = NA_real_, upper_threshold = NA_real_, range = NA_real_, prop_max = NA_real_))
    } 
  } 
      

  x <- (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]])
  y <- exp(c + x_pred * b + x_pred^2 * a) 
  crit_y <- y[which(y == max(y))] * threshold
  max_y <- y[which(y == max(y))]
  #   solve for 0 = ax2 + bx + (c - crit_y)


  res <- numeric(length = 2)
  .max <- c - log(max_y)
  res[1] <- -1 * (b + sqrt(b^2 - 4 * .max * a))/(2*a)
  .c <- c - log(crit_y)
  res[2] <- -1 * (b - sqrt(b^2 - 4 * .c * a))/(2*a)
  res[3] <- -1 * (b + sqrt(b^2 - 4 * .c * a))/(2*a)
  
  res[1] <- (res[1] * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]])
  res[2] <- (res[2] * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]])
  res[3] <- (res[3] * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]])
  
if (plot){  
  plot(x, y, type = "l", yaxt='n', xlab=predictor)
    abline(h = crit_y)
    abline(v = res[1], col = "red")
    abline(v = res[2])
    abline(v = res[3])
}  
  
  list(optimal = res[1], lower_threshold = res[2], upper_threshold = res[3], range = res[1]-res[2], prop_max = threshold)
}


# time_varying_density3 <- function(m, predictor = "depth") {
#   get_y_hat <- function(b0, b1, b2, b3, year, 
#     predictor, mean_column, sd_column
#   ) {
#     
#     x_pred <- seq(min(m$data[[predictor]], na.rm = TRUE), max(m$data[[predictor]], na.rm = TRUE), length.out = 300)
#     
#     data.frame(
#       x = (x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]]), # if depth, actually is log_depth so must exp(x) for raw depth
#       y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2 + b3 * x_pred^3), # leave these values as predicted for un-trawled zone
#       year = year
#     )
#     
#   }
#   r <- m$tmb_obj$report()
#   r$b_rw_t
#   b_j <- m$model$par
#   n_t <- nrow(r$b_rw_t)
#   yrs <- sort(unique(m$data$year))
#   ssid <- m$data$ssid
#   
#   pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
#     get_y_hat(
#       b0 = b_j[.t],
#       b1 = r$b_rw_t[.t, 1],
#       b2 = r$b_rw_t[.t, 2],
#       b3 = r$b_rw_t[.t, 3],
#       year = yrs[.t],
#       sd_column = paste0(predictor, "_sd"),
#       mean_column = paste0(predictor, "_mean"),
#       predictor = paste0(predictor, "_scaled")
#     )
#   })
#   pred_density
# }
# 

# #Calculate density curve for scaled, quadratic parameters with non-AR1 year interaction
# fixed_interaction_density <- function(m, predictor = "temp") {
#   get_y_hat <- function(b0, b1, b2, year,
#     predictor, mean_column, sd_column
#   ) {
#     x_pred <- seq(min(m$data[[predictor]], na.rm = TRUE), max(m$data[[predictor]], na.rm = TRUE), length.out = 300)
#     #x_pred <- seq(m$data[[mean_column]][[1]] - 2*m$data[[sd_column]][[1]], m$data[[mean_column]][[1]] + 2*m$data[[sd_column]][[1]], length.out = 300)
# 
#     data.frame(
#       x = x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]],
#       y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2),
#       year = year
#     )
#     #browser()
#   }
# 
#   r <- m$tmb_obj$report()
#   b_j <- m$model$par
#   yrs <- sort(unique(m$data$year))
#   n_t <- length(yrs)
#   b_yrs <- b_j[1:n_t]
#   b_1s <- c(b_j[n_t + 1], b_j[(n_t+3):(n_t*2 + 1)])
#   b_2s <- c(b_j[n_t + 2], b_j[(n_t*2 + 2):(n_t*3)])
# 
# 
#   pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
#     get_y_hat(
#       b0 = b_yrs[.t],
#       b1 = b_1s[.t],
#       b2 = b_2s[.t],
#       year = yrs[.t],
#       sd_column = paste0(predictor, "_sd"),
#       mean_column = paste0(predictor, "_mean"),
#       predictor = paste0(predictor, "_scaled")
#     )
#   })
#   pred_density
#   }

# fixed_density3 <- function(m, predictor = "temp") {
#   get_y_hat <- function(b0, b1, b2, b3, year, 
#     predictor, mean_column, sd_column
#   ) {
#     x_pred <- seq(min(m$data[[predictor]], na.rm = TRUE), max(m$data[[predictor]], na.rm = TRUE), length.out = 300)
#     data.frame(
#       x = x_pred * m$data[[sd_column]][[1]] + m$data[[mean_column]][[1]],
#       y_hat = exp(b0 + b1 * x_pred + b2 * x_pred^2 + b3 * x_pred^3), 
#       year = year
#     )
#     
#   }
#   
#   r <- m$tmb_obj$report()
#   b_j <- m$model$par
#   yrs <- sort(unique(m$data$year))
#   n_t <- length(yrs)
#   
#   pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
#     get_y_hat(
#       b0 = b_j[.t],
#       b1 = b_j[n_t + 1],
#       b2 = b_j[n_t + 2],
#       b3 = b_j[n_t + 3],
#       year = yrs[.t],
#       sd_column = paste0(predictor, "_sd"),
#       mean_column = paste0(predictor, "_mean"),
#       predictor = paste0(predictor, "_scaled")
#     )
#   })
#   pred_density
# }
# 

