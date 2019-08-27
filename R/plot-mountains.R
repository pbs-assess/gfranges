#' Predicted curves for parameters in sdmTMB models
#'
#' @param dat Dataframe of density estimates containing x, y_hat, and year
#' @param time_varying Logical for if the parameter of interest is time-varying
#' @param variable_label Descriptive label for parameter "x"
#' @param xlimits X-axis limits. Default is c(0, max(dat$x)).
#'
#' @export
#'
plot_mountains <- function(dat, time_varying = TRUE, variable_label = "Depth", xlimits = c(0, max(dat$x))) {

  if (time_varying) {
    # mountain_scaler <- max(dat$y_hat)  
    # #mountain_scaler <- mountain_scaler * 10
    dat <- dat %>% group_by(year) %>% mutate(max_y = max(y_hat), xintercept = x[y_hat == max_y])
    dat$year <- as.factor(dat$year)
    
    ymaximum <- max(dat$y_hat[dat$x > xlimits[1] & dat$x < xlimits[2]])
    
    # ALTERNATIVE METHOD FOR DETERMINING YMAX. Way worse than above!
    #rangex <- max(dat$x) - min(dat$x)
    #edge_buffer <- rangex*ymax_trim_fact # @param ymax_trim_fact Proportion of extreme x values to exclude before setting ymax.
    #ymaximum <- max(dat$y_hat[dat$x > (min(dat$x)+edge_buffer) & dat$x < (max(dat$x)-edge_buffer )])
    
    ggplot(dat, aes_string(x = "x", 
      y="y_hat*100",
      # ymax = "10*y_hat + year*mountain_scaler", ymin = "0 + year*mountain_scaler",
      # # ymax = "y_hat*mountain_scaler", ymin = "0",
      group = "year", colour = "year", fill = "year"
    )) +
      # # geom_ribbon(lwd = 0.75, alpha = 0.01) +
      geom_line(lwd = 0.75, alpha = 0.8) +
      geom_vline(aes_string(xintercept = "xintercept", group = "year", colour = "year"), linetype =2) +
      scale_y_continuous(limits = c(0, ymaximum*100 )) +
      #scale_y_continuous(limits = c(0, median(dat$y_hat*100)*20)) +
      scale_x_continuous(expand = c(0,0), limits = xlimits) +
      xlab(variable_label) +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      ylab("Predicted density g/ha ")+
      gfplot::theme_pbs() 
    
  } else {
    #browser()
    dat$year <- as.factor(dat$year)
    
    ggplot(dat, aes_string(x = "x",       
      y="y_hat",
      group = "year", colour = "year", fill = "year"
      )) +
      geom_line(lwd = 0.75, alpha = 0.8) +
      #scale_y_continuous(expand = c(0,0), limits = c(0, max(dat$y_hat)*110)) +
      #scale_x_continuous(expand = c(0,0)) +
      xlab(variable_label) +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      ylab("Predicted density g/ha") +
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
      year = year
    )
       
  } else {
     NULL
  }
  }
   
  r <- m$tmb_obj$report()
  r$b_rw_t
  b_j <- m$model$par
  n_t <- nrow(r$b_rw_t)
  yrs <- sort(unique(m$data$year))
  ssid <- m$data$ssid
  
  pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
    get_y_hat(
      b0 = b_j[.t],
      b1 = r$b_rw_t[.t, 1],
      b2 = r$b_rw_t[.t, 2],
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
#'
#' @export
#'
fixed_density <- function(m, predictor = "temp") {
  
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
        year = year
      )
      
    } else {
      NULL
    }
  }
  
  r <- m$tmb_obj$report()
  b_j <- m$model$par
  yrs <- sort(unique(m$data$year))
  n_t <- length(yrs)
  
  pred_density <- purrr::map_df(seq_len(n_t), function(.t) {
    get_y_hat(
      b0 = b_j[.t],
      b1 = b_j[n_t + 1],
      b2 = b_j[n_t + 2],
      year = yrs[.t],
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
  dat <- dat %>% mutate(max_y = max(y_hat), xintercept = x[y_hat == max_y])
  dat$xintercept[1]
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

