library(TMB)
library(dplyr)
library(ggplot2)
library(gfranges)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")


stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats <- stats %>% separate(species_science_name, " ", into = c("genus","specific"))
stats$group[stats$group == "SHARK"] <- "DOGFISH"
# stats$group[stats$group == "SHARK"] <- "SHARKS & SKATES"
# stats$group[stats$group == "SKATE"] <- "SHARKS & SKATES"
stats$group[stats$group == "HAKE"] <- "COD"


##############################
#### LOAD MODELS ####

#### ONE JUST BUILT
model <- new_model

nrow(model$data)

model2 <- add_colours(model$coefs) %>%
  # filter(coeffZicient != "mean_DO_scaled" ) %>%
  # filter(coefficient != "temp_grad_scaled" ) %>%
  #   filter(coefficient != "mean_temp_scaled" ) %>% 
  filter(coefficient != "log_biomass_scaled" )

# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# mature <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")
# model2 <- add_colours(mmature$coefs) # must be saved as model2 for use in function below
# model2 <- add_colours(model$coefs, last_used = TRUE)

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by) 
  #+ ylim(-0.05,0.095)
}, order_by = manipulate::picker( 
  as.list(sort(unique(shortener(model2$coefficient))), decreasing=F))
)


# manipulate::manipulate({
#   plot_coefs(model2, order_by_trait = T, fixed_scales = F, order_by = order_by)
# },
# order_by = manipulate::picker(as.list(sort(names(model2[, 6:15]))))
# )

# model2a <- model2 %>%
#   filter(coefficient != "mean_temp_scaled") %>%
#   filter(coefficient != "mean_DO_scaled") %>%
#   filter(coefficient != "mean_biomass_scaled") %>%
#   filter(coefficient != "mean_temp_scaled:mean_DO_scaled") %>%
#   filter(coefficient != "mean_DO_scaled:mean_temp_scaled")
# manipulate::manipulate({plot_coefs(model2a, order_by = order_by)},
#   order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))

# model2b <- model2 %>% filter(coefficient %in%
#    c("mean_temp_scaled", "mean_DO_scaled", "mean_biomass_scaled",
#    "mean_temp_scaled:mean_DO_scaled", "mean_DO_scaled:mean_temp_scaled"))
# manipulate::manipulate({plot_coefs(model2b, order_by = order_by)},
#   order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))

### SAVE PLOT WITH SELECTED PARAMS
# model3 <- plot_coefs(model2, order_by = "squashed_temp_vel_scaled)")
# model_plot <- model3 +
#   ggtitle(paste("Mature biomass trend"))
#   # ggtitle(paste("Immature biotic trend"))
# model_plot
# ggsave("figs/worm-plot-temp-trend-sort.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/worm-plot-imm-temp-trend-sort.png", width = 10, height = 10, dpi = 300)


##############################
#### CHECK SAMPLE SIZE AND DISTRIBUTION OF MODEL DATA
##############################
# nrow(model$data)
# mean(model$data$mean_biomass)
# range(model$data$mean_biomass)
# hist(log(model$data$mean_biomass))
# hist((model$data$biotic_trend))
# hist((model$data$temp_trend))


##############################
#### MODEL COEFFICIENTS ####
##############################

coef_names <- shortener(unique(model$coefs$coefficient))
betas <- signif(as.list(model$sdr, "Estimate")$b_j, digits = 3)
SE <- signif(as.list(model$sdr, "Std. Error")$b_j, digits = 3)
lowerCI <- signif(betas + SE * qnorm(0.025), digits = 3)
upperCI <- signif(betas + SE * qnorm(0.975), digits = 3)
overall_betas <- as.data.frame(cbind(coef_names, betas, SE, lowerCI, upperCI))
overall_betas

get_aic(model)

##############################
#### CHECK MODEL RESIDUALS ####
##############################
# # use 6-vocc-regression code to save pdf of all relevant plots

ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2(trans = fourth_root_power) + gfplot::theme_pbs() +
  facet_wrap(~species)

# # ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# # ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)


if (is.null(model$data$residual)){
r <- model$obj$report()
model$data$residual <- model$y_i - r$eta_i
model$data$eta <- r$eta_i
}

 ggplot(model$data, aes(eta, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(biotic_trend)) + geom_histogram() + 
    facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(temp_trend, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free") 
  
  ggplot(model$data, aes(DO_trend, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_temp, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_DO, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_temp^2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(mean_DO^2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_biomass_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_biomass_scaled2, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(fishing_trend_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(log_effort_scaled, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
  
  model$data %>%
    mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
    mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>% # compress tails
    mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>%
    mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
    ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
    scale_fill_gradient2() + gfplot::theme_pbs() +
    facet_wrap(~species)


if(y_type == "vel") {

  ggplot(model$data, aes(squashed_biotic_vel)) + geom_histogram() +   
    facet_wrap(~species, scales = "free")
  
  ggplot(model$data, aes(squashed_temp_vel, residual)) + geom_point(alpha =0.2) + 
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free") 
  
  ggplot(model$data, aes(squashed_DO_vel, residual)) + geom_point(alpha =0.2) +
    geom_smooth(method= "loess") + facet_wrap(~species, scales = "free")
}


norm_resids <- qres_student(model)
norm_resids <- norm_resids[is.finite(norm_resids)]
qqnorm(norm_resids)
hist(norm_resids)
 
# qqnorm(model$data$residual)
# qqline(model$data$residual)
