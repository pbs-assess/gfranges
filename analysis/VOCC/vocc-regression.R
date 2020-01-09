library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))
source("vocc-regression-functions.R")

stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")
stats$genus <- tolower(stats$group)


# model <- "multi-spp-biotic-vocc"
model_age <- "multi-spp-biotic-vocc-mature"
d <- readRDS(paste0("data/", model_age, ".rds"))
d <- na.omit(d) %>% as_tibble()

d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
  filter(species != "Bocaccio") %>%
  filter(species != "Sand Sole") %>%
  filter(species != "Longspine Thornyhead") 


# model_age <- "multi-spp-biotic-vocc-immature"
# d <- readRDS(paste0("data/", model_age, ".rds"))
# d <- na.omit(d) %>% as_tibble()
# 
# d <- suppressWarnings(left_join(d, stats, by = "species")) %>%
#   filter(species != "Curlfin Sole") %>%
#   filter(species != "Longspine Thornyhead")
# 


select(d, genus, species) %>%
  distinct() %>%
  arrange(genus, species) %>% 
  as.data.frame()

# ggplot(d, aes(x, y, colour = temp_vel)) + geom_point() +
#   facet_wrap(~species) +
#   scale_color_viridis_c()
#
# ggplot(d, aes(x, y, colour = biotic_vel)) + geom_point() +
#   facet_wrap(~species) #+ scale_color_viridis_c()


### Local biomass velocity from 2007-2018

# y <- d$biotic_vel
# d$temp_vel_squashed <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))
# x <- model.matrix(~ scale(temp_vel_squashed) + I(scale(temp_vel_squashed)^2), data = d)
# 
# y <- collapse_outliers(y, c(0.005, 0.995))
# hist(y, breaks = 100)
# hist(x[,2], breaks = 100)
# hist(x[,3], breaks = 100)
#
# .d <- d
# .d$y_i <- y
# .d$x_scaled <- x[,2]
# .d$x_scaled_sq <- x[,3]
# ggplot(.d, aes(x_scaled, y_i)) + facet_wrap(~species) + geom_point()
# ggplot(.d, aes(x_scaled_sq, y_i)) + facet_wrap(~species) + geom_point()
#
# cube_root <- function(x) sign(x) * abs(x)^(1/3)
# fourth_root <- function(x) sign(x) * abs(x)^(1/4)
# hist(cube_root(y), breaks = 100)
# hist(cube_root(d$temp_vel), breaks = 100)
#
# hist(fourth_root(y), breaks = 100)
# hist(fourth_root(d$temp_vel), breaks = 100)
#
# ggplot(.d, aes(fourth_root(temp_vel), fourth_root(y_i))) +
#   facet_wrap(~species) + geom_point(alpha = 0.1)
#
# y <- fourth_root(d$biotic_vel)
# x <- model.matrix(~ fourth_root(temp_vel), data = d)
# plot(x[,2], y, col = "#00000005")

# y <- d$biotic_grad
# hist(log(collapse_outliers(y, c(0, 1))), breaks = 100)
#
# hist(collapse_outliers(x[,2], c(0.001, 0.999)), breaks = 100)
# hist(collapse_outliers(y, c(0.01, 0.99)), breaks = 100);abline(v = 0)
#
# # fourth <- function(x) ifelse(x > 0, x^0.25, -(-x)^0.25)
# # hist(fourth(collapse_outliers(y, c(0.01, 0.99))), breaks = 100);abline(v = 0)
#
# y <- d$biotic_trend
# x <- model.matrix(~ scale(temp_vel), data = d)
# bio_temp <- vocc_regression(log(y), x, offset = log(d$biotic_grad),
#   knots = 200, group_by_genus = FALSE, student_t = FALSE)

# y <- d$biotic_vel
# .y <- y
# hist(y, xlim = c(-10, 10), breaks = 2000)
# lims <- c(-2, 2)
# y[.y > lims[2]] <- 1L
# y[.y < lims[1]] <- 0L
# y[.y <= lims[2] & .y >= lims[1]] <- NA
# sum(.y <= lims[2] & .y >= lims[1])/length(y)
# table(y)
#
# d2 <- d[!is.na(y), , drop = FALSE]
# y <- as.numeric(na.omit(y))
# x <- model.matrix(~ temp_vel, data = d2)

y <- collapse_outliers(d$biotic_vel, c(0.005, 0.995))
d$squashed_do_vel <-  collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$squashed_temp_vel <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))

d$do_vel_squashed <-  collapse_outliers(d$DO_vel, c(0.005, 0.995))
d$temp_vel_squashed <-  collapse_outliers(d$temp_vel, c(0.005, 0.995))
plot((do_vel_squashed)~(temp_vel_squashed), data=d, col = "#00000010")

x <- model.matrix(~scale(mean_DO) + scale(mean_temp) + scale(mean_biomass) +
   scale(mean_temp):scale(mean_DO) + 
    # scale(squashed_temp_vel):scale(squashed_do_vel) +
    scale(squashed_do_vel) + scale(squashed_do_vel):scale(mean_DO) + 
    scale(squashed_temp_vel), #+ scale(squashed_temp_vel):scale(mean_temp), 
  data = d)

hist(y)
hist(x[,4])
hist(x[,5])

vel_reg <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)
vel_reg$sdr

vel_reg_genus <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = TRUE, student_t = TRUE, nu = 5)
vel_reg_genus$sdr

get_aic(vel_reg_genus) - get_aic(vel_reg)
get_aic(vel_reg) - get_aic(model)
# saveRDS(vel_reg, file = "data/interacting_vel_with_means_mature_01-06.rds")
# saveRDS(vel_reg, file = "data/interacting_only_with_means_mature_01-06.rds")
saveRDS(vel_reg, file = "data/vel_interacting_with_means_mature_01-07.rds")
saveRDS(vel_reg_genus, file = "data/vel_interacting_with_means_mature_genus_01-07.rds")

# saveRDS(vel_reg, file = "data/bio_temp_mature_01-04.rds")
# saveRDS(vel_reg_genus, file = "data/bio_temp_genus_mature_01-04.rds")
# saveRDS(vel_reg, file = "data/bio_temp_immature_01-04.rds")
# saveRDS(vel_reg_genus, file = "data/bio_temp_genus_immature_01-04.rds")
saveRDS(vel_reg, file = "data/vel_interacting_with_means_immature_01-08.rds")

# ### TRY FOR SD OF LOG BIOMASS
# hist(log(d$sd_est))
# y <- log(d$sd_est)
# 
# sd_reg <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = FALSE, student_t = TRUE, nu = 5)
# sd_reg$sdr
# 
# get_aic(sd_reg)
# get_aic(model)
# # get_aic(vel_reg_genus) - get_aic(sd_reg)
# # get_aic(vel_reg) - get_aic(model)
# # saveRDS(sd_reg, file = "data/sd_interacting_with_means_mature_01-06.rds")
# # saveRDS(sd_reg, file = "data/sd_do_interacting_with_mean_mature_01-06.rds")
# # saveRDS(sd_reg, file = "data/sd_interacting_means_only_mature_01-06.rds")
# saveRDS(sd_reg, file = "data/sd_with_mean_biomass_mature_01-07.rds")
# 



# ### TRY FOR TRENDS INSTEAD OF VOCC
hist(d$temp_trend)
hist(d$DO_trend)
length(d$biotic_trend)
y <- d$biotic_trend

plot((DO_trend)~(temp_trend), data=d, col = "#00000010")
cor(d$DO_trend,d$temp_trend)
d$do_trend <- d$DO_trend
plot((temp_vel_squashed)~(temp_trend), data=d, col = "#00000010")
cor(d$temp_vel_squashed,d$temp_trend)
plot((do_vel_squashed)~(do_trend), data=d, col = "#00000010")
cor(d$do_vel_squashed,d$do_trend)


# #x <- model.matrix(~do_trend + temp_trend + temp_trend:do_trend, data = d)
x <- model.matrix(~scale(mean_temp) + scale(mean_DO) + scale(mean_biomass) +
    scale(mean_temp):scale(mean_DO) + 
    scale(do_trend) + scale(mean_DO):scale(do_trend) +
    scale(temp_trend) + scale(mean_temp):scale(temp_trend)
, data = d)
# length(y)
# hist(y)
# hist(x[,2])
# length(x[,2])
trend_reg <- vocc_regression(d, y, x,
  knots = 200, group_by_genus = FALSE, student_t = FALSE)
trend_reg$sdr
# # saveRDS(trend_reg, file = "data/trend_reg_mature_01-06.rds")
# saveRDS(trend_reg, file = "data/trend_w_means_mature_01-06.rds")
# 
# trend_reg_genus <- vocc_regression(d, y, x,
#   knots = 200, group_by_genus = TRUE, student_t = FALSE)
# trend_reg_genus$sdr
# # saveRDS(trend_reg_genus, file = "data/trend_reg_genus_mature_01-06.rds")

# saveRDS(trend_reg, file = "data/trend_interacting_with_means_mature_01-08.rds")
saveRDS(trend_reg, file = "data/trend_by_vel_mature_simplified_01-09.rds")

# saveRDS(trend_reg, file = "data/trend_interacting_with_means_immature_01-08.rds")

# get_aic(trend_reg_genus) - get_aic(trend_reg)

############################

# model <- readRDS(file = "data/bio_temp_mature_01-04.rds")
# model <- readRDS(file = "data/trend_reg_mature_01-06.rds")

# model <- readRDS(file = "data/interacting_vel_with_means_mature_01-06.rds") #164942.4
# model <- readRDS(file = "data/interacting_only_with_means_mature_01-06.rds") #164943.1
# model <- readRDS(file = "data/interacting_with_means_mature_01-06.rds") #164816.8

model <- readRDS(file =  "data/vel_interacting_with_means_mature_01-07.rds")
model <- readRDS(file = "data/vel_interacting_with_means_immature_01-08.rds")

model <- readRDS(file = "data/trend_interacting_with_means_mature_01-08.rds")
model <- readRDS(file = "data/trend_interacting_with_means_immature_01-08.rds")
# model <- readRDS(file = "data/trend_by_vel_mature_01-08.rds")
model <- readRDS(file = "data/trend_by_vel_mature_simplified_01-09.rds")

model <- vel_reg
#model <- sd_reg
model <- trend_reg
get_aic(trend_reg)
get_aic(model)

coef_names <- (unique(model$coefs$coefficient)) 
coef_names 
model$sdr
model2 <- add_colours(model$coefs) 

model2a <- model2 %>% filter(coefficient != "scale(mean_temp)") %>% 
  filter(coefficient != "scale(mean_DO)") %>% 
  filter(coefficient != "scale(mean_biomass)")
 
manipulate::manipulate({plot_coefs(model2a, order_by = order_by)}, 
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2a$coefficient))))))

model2b <- model2 %>% filter(coefficient %in% 
      c("scale(mean_temp)","scale(mean_DO)", "scale(mean_biomass)"))
manipulate::manipulate({plot_coefs(model2b, order_by = order_by)}, 
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2b$coefficient))))))

manipulate::manipulate({plot_coefs(model2, order_by_trait = TRUE, fixed_scales = FALSE, order_by = order_by)},
  order_by = manipulate::picker(as.list(sort(names(model2[, 6:15])))))

# p <- plot_interaction (model= model, 
#   variables = c("temp_vel_squashed", "do_vel_squashed")
# )

p <- plot_interaction (model = model, 
  variables = c("mean_temp", "squashed_temp_vel"), choose_x = 2
  )

p <- plot_interaction (model = model, variables = c("mean_DO", "squashed_do_vel")
  , choose_x = 2
  ) + scale_colour_manual(values = c("#5E4FA2", "#FDAE61")
  )

p <- plot_interaction (model = model, variables = c("mean_DO", "mean_temp"), choose_x = 1) 

p <- plot_interaction (model = model, variables = c("mean_DO", "mean_temp"), choose_x = 2) + scale_colour_manual(values = c("#5E4FA2", "#FDAE61")
)

# p + ylab("Predicted biotic trend") +
#   # ylab("Predicted biotic variability") +
#   # ylim(-12, 15) +
#   # xlim(-5,6.8) +
#   ggtitle("Interation plots for mature abundance")
# 
# ggsave("figs/interation-plot-vel-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-vel-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-vel-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-vel-by-mean-do.png", width = 10, height = 10, dpi = 300)

# p + ylab("Predicted biotic velocity") +
#   ggtitle("Interation plots for immature abundance")
# 
# ggsave("figs/interation-plot-imm-vel-by-temp-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-vel-by-do-vel.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-vel-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-vel-by-mean-do.png", width = 10, height = 10, dpi = 300)

p <- plot_interaction (model = model, 
  variables = c("mean_temp", "temp_trend"), choose_x = 2
)

p <- plot_interaction (model = model, variables = c("mean_DO", "do_trend")
  , choose_x = 2
) + scale_colour_manual(values = c("#5E4FA2", "#FDAE61")
)


p +
  ylab("Predicted biotic trend") +
  ggtitle("Interation plots for mature abundance")

ggsave("figs/interation-plot-trend-by-temp.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-do.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)


ggsave("figs/interation-plot-trend-by-temp_vel.png", width = 10, height = 10, dpi = 300)
ggsave("figs/interation-plot-trend-by-do-vel.png", width = 10, height = 10, dpi = 300)
# p +
#   ylab("Predicted biotic trend") +
#   ggtitle("Interation plots for immature abundance")

# ggsave("figs/interation-plot-imm-trend-by-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-do.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-temp.png", width = 10, height = 10, dpi = 300)
# ggsave("figs/interation-plot-imm-trend-by-mean-do.png", width = 10, height = 10, dpi = 300)


# print(sdmTMB:::get_convergence_diagnostics(model))
# print(sdmTMB:::get_convergence_diagnostics(model_genus))


library(ggsidekick) # for fourth_root_power
# ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
#   scale_fill_gradient2(trans = "fourth_root_power") +
#   facet_wrap(~species)

ggplot(model$data, aes(x, y, fill = omega_s)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + 
  facet_wrap(~species)

# ggsave("figs/vel-model-omega.png", width = 12, height = 12, dpi = 300)
# ggsave("figs/trend-model-omega.png", width = 12, height = 12, dpi = 300)


model$data %>%
  mutate(resid_upper = quantile(model$data$residual, probs = 0.975)) %>% # compress tails
  mutate(resid_lower = quantile(model$data$residual, probs = 0.025)) %>%  # compress tails
  mutate(residual = if_else(residual > resid_upper, resid_upper, residual)) %>% 
  mutate(residual = if_else(residual < resid_lower, resid_lower, residual)) %>%
  ggplot(aes(x, y, fill = residual)) + geom_tile(width = 4, height = 4) +
  scale_fill_gradient2() + 
  facet_wrap(~species)
# ggsave("figs/vel-model-residuals.png", width = 12, height = 12, dpi = 300)
# ggsave("figs/trend-model-residuals.png", width = 12, height = 12, dpi = 300)

norm_resids <- qres_student(model)
norm_resids <- norm_resids[is.finite(norm_resids)]
# qqnorm(norm_resids)
hist(norm_resids)

norm_resids <- qres_student(model_genus)
norm_resids <- norm_resids[is.finite(norm_resids)]
# qqnorm(norm_resids)
hist(norm_resids)

# qqnorm(model$data$residual)
# qqline(model$data$residual)



### SAVE PLOT WITH SELECTED PARAMS
# model2 <- model2 %>% 
#   #filter(coefficient != "scale(squashed_temp_vel)")
#   #filter(coefficient != "scale(squashed_do_vel)")
#   filter(coefficient != "scale(mean_DO):scale(mean_temp)")

# model3 <- plot_coefs(model2, order_by = "scale(do_vel_squashed)")
# model3 <- plot_coefs(model2, order_by = "scale(temp_vel_squashed)")
# model3 <- plot_coefs(model2, order_by = "temp_vel_squashed:do_vel_squashed")
# model3 <- plot_coefs(model2, order_by = "scale(mean_temp):scale(squashed_temp_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_DO):scale(mean_temp)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_DO):scale(squashed_do_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(squashed_temp_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(squashed_do_vel)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_temp)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_DO)")
# model3 <- plot_coefs(model2, order_by = "scale(mean_biomass)")
# model_plot <- model3 + 
#   ggtitle(paste("Mature biotic velocity")) #+ 
#   #ggtitle(paste("Mature biotic variability")) 
# model_plot 
# 
# ggsave("figs/worm-plot-vel-do-vel-with-biomass.png", width = 10, height = 10, dpi = 300)
# 
# model3 <- plot_coefs(model2, order_by = "scale(do_trend)")
# model3 <- plot_coefs(model2, order_by = "scale(temp_trend)")
# #model3 <- plot_coefs(model2, order_by = "scale(do_trend):scale(temp_trend)")
# model_plot <- model3 + ggtitle(paste("Mature biomass trend"))
# model_plot 





model <- readRDS(file = "data/model_immature_01-04.rds")

model2 <- add_colours(model$coefs, last_used = TRUE )
model3 <- plot_coefs(model2, order_by = "scale(do_vel_squashed)")
model_plot <- model3 + ggtitle(paste("Immature Biotic velocity by VOCC")) + facet_wrap(~coefficient, scales = "free_x") #
model_plot 

model3 <- plot_coefs(model2, order_by = "scale(temp_vel_squashed)")
model_plot <- model3 + ggtitle(paste("Immature Biotic velocity by VOCC")) + facet_wrap(~coefficient, scales = "free_x") #
model_plot 


model2 <- add_colours(model_genus$coefs)
model3 <- plot_coefs(model2)
model_plot_g <- model3 + ggtitle(paste("Mature biotic velocity by VOCC with genus re")) + facet_wrap(~coefficient, scales = "free_x")
model_plot_g

# p <- plot_interaction (model= model, #species = "North Pacific Spiny Dogfish",
#   variables = c("temp_vel_squashed", "do_vel_squashed"),
#   scaled = TRUE
# )
# p + theme(legend.position = "top",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.direction = "vertical") +
#   guides(colour = guide_legend(nrow= 2, ncol= 2)) +
#   ggtitle("Interation plots for immature abundance")

## Ordered by increasing max weight and split by rockfish or not
# model3 <- plot_coefs(model2, order_by = "max_weight")
# model_plot <- model3 + ggtitle(paste("Biotic velocity by thermal VOCC ordered by increasing max weight")) +
#   facet_wrap(~rockfish, scales="free_y")
# model_plot

## Ordered by increasing max age and split by rockfish or not
# model_age <- na.omit(model2)
# model3 <- plot_coefs(model_age, order_by = "max_age")
# model_plot <- model3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of max age")) + facet_wrap(~rockfish, scales = "free_y")
# model_plot


# ## Ordered by increasing depth and split by rockfish or not
# model3 <- plot_coefs(model2, order_by = "large_depth")
# model_plot <- model3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# model_plot


# ## Ordered by increasing depth and split by rockfish or not
# bio_do3 <- plot_coefs(bio_do2, order_by = "large_depth")
# bio_do_plot <- bio_do3 + ggtitle(paste("Biotic velocity by DO VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# bio_do_plot



### Local biomass trend from 2007-2018

### Local biomass variability from 2007-2018

y <- d$biotic_CV
x <- model.matrix(~ scale(temp_vel), data = d)

CV_temp <- vocc_regression(y, x, knots = 100)
CV_temp2 <- add_colours(CV_temp$coefs)
CV_temp3 <- plot_coefs(CV_temp2)
CV_temp_plot <- CV_temp3 + ggtitle(paste("Biotic CV by thermal VOCC"))
CV_temp_plot


y <- d$biotic_CV
x <- model.matrix(~ scale(DO_vel), data = d)

CV_do <- vocc_regression(y, x, knots = 100)
CV_do2 <- add_colours(CV_do$coefs)
CV_do3 <- plot_coefs(CV_do2)
CV_do_plot <- CV_do3 + ggtitle(paste("Biotic CV by DO VOCC"))
# CV_do_plot <- NULL

png(
  file = paste0("figs/", model, "100-knots-29-spp.png"),
  res = 600,
  units = "in",
  width = 8.5,
  height = 11
)
gridExtra::grid.arrange(
  grobs = c(list(
    bio_temp_plot,
    bio_do_plot,
    trend_temp_plot,
    trend_do_plot,
    CV_temp_plot,
    CV_do_plot
  )),
  nrow = 3,
  top = grid::textGrob(paste(model, " knots = 100"))
)
dev.off()
