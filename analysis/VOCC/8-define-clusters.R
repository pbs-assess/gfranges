getwd()
setwd(here::here("analysis", "VOCC"))

library(tidyverse)
library(clusterthat)
library(gfranges)
source("vocc-regression-functions.R")
# source("plot-clusters.R")

# model <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")
# model <- readRDS("data/trend_by_vel_01-16-multi-spp-biotic-vocc-mature.rds")

model <- readRDS(("data/trend_by_all_temp_w_fishing_01-23-all-temp-mature.rds"))
model <- readRDS("data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")


stats <- readRDS(paste0("data/life-history-stats3.rds"))

model2 <- add_colours(model$coefs, species_data = stats)
# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# model2 <- add_colours(model$coefs, last_used = TRUE )

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by)
},
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2$coefficient)))))
)


coefs <- model2 %>% select(species, age, group, depth, age_max, 
    length_99th, weight_99th, coefficient, Estimate) %>% 
  pivot_wider(names_from = coefficient, values_from = Estimate) %>% 
  mutate(status = if_else(`(Intercept)`< 0, "negative", "stable")) 


### subset options ###
# coefs <- filter(coefs, status != "negative")
# coefs <- filter(coefs, status == "negative")
coefs <- filter(coefs_all, age == "mature") %>% ungroup()

# all_coefs <- select(coefs,
#   `(Intercept)`, 
#   mean_DO_scaled, DO_trend_scaled, 
#   `mean_DO_scaled:DO_trend_scaled`, 
#   mean_temp_scaled, temp_trend_scaled, 
#   #`mean_DO_scaled:mean_temp_scaled`, 
#   `mean_temp_scaled:temp_trend_scaled`) %>% scale()


colnames(all_coefs) <- gsub("_scaled", "", colnames(all_coefs))
colnames(all_coefs) <- gsub("mean_", "", colnames(all_coefs))

# all_coefs <- select(coefs,
#   `(Intercept)`, 
#   # mean_DO_scaled, 
#   `scale(squashed_do_vel, center = F)`, 
#   `scale(mean_DO):scale(squashed_do_vel, center = F)`, 
#   # mean_temp_scaled, 
#   `scale(squashed_temp_vel, center = F)`, 
#   #`mean_DO_scaled:mean_temp_scaled`, 
#   `scale(mean_temp):scale(squashed_temp_vel, center = F)`) %>% scale()


#### USE SLOPES INSTEAD OF COEFICIENTS ####
status <- coefs %>% select(species, age, status, `(Intercept)`)
status$species[status$species == "Rougheye/Blackspotted Rockfish Complex"] <- "Rougheye/Blackspotted"


coef_slope_t <- temp_slopes %>% select(species, age, type, chopstick, slope, depth, age_mean, length_50_mat_f) %>% 
  pivot_wider(names_from = c(type, chopstick), values_from = slope) 
coef_slope_d <- do_slopes %>% select(species, age, type, chopstick, slope) %>% 
  pivot_wider(names_from = c(type, chopstick), values_from = slope) 

coefs <- left_join(coef_slope_t, coef_slope_d)
coefs_all <- left_join(coefs, status) %>% rename (intercept = `(Intercept)`)

# coefs <- filter(coefs, status != "negative")
# coefs <- filter(coefs, status == "negative")
coefs <- filter(coefs_all, age == "mature") %>% ungroup()

coefs <- coefs[ order(row.names(coefs)), ]
names(coefs)

coefs$temp_high <- collapse_outliers(coefs$temp_high, c(0.025, 0.975))
coefs$DO_high <- collapse_outliers(coefs$DO_high, c(0.025, 0.975))
coefs$temp_low <- collapse_outliers(coefs$temp_high, c(0.025, 0.975))
coefs$DO_low <- collapse_outliers(coefs$DO_high, c(0.025, 0.975))

hist(coefs$temp_high)
hist(coefs$temp_low)
hist(coefs$DO_low)
hist(coefs$DO_high)

all_coefs <- coefs  %>% select( -species, -age, -depth, -age_mean, -length_50_mat_f, -status) %>% scale()



#### PLOTTING ####

all_coefs <- all_coefs[, c(2:3, 5)]
all_coefs <- all_coefs[, c(2:3)]
all_coefs <- all_coefs[, c(1:4)]


factoextra::fviz_nbclust(all_coefs, kmeans, method = "silhouette",
  k.max = 10)

factoextra::fviz_nbclust(all_coefs, cluster::pam, method = "silhouette",
  k.max = 10)


m_kmeans <- kmeans(all_coefs, 2)
m_kmeans <- kmeans(all_coefs, 3)
m_kmeans <- kmeans(all_coefs, 4)
m_kmeans <- kmeans(all_coefs, 5)
m_kmeans <- kmeans(all_coefs, 6)
m_kmeans <- kmeans(all_coefs, 7)

gfranges::plot_clusters(
  m_kmeans,
  data = all_coefs,  text_label = coefs$species,
  colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = fourth_root_power) 
# + 
#   scale_y_continuous(trans = fourth_root_power) + scale_x_continuous(trans = fourth_root_power)

gfranges::plot_clusters(
  m_kmeans,
  data = all_coefs,  text_label = coefs$species,
  # colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = log10) 
  colour_vector = (coefs$intercept), colour_label = "intercept") + scale_color_viridis_c(direction = -1) 

gfranges::plot_clusters(
  m_kmeans,
  data = all_coefs,  text_label = coefs$species,
  # colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = log10) 
  colour_vector = (coefs$temp_high), colour_label = "temp_high") + scale_color_viridis_c(direction = -1) 

gfranges::plot_clusters(
  m_kmeans,
  data = all_coefs,  text_label = coefs$species,
  # colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = log10) 
  colour_vector = (coefs$DO_low), colour_label = "DO_low") + scale_color_viridis_c(direction = -1) 




m_pam <- cluster::pam(all_coefs, k = 3)
m_pam <- cluster::pam(all_coefs, k = 4)
m_pam <- cluster::pam(all_coefs, k = 5)
m_pam <- cluster::pam(all_coefs, k = 6)
gfranges::plot_clusters(
  m_pam,
  data = all_coefs,
  text_label = coefs$species, #shape_by_group = T,
  colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = fourth_root_power)
  # colour_vector = (coefs$intercept), colour_label = "intercept") + scale_color_viridis_c(direction = -1) 

m_pam_manhattan <- cluster::pam(all_coefs, k = 3, metric = "manhattan")
m_pam_manhattan <- cluster::pam(all_coefs, k = 4, metric = "manhattan")
m_pam_manhattan <- cluster::pam(all_coefs, k = 5, metric = "manhattan")
m_pam_manhattan <- cluster::pam(all_coefs, k = 8, metric = "manhattan")

gfranges::plot_clusters(
  m_pam_manhattan,
  data = all_coefs,
  text_label = coefs$species, #shape_by_group = T,
  colour_vector = (coefs$depth), colour_label = "Depth") + scale_color_viridis_c(direction = -1, trans = fourth_root_power)
# colour_vector = (coefs$intercept), colour_label = "intercept") + scale_color_viridis_c(direction = -1) 



#### TEMP ONLY #### 
# # temp_coefs <- select(coefs, `(Intercept)`, 
# #   temp_grad_scaled, 
# #   # `temp_trend_scaled:temp_grad_scaled`, 
# #   mean_temp_scaled, temp_trend_scaled, 
# #   `temp_trend_scaled:mean_temp_scaled`) %>% 
# #   rename(intercept = `(Intercept)`, 
# #     `mean temp` = mean_temp_scaled, 
# #     `temp trend`= temp_trend_scaled, 
# #     `trend x mean` = `temp_trend_scaled:mean_temp_scaled`, 
# #     # `trend x grad` = `temp_trend_scaled:temp_grad_scaled`, 
# #     gradient = temp_grad_scaled ) %>% 
# #   scale()
# 
# if actually slopes
temp_coefs <- all_coefs[, c(1:2,5)]

factoextra::fviz_nbclust(temp_coefs, kmeans, method = "silhouette",
  k.max = 10)
factoextra::fviz_nbclust(temp_coefs, cluster::pam, method = "silhouette",
  k.max = 10)


m_kmeans <- kmeans(temp_coefs, 4)
gfranges::plot_clusters(m_kmeans, data = temp_coefs,
  colour_vector = (coefs$depth), text_label = coefs$species,
  colour_label = "Depth") +
  scale_color_viridis_c(direction = -1, trans = log10)

m_pam <- cluster::pam(temp_coefs, k = 2L)
m_pam_manhattan <- cluster::pam(temp_coefs, k = 2L, metric = "manhattan")

gfranges::plot_clusters(
  #m_kmeans,
  m_pam,
  # m_pam_manhattan,
  data = temp_coefs,
  colour_vector = (coefs$depth),
  text_label = coefs$species,
  colour_label = "Depth"
) + scale_color_viridis_c(direction = -1, trans = log10)

 

#### DO ONLY #### 
# 
# # do_coefs <- select(coefs, `(Intercept)`, 
# #   mean_DO_scaled, DO_trend_scaled, 
# #   `mean_DO_scaled:DO_trend_scaled`) %>% scale()
#
# ## if actually slopes 
# do_coefs <- all_coefs[, 3:4]
# factoextra::fviz_nbclust(do_coefs, kmeans, method = "silhouette",
#   k.max = 10)
# factoextra::fviz_nbclust(do_coefs, cluster::pam, method = "silhouette",
#   k.max = 10)
# m_kmeans <- kmeans(do_coefs, 2)
# m_pam <- cluster::pam(do_coefs, k = 4)
# m_pam_manhattan <- cluster::pam(do_coefs, k = 4, metric = "manhattan")
# 
# plot_clusters(
#   m_kmeans,
# #  m_pam, 
# #  m_pam_manhattan, 
#   data = do_coefs,
#   colour_vector = (coefs$depth),
#   text_label = coefs$species,
#   colour_label = "Depth"
# ) + scale_color_viridis_c(direction = -1, trans = log10) 
# 


#### nMDS ordination ####

library(smacof)
library(vegan)

rownames(temp_coefs) <- gsub(" Rockfish", "", coefs$species)
rownames(all_coefs) <- gsub(" Rockfish", "", coefs$species)


dist <- vegdist(all_coefs,  method = "euclidean")
temp_dist <- vegdist(temp_coefs,  method = "euclidean")

# automatically performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)
NMDS.scree(temp_dist)

set.seed(54)
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F )
NMDS1

NMDS2 <- metaMDS(temp_dist, k = 2, trymax = 100, trace = F )
NMDS2
#stressplot(NMDS1)
plot(NMDS1, type = "t")
## Fit environmental variables
ef <- envfit(NMDS1, all_coefs)
ef
plot(ef, p.max = 0.25)

plot(NMDS2, type = "t")
## Fit environmental variables
ef <- envfit(NMDS2, temp_coefs)
ef
plot(ef, p.max = 0.25)



# unique(coefs$group)
# + scale_shape_manual(values = c("COD"= 18, "FLATFISH"= 15, "LINGCOD"= 18, "RATFISH"= 17, "ROCKFISH"= 16, "DOGFISH"= 17 , "SABLEFISH"= 18, "SKATE"= 17))

