library(tidyverse)
library(clusterthat)
library(gfranges)
source("vocc-regression-functions.R")
source("plot-clusters.R")

model <- readRDS("data/trend_by_trend_only_01-17-multi-spp-biotic-vocc-mature.rds")
stats <- readRDS(paste0("data/life-history-stats.rds"))

model2 <- add_colours(model$coefs, species_data = stats)
# ### IF IMMATURE CAN RUN THIS TO MAKE COLOURS MATCH
# model2 <- add_colours(model$coefs, last_used = TRUE )

manipulate::manipulate({
  plot_coefs(model2, fixed_scales = F, order_by = order_by)
},
  order_by = manipulate::picker(as.list(sort(unique(shortener(model2$coefficient)))))
)

glimpse(model2) 

coefs <- model2 %>% 
  select(species, group, depth, age_max, length_99th, weight_99th, coefficient, Estimate) %>% 
  pivot_wider(names_from = coefficient, values_from = Estimate) %>% mutate(status = if_else(`(Intercept)`< 0, "negative", "stable"))

glimpse(coefs) 

# coefs <- filter(coefs, status != "negative")
# coefs <- filter(coefs, status == "negative")

coefs <- coefs[ order(row.names(coefs)), ]

all_coefs <- select(coefs,
  `(Intercept)`, 
  #mean_DO_scaled, 
  DO_trend_scaled, 
  `mean_DO_scaled:DO_trend_scaled`, 
 # mean_temp_scaled, 
  temp_trend_scaled, 
  #`mean_DO_scaled:mean_temp_scaled`, 
  `mean_temp_scaled:temp_trend_scaled`) %>% scale()

temp_coefs <- select(coefs, `(Intercept)`, 
  mean_temp_scaled, temp_trend_scaled, 
  `mean_temp_scaled:temp_trend_scaled`) %>% scale()

do_coefs <- select(coefs, `(Intercept)`, 
  mean_DO_scaled, DO_trend_scaled, 
  `mean_DO_scaled:DO_trend_scaled`) %>% scale()


glimpse(do_coefs) 

factoextra::fviz_nbclust(all_coefs, kmeans, method = "silhouette",
  k.max = 10)

factoextra::fviz_nbclust(all_coefs, cluster::pam, method = "silhouette",
  k.max = 10)


factoextra::fviz_nbclust(temp_coefs, kmeans, method = "silhouette",
  k.max = 10)

factoextra::fviz_nbclust(temp_coefs, cluster::pam, method = "silhouette",
  k.max = 10)


factoextra::fviz_nbclust(do_coefs, kmeans, method = "silhouette",
  k.max = 10)

factoextra::fviz_nbclust(do_coefs, cluster::pam, method = "silhouette",
  k.max = 10)


m_kmeans <- kmeans(all_coefs, 4)
m_pam <- cluster::pam(all_coefs, k = 5L)
m_pam_manhattan <- cluster::pam(all_coefs, k = 5L, metric = "manhattan")

plot_clusters(
  m_kmeans,
#  m_pam,
#  m_pam_manhattan, 
  data = all_coefs, 
  colour_vector = -(coefs$depth),
  text_label = coefs$species,
  colour_label = "Depth"
) + scale_color_viridis_c() 



m_kmeans <- kmeans(temp_coefs, 2)
m_pam <- cluster::pam(temp_coefs, k = 2L)
m_pam_manhattan <- cluster::pam(temp_coefs, k = 2L, metric = "manhattan")

plot_clusters(
  m_kmeans,
  # m_pam,
  # m_pam_manhattan, 
  data = temp_coefs,
  colour_vector = -(coefs$depth),
  text_label = coefs$species,
  colour_label = "Depth"
) + scale_color_viridis_c() 


m_kmeans <- kmeans(do_coefs, 2)
m_pam <- cluster::pam(do_coefs, k = 4)
m_pam_manhattan <- cluster::pam(do_coefs, k = 4, metric = "manhattan")

plot_clusters(
  m_kmeans,
#  m_pam, 
#  m_pam_manhattan, 
  data = do_coefs,
  colour_vector = -(coefs$depth),
  text_label = coefs$species,
  colour_label = "Depth"
) + scale_color_viridis_c() 



# unique(coefs$group)
# + scale_shape_manual(values = c("COD"= 18, "FLATFISH"= 15, "LINGCOD"= 18, "RATFISH"= 17, "ROCKFISH"= 16, "DOGFISH"= 17 , "SABLEFISH"= 18, "SKATE"= 17))

