getwd()
setwd(here::here("/analysis/VOCC/data"))

covs <- "-fixed"
mydir = paste0( "_lags", covs)
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
fixed <- do.call(rbind, lapply(myfiles[21:40], read.csv)) %>% 
  select(-X, -lat, -lon)
glimpse(fixed)


#hist(dist$lag) 


fixed <- mutate(fixed, model = "fixed-depth")

plot_data <- filter(fixed, method == "gradient")

plot_data <- filter(fixed, method == "distance")

ggplot(plot_data, aes(bioclim_vel, biotic_vel, colour = start_year)) +
  geom_abline(slope = 1, intercept = 0) +
  #geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.25 ) + 
  geom_smooth(method=lm) +
  #facet_grid(species~start_year, scales = "free") + 
  facet_grid(species~start_year) + 
  coord_fixed() +
  scale_x_continuous(trans = sqrt, limits = c(-100,100)) + 
  scale_y_continuous(trans = sqrt, limits = c(-100,100)) + 
  # scale_x_continuous(trans = log10, limits = c(-200,200)) + 
  # scale_y_continuous(trans = log10, limits = c(-200,200)) + 
  # geom_vline(aes(xintercept = 0)) +
  # geom_hline(aes(yintercept = 0)) +
  theme_bw() + ylab("biotic velocity")+ xlab("bioclimtic velocity")

ggplot(plot_data) + 
  geom_abline(slope = 1, intercept = 0) +
  #geom_abline(slope = 0, intercept = 0) +
  geom_point(aes(DO_vel, biotic_vel, colour = start_year), alpha = 0.25 ) + 
  geom_smooth(aes(DO_vel, biotic_vel, colour = start_year), method=lm) +
  #facet_grid(species~start_year, scales = "free") + 
  facet_grid(species~start_year) + 
  coord_fixed() +
  scale_x_continuous(trans = sqrt, limits = c(-50,50)) + 
  scale_y_continuous(trans = sqrt, limits = c(-50,50)) + 
  # scale_x_continuous(trans = sqrt, limits = c(-100,100)) + 
  # scale_y_continuous(trans = sqrt, limits = c(-100,100)) + 
  theme_bw() + ylab("biotic velocity")+ xlab("DO velocity")

ggplot(plot_data) + 
  geom_abline(slope = 1, intercept = 0) +
  #geom_abline(slope = 0, intercept = 0) +
  geom_point(aes(temp_vel, biotic_vel, colour = start_year), alpha = 0.25 ) + 
  geom_smooth(aes(temp_vel, biotic_vel, colour = start_year), method=lm) +
  #facet_grid(species~start_year, scales = "free") + 
  facet_grid(species~start_year) + 
  coord_fixed() +
  scale_x_continuous(trans = sqrt, limits = c(-50,50)) +
  scale_y_continuous(trans = sqrt, limits = c(-50,50)) +
  # scale_x_continuous(trans = sqrt) + 
  # scale_y_continuous(trans = sqrt) + 
  theme_bw() + ylab("biotic velocity")+ xlab("temperature velocity")


year_list <- sort(unique(fixed$start_year))

cor_dfs <- list()
for (a in seq_len(length(year_list))) {
fixed_multi <- filter(fixed, start_year == year_list[[a]])
# fixed_multi <- filter(fixed, start_year == 2009)

species_list <- levels(unique(fixed_multi$species))

temp_coef <- list()
for (i in seq_len(length(species_list))) {
spp_df <- filter(fixed_multi, species == species_list[[i]])
x <- spp_df$temp_vel
y <- spp_df$biotic_vel
coords <- spp_df[ ,1:2]
temp_coef[i] <- SpatialPack::cor.spatial(x, y, coords)
temp_coef

#z <- SpatialPack::codisp (x , y , coords )
#ccf (x , y , ylab = " cross - correlation " , max . lag = 20)
#plot ( z )
}

do_coef <- list()
for (i in seq_len(length(species_list))) {
  spp_df <- filter(fixed_multi, species == species_list[[i]])
  x <- spp_df$DO_vel
  y <- spp_df$biotic_vel
  coords <- spp_df[ ,1:2]
  do_coef[i] <- SpatialPack::cor.spatial(x, y, coords)
  do_coef
}

bioclim_coef <- list()
for (i in seq_len(length(species_list))) {
  spp_df <- filter(fixed_multi, species == species_list[[i]])
  x <- spp_df$bioclim_vel
  y <- spp_df$biotic_vel
  coords <- spp_df[ ,1:2]
  bioclim_coef[i] <- SpatialPack::cor.spatial(x, y, coords)
  bioclim_coef
}

cor_dfs[[a]] <- bind_cols(species = unlist(species_list), temp_biotic = unlist(temp_coef), do_biotic = unlist(do_coef), bioclim_biotic = unlist(bioclim_coef))

cor_dfs 
}


names(cor_dfs) <- year_list
# cor <- cbind(species = unlist(species_list), temp_biotic = c(unlist(temp_coef)), do_coef = c(unlist(do_coef)), bioclim_coef = c(unlist(bioclim_coef)))

cor2 <- cor %>% mutate_if(is.numeric, round, digits = 3)
cor2
glimpse(cor2)


fixed2 <- fixed %>% select( -bioclim_trend, -biotic_trend, 
  -bioclim_grad, -biotic_grad, -DO_grad, -temp_grad)
glimpse(fixed2)
dist <- do.call(rbind, lapply(myfiles[1:20], read.csv)) %>% select(-X)
glimpse(dist)
dist <- mutate(dist, model = "fixed-depth")
fixed2 <- mutate(fixed2, model = "fixed-depth")

fixed <- rbind(fixed2, dist)

ggplot(fixed) + 
  geom_boxplot(aes(as.factor(start_year), lag, fill= start_year)) + 
 # geom_violin(aes(as.factor(start_year), lag, fill= start_year)) + 
  facet_grid(species~method) + 
  scale_y_continuous(trans = sqrt, limits = c(-240,240)) + 
  theme_bw() + ylab("biotic - bioclimatic")

ggplot(fixed) + 
  geom_boxplot(aes(as.factor(start_year), biotic_vel, fill= start_year)) + 
  #geom_violin(aes(as.factor(start_year), biotic_vel, fill= start_year)) + 
  facet_grid(species~method) + 
  scale_y_continuous(trans = sqrt, limits = c(-240,240)) + 
  theme_bw() + ylab("biotic velocity")

ggplot(fixed) + 
  geom_boxplot(aes(as.factor(start_year), bioclim_vel, fill = start_year)) + 
  #geom_violin(aes(as.factor(start_year), bioclim_vel, fill = start_year)) + 
  facet_grid(species~method) + 
  scale_y_continuous(trans = sqrt, limits = c(-240,240)) + 
  theme_bw() + ylab("bioclimatic velocity")




# Compare with time-varying depth
# covs <- "-tv-depth"
# mydir2 = paste0( "_lags", covs)
# myfiles2 = list.files(path=mydir2, pattern="*.rds", full.names=TRUE)
# myfiles2
# tv_depth <- do.call(rbind, lapply(myfiles2[1:5], read.csv))
# tv_depth <- mutate(tv_depth, model = "tv-depth")


# all <- rbind(fixed, tv_depth)
#  boxplot(lag~start_year, data=result, ylim=c(-100,200))
#  boxplot(lag~start_year, data=result2, ylim=c(-100,200))
#  
#  ggplot(fixed) + geom_violin(aes(as.factor(start_year), lag)) + ylim(-100,200)
#  
#  ggplot(all) + geom_violin(aes(as.factor(start_year), lag, fill= start_year)) + facet_wrap(~model) +scale_y_continuous(trans = sqrt) + theme_bw() + ylab("biotic - bioclimatic")
#  

 
