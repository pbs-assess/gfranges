setwd(here::here("analysis", "VOCC"))

if (!require(patchwork)) install.packages("patchwork")
if (!require(ggpubr)) install.packages("ggpubr")

library(TMB)
library(tidyverse)
library(gfranges)
library(patchwork)
library(ggpubr)

model <- readRDS("data/trend-all-95-all-do-04-11-trend-with-do-family-family-1-500.rds")

stats <- readRDS(paste0("data/life-history-behav.rds"))
model2 <- add_colours(model$coefs, species_data = stats)

temp_slopes <- chopstick_slopes(model,
  x_variable = "temp_trend_scaled",
  interaction_column = "temp_trend_scaled:mean_temp_scaled", type = "temp"
)
do_slopes <- chopstick_slopes(model,
  x_variable = "DO_trend_scaled",
  interaction_column = "DO_trend_scaled:mean_DO_scaled", type = "DO"
)
temp_slopes1 <- left_join(temp_slopes, stats)
do_slopes1 <- left_join(do_slopes, stats)

long_slopes <- rbind(temp_slopes1, do_slopes1) %>% mutate(slope_type = paste(chopstick, type)) %>% ungroup()

long_slopes <- long_slopes %>% mutate(
  type = factor(type, levels = c("temp", "DO")),
  species_age = paste(species, age),
  age = factor(age, levels = c("immature", "mature"),  labels = c("Immature", "Mature")),
  Rockfish = factor(rockfish, levels = c("rockfish", "other fishes"),  labels = c("Rockfish", "Other fishes")),
  slope_type = factor(slope_type, levels = c("high temp", "low temp", "high DO", "low DO")),
  Diet = factor(Diet, levels = c("Zooplankton", "Generalist", "Polychaetes", "Crustaceans", "Fish")),
  Zone = factor(BenthoPelagicPelagicDemersal, levels = c("Demersal", "Benthopelagic", "Pelagic")),
  Latitude = factor(NorthMiddleSouth, levels = c("North", "Middle", "South")),
  Schooling = as.factor(Schooling),
  Trophic = factor(if_else(Diet == "Zooplankton", "Lower", "Higher"), levels = c("Lower", "Higher")),
  Specialist = factor(if_else(Diet == "Generalist", "Generalist", "Specialist"), levels = c("Generalist", "Specialist")),
  log_age_scaled = scale(log(age_mean + 1), center = T),
  max_mass_scaled = scale(log(weight_99th + 1), center = T),
  growth_rate_scaled = scale(log((length_50_mat_f / age_mat)+1), center = T)
  )

# collapse Middle and South together because only 3 "southern" species
long_slopes$Latitude[long_slopes$Latitude == "Middle"] <- "South"

# collapse Pelagic into Benthopelagic because only 3 "pelagic" species
long_slopes$Zone[long_slopes$Zone == "Pelagic"] <- "Benthopelagic"

long_slopes <- long_slopes %>% mutate(
  Zone = factor(Zone, levels = c("Demersal", "Benthopelagic")),
  Latitude = factor(Latitude, levels = c("North", "South"))
)

long_slopes <- long_slopes %>% group_by(slope_type) %>% mutate(slope_trim = collapse_outliers(slope, c(0.025, 0.975))) %>% ungroup()
# long_slopes <- long_slopes %>% rename(slope_raw = slope, slope = slope_trim)

temp_slopes <- long_slopes %>% filter(type == "temp")
do_slopes <- long_slopes %>% filter(type == "DO") %>% mutate (chopstick = factor(chopstick, levels = c("low", "high")))

#### Which slopes have non-zero effects? ####
temp_slopes %>% lmerTest::lmer(slope ~ 0 + chopstick + (1|species) + (1|species_age), data = .) %>% summary() # *
do_slopes %>% lmerTest::lmer(slope ~ 0 + chopstick + (1|species) + (1|species_age), data = .) %>% summary() # *

# high temp and low DO have sig non-zero effects (in both cases negative) 
best_slopes <- long_slopes %>% filter( slope_type == "high temp" | slope_type == "low DO") 


#### WILCOX TESTS OF OVERALL EFFECTS ####
# preliminary impressions from the following code
# intercept slightly negative, for northern, immature, zooplankton eating pops at high temperatures
# less negative for low temp, moderate latitudes
# more negative for generalists and piscivores and those foraging in Benthopelagic zone


#### Is there an effect of age? ####
#### paired test of coefficients by age 
# only effect of mean DO differs with age

matched_temp_coef <- model2 %>% filter(species != "Pacific Halibut") %>% filter(species != "Curlfin Sole") %>% filter(species != "Shortbelly Rockfish") %>% filter(species != "Shortraker Rockfish") %>% filter(species != "Spotted Ratfish") %>% filter(parent_taxonomic_unit != "rajidae(skates)") #%>% filter(coefficient == "temp_trend_scaled")

ggpaired(matched_temp_coef, x = "age", y = "Estimate", 
  color = "age", id = "species", 
  ylab = "Estimate", line.color = "gray", line.size = 0.4, facet.by = c("coefficient") ) + 
  stat_compare_means(
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)),
    method = "t.test", 
    paired = TRUE, 
    ref.group = NULL) + scale_colour_manual(values = c( "darkcyan", "royalblue4")) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_wrap(vars(coefficient), scales = "free") + theme(legend.position = "none") +
  gfplot::theme_pbs() + theme(legend.position = "none") + scale_y_continuous(expand = c(0.1, 0))



#### paired test of slopes by age
# no sig differences by age

matched_temp <- long_slopes %>% filter(species != "Pacific Halibut") %>% filter(species != "Curlfin Sole") %>% filter(species != "Shortbelly Rockfish") %>% filter(species != "Shortraker Rockfish") %>% filter(species != "Spotted Ratfish") %>% filter(parent_taxonomic_unit != "rajidae(skates)") %>% filter(type == "temp")
compare_means(slope~age, matched_temp, method = "wilcox.test", paired = T, id = "species")

matched_do <- long_slopes %>% filter(species != "Pacific Halibut") %>% filter(species != "Curlfin Sole") %>% filter(species != "Shortbelly Rockfish") %>% filter(species != "Shortraker Rockfish") %>% filter(species != "Spotted Ratfish") %>% filter(parent_taxonomic_unit != "rajidae(skates)") %>% filter(type == "DO")
compare_means(slope~age, matched_do, method = "wilcox.test", paired = T, id = "species")

matched <- rbind(matched_temp, matched_do)
ggpaired(matched, x = "age", y = "slope",  color = "slope_type", id = "species", ylab = "Slope", line.color = "gray", line.size = 0.4, facet.by = c("slope_type") ) + 
  stat_compare_means(
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    method = "t.test", 
    paired = TRUE, 
    ref.group = NULL) + scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_grid(cols = vars(slope_type), scales = "free") + theme(legend.position = "none")


#### Is there a difference in strength of effect between high and low slopes? ####
# YES
# only sig for DO in mature fishes with t test
# only non-sig for temp in mature fishes with wilcox test

# paired across species and ages
compare_means(slope~chopstick, temp_slopes, method = "wilcox.test", paired = T, id= "species_age") # **
compare_means(slope~chopstick, do_slopes, method = "wilcox.test", paired = T, id= "species_age") # **

ggplot(temp_slopes, aes(chopstick,slope,  colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") ) + scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  gfplot:::theme_pbs() + 
  stat_compare_means(method = "wilcox.test", 
    paired = TRUE, ref.group = NULL) + 
  theme(plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    # legend.position = "none"
    legend.title = element_blank(),
    legend.position = c(.1, .25)
  ) 


## split by age classes 
ggpaired(long_slopes, x = "chopstick", y = "slope",  color = "slope_type", id = "species_age", ylab = "Slope", line.color = "gray", line.size = 0.4, facet.by = c("type", "age") ) + 
  stat_compare_means(
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # method = "t.test",
    method = "wilcox.test",
    paired = TRUE, 
    ref.group = NULL) + scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_grid(rows = vars(type), cols = vars(age), scales = "free") + theme(legend.position = "none")


#### within more extreme slopes test test unpaired effects of ecology ####
# t test
#temp
compare_means(slope~Latitude, filter(temp_slopes, chopstick == "high"), method = "t.test") # * North - Middle
compare_means(slope~Rockfish, filter(temp_slopes, chopstick == "high"), method = "t.test")
compare_means(slope~Schooling, filter(temp_slopes, chopstick == "high"), method = "t.test")
compare_means(slope~Zone, filter(temp_slopes, chopstick == "high"), method = "t.test") # * Demersal - Benthopelagic
compare_means(slope~Diet, filter(temp_slopes, chopstick == "high"), method = "t.test") # * Zooplankton - Crustaceans
compare_means(slope~Trophic, filter(temp_slopes, chopstick == "high"), method = "t.test") 
compare_means(slope~Specialist, filter(temp_slopes, chopstick == "high"), method = "t.test") 

# DO
compare_means(slope~Latitude, filter(do_slopes, chopstick == "low"), method = "t.test")
compare_means(slope~Rockfish, filter(do_slopes, chopstick == "low"), method = "t.test")
compare_means(slope~Schooling, filter(do_slopes, chopstick == "low"), method = "t.test")
compare_means(slope~Zone, filter(do_slopes, chopstick == "low"), method = "t.test") 
compare_means(slope~Diet, filter(do_slopes, chopstick == "low"), method = "t.test") 
compare_means(slope~Trophic, filter(do_slopes, chopstick == "low"), method = "t.test") 
compare_means(slope~Specialist, filter(do_slopes, chopstick == "low"), method = "t.test") 

# wilcox test
#temp
compare_means(slope~Latitude, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Rockfish, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Schooling, filter(temp_slopes, chopstick == "high"), method = "wilcox.test")
compare_means(slope~Zone, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") # ** Demersal - Benthopelagic
compare_means(slope~Diet, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") # * Zooplankton - Crustaceans
compare_means(slope~Trophic, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 
compare_means(slope~Specialist, filter(temp_slopes, chopstick == "high"), method = "wilcox.test") 

# DO
compare_means(slope~Latitude, filter(do_slopes, chopstick == "low"), method = "wilcox.test")
compare_means(slope~Rockfish, filter(do_slopes, chopstick == "low"), method = "wilcox.test")
compare_means(slope~Schooling, filter(do_slopes, chopstick == "low"), method = "wilcox.test")
compare_means(slope~Zone, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 
compare_means(slope~Diet, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 
compare_means(slope~Trophic, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 
compare_means(slope~Specialist, filter(do_slopes, chopstick == "low"), method = "wilcox.test") 


#########################################
#########################################
#### MIXED MODELS WITHOUT INTERCEPTS ####
#########################################
#########################################

# # including age doesn't change anything
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick * age + (1|species), data = .) %>% summary() # *
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick * age + (1|species), data = .) %>% summary() # *

ann_text <- data.frame(slope = c(-11.5,-3.7), 
  chopstick = c("High", "Low"), tag = c("*","*"), slope_type = c("high temp", "low DO"),
  type = factor(c("temp","DO"),levels = c("temp","DO")))

(p1 <- long_slopes %>% mutate(chopstick = factor(chopstick, levels = c("low", "high"), labels = c("Low", "High"))) %>%
 ggplot( aes(slope, chopstick, colour = slope_type, fill = slope_type )) + 
  ylab("Mean\nClimate") +
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
    # legend.title = element_blank(),
    # legend.position = c(.6, .3)
  ) )
### MATURITY ####

### No sig diff between maturity classes although only immature show consitant non-zero effects ####
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + age + (1|species) , data = .) %>% summary() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + age + (1|species) , data = .) %>% summary()
filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + age + (1|species), data = .) %>% summary()
filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + age + (1|species), data = .) %>% summary()

ann_text <- data.frame(slope = c(-11.5,-3.7), 
  age = c("Immature", "Immature"), tag = c("-","*"),
  type = factor(c("temp","DO"),levels = c("temp","DO")))

p2 <- ggplot(best_slopes, aes(slope, age, colour = slope_type, fill = slope_type )) + 
  ylab("Maturity") +
  geom_text(data = ann_text, aes(slope, age , label = tag), size = 6, inherit.aes = F) + 
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  # ggtitle("Range limits") +
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 


### LATITUDE ####

# use moscaic plots to check how unbalanced groups are
# library(ggmosaic)
plot(Latitude~Rockfish, data = filter(best_slopes, type == "temp")) 
plot(Latitude~Zone, data = filter(best_slopes, type == "temp")) 

# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Latitude + (1|species) + (1|species_age), data = .) %>% summary() # *
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% anova() # *
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Latitude * Zone + (1|species), data = .) %>% anova() 
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Latitude * Rockfish + (1|species), data = .) %>% anova()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Latitude + (1|species), data = .) %>% summary() # *

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Latitude + (1|species) + (1|species_age), data = .) %>% summary() 
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Latitude + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Latitude = c("North"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("*", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))


best_slopes <- mutate(best_slopes, Latitude = factor(Latitude, levels = c("South", "North")))

p3 <- ggplot(best_slopes, aes(slope, Latitude, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  ylab("Latitude") +
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### ROCKFISH ####
plot(Rockfish~age, data = filter(best_slopes, type == "temp")) 

### No sig diff between rockfish and other fishes but rockfish do show consitant non-zero effects more than other ####
# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species) + (1|species_age), data = .) %>% summary()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Rockfish + (1|species), data = .) %>% anova() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species), data = .) %>% summary() # *

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Rockfish + (1|species), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Rockfish + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Rockfish = c("Rockfish"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("-", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p4 <- ggplot(best_slopes, aes(slope, Rockfish, colour = slope_type, fill = slope_type )) + 
  ylab("Rockfish") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### SOCIALITY ####
plot(Schooling~age, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Latitude, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Zone, data = filter(best_slopes, type == "temp")) 
plot(Schooling~Rockfish, data = filter(best_slopes, type == "temp")) 

### No sig diff between groups but schooling fishes do show consitant non-zero effects more than solitary ####
# temp_slopes %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species) + (1|species_age), data = .) %>% summary() # .
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Schooling + (1|species), data = .) %>% anova() 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species), data = .) %>% summary() # *

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Schooling + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Schooling + age + (1|species), data = .) %>% summary()
ann_text <- data.frame(
  Schooling = c("Schooling"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("-", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p5 <- ggplot(best_slopes, aes(slope,  Schooling, colour = slope_type, fill = slope_type )) + 
  ylab("Schooling") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin( alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### ZONE ####
plot(Zone~age, data = filter(best_slopes, type == "temp")) 
plot(Zone~Schooling, data = filter(best_slopes, type == "temp")) 
plot(Zone~Rockfish, data = filter(best_slopes, type == "temp")) 

temp_slopes %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species) + (1|species_age), data = .) %>% summary() # **

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone + (1|species), data = .) %>% anova() # **
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Schooling + (1|species), data = .) %>% anova() 
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Rockfish + (1|species), data = .) %>% anova()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species), data = .) %>% summary() # ***

# do_slopes %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species) + (1|species_age), data = .) %>% summary()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Zone + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Schooling + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone * Rockfish + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 0 + Zone + (1|species), data = .) %>% summary()

ann_text <- data.frame(
  Zone = c("Benthopelagic"), 
  slope_type = c("high temp"),
  slope = c(-11.5, -3.7), tag = c("*", " "), 
  type = factor(c("temp", "DO"), levels = c("temp","DO")))

p6 <- ggplot(best_slopes, aes(slope,  Zone, colour = slope_type, fill = slope_type )) + 
  ylab("Zone") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) 

### DIET ####
# not sig on its own, 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species) + (1|species_age), data = .) %>% anova() # .
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species), data = .) %>% anova()

### but not all diets are represented in all zones, in north, or amoungst rockfish
plot(Diet~age, data = filter(best_slopes, type == "temp"))
plot(Diet~Rockfish, data = filter(best_slopes, type == "temp"))
plot(Diet~Latitude, data = filter(best_slopes, type == "temp"))
plot(Diet~Zone, data = filter(best_slopes, type == "temp")) 

# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Diet * Zone + (1|species), data = .) %>% anova()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope_trim ~ 1 + Diet * Rockfish + (1|species), data = .) %>% anova() # *
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope_trim ~ 1 + Diet * Latitude + (1|species), data = .) %>% anova()

# 
## split by age shows effects are only in immatures 
# filter(temp_slopes, age == "Immature") %>% #filter(Zone == "Benthopelagic") %>% 
#   # filter(chopstick == "high") %>%
#   lm(slope_trim ~ 1 + Diet + Zone, data = .) %>% anova() # ***
filter(temp_slopes, age == "Immature") %>% #filter(Zone == "Benthopelagic") %>% 
  # filter(chopstick == "high") %>%
  lm(slope_trim ~ 0 + Diet + Zone, data = .) %>% summary() # ***

filter(temp_slopes, age == "Mature") %>% #filter(Zone == "Benthopelagic") %>%
#   # filter(chopstick == "high") %>%
  lm(slope ~ 1 + Diet , data = .) %>% anova()
 

# do_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species) + (1|species_age), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Diet + (1|species), data = .) %>% anova()
# filter(do_slopes, chopstick == "low") %>% lmerTest::lmer(slope ~ 1 + Diet * Zone * age + (1|species), data = .) %>% anova()

ann_text <- data.frame(slope = c(-11.5, -11.5, -3.7),
  Diet = c("Zooplankton", "Fish", "Fish"), 
  Trophic = c("Lower", "Higher", "Higher"), 
  Specialist = c("Specialist", "Specialist", "Specialist"), 
  slope_type = c("high temp", "high temp", "low DO"), 
  tag = c(" ", " ", " "),
  type = factor(c("temp", "temp", "DO"), levels = c("temp","DO")))

(p7 <- ggplot(best_slopes, aes(slope, Diet, colour = slope_type, fill = slope_type)) + 
  ylab("Diet") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
  gfplot:::theme_pbs() + 
  theme(#axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) )

(p8 <- ggplot(best_slopes, aes(slope, Trophic, colour = slope_type, fill = slope_type)) + 
  ylab("Trophic-level") +
  scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
  scale_fill_manual(values = c("Red 3", "darkcyan")) + 
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_vline(xintercept = 0, colour = "grey") +
  # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
  geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
  scale_x_continuous(expand = c(0.1, 0)) +
  facet_grid(cols = vars(type), scales = "free") + 
  geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
  gfplot:::theme_pbs() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
  ) )

p9 <- ggplot(best_slopes, aes(slope, Specialist, colour = slope_type, fill = slope_type)) + 
    ylab("Diet") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
    gfplot:::theme_pbs() + 
    theme(#axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) 
p9b <- ggplot(best_slopes, aes(slope, Specialist, colour = slope_type, fill = slope_type)) + 
    ylab("Diet") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") +
    gfplot:::theme_pbs() + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) 
(p7b <- ggplot(filter(best_slopes, Diet != "Generalist"), aes(slope, Diet, colour = slope_type, fill = slope_type)) + 
    ylab("Specialist type") +
    scale_colour_manual(values = c("Red 3", "darkcyan") ) + 
    scale_fill_manual(values = c("Red 3", "darkcyan")) + 
    geom_violin(alpha = 0.1) + #scale = "width",
    geom_vline(xintercept = 0, colour = "grey") +
    # geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
    geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
    scale_x_continuous(expand = c(0.1, 0)) +
    facet_grid(cols = vars(type), scales = "free") + 
    geom_text(data = ann_text, aes(label = tag), size = 6, colour = "black") + 
    gfplot:::theme_pbs() + 
    theme(#axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      strip.text.x = element_blank(),
      plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
      legend.position = "none"
    ) )

# p1 + p2 + p3 + p4 + p5 + p6 + p7 + patchwork::plot_layout(ncol=1, heights = c(1, 1, 1, 1, 1, 1, 2.3)) 
# p1 + p2 + p3 + p4 + p5 + p6 + p8 + p9 + patchwork::plot_layout(ncol=1, ) 
p1 + p2 + p3 + p4 + p5 + p6 + p8 + p9b + p7b + patchwork::plot_layout(ncol=1, heights = c(1, 1, 1, 1, 1, 1, 1, 1, 2.3)) 

ggsave(here::here("ms", "figs", "behav-slope-diff-zero2.pdf"), width = 5, height = 10 )


#### ALL SIG EFFECT COMBINED #### 

temp_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + age + Zone + Latitude + chopstick + Schooling + Rockfish + (1|species) + (1|species_age), data = .) %>% anova() # .
temp_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + Zone + Latitude + chopstick + age + (1|species) + (1|species_age), data = .) %>% anova() # .

temp_slopes %>% lmerTest::lmer(slope ~ 0 + Diet + age + Zone + Latitude + chopstick + (1|species) + (1|species_age), data = .) %>% summary()  # .
temp_slopes %>% lmerTest::lmer(slope ~ 0 + Diet + Zone + Latitude + chopstick + (1|species) + (1|species_age), data = .) %>% summary()  # .

# temperature has stronger -ve effects for northern benthopelagic piscivores 

do_slopes %>% lmerTest::lmer(slope ~ 1 + Diet + age + Zone + Latitude + chopstick + Schooling + Rockfish + (1|species) + (1|species_age), data = .) %>% anova() # .
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + age + (1|species) + (1|species_age), data = .) %>% anova() # .
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + age + (1|species) + (1|species_age), data = .) %>% summary() 

# no patterns for DO
#########################################
#### Add in continuous covariatiates #### 

# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Zone + Latitude + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + log_age_scaled + (1|species) + (1|species_age), data = .) %>% anova()
# 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Zone + Latitude + max_mass_scaled + growth_rate_scaled * age + log_age_scaled + (1|species) + (1|species_age), data = .) %>% anova()
# 
# 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Zone + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species) + (1|species_age), data = .) %>% anova()
# 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Zone + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species) + (1|species_age), data = .) %>% summary()
# 
########################################
# # full model with both chopsticks (mean effects) 
# temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species) + (1|species_age), data = .) %>% anova()
temp_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species) + (1|species_age), data = .) %>% summary()

### models for just highest temperatures ####
# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 +  Zone * growth_rate_scaled + Zone * max_mass_scaled + Latitude * growth_rate_scaled + growth_rate_scaled * age + log_age_scaled * max_mass_scaled + (1|species), data = .) %>% anova()

# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 +  Zone + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 +  Zone + Latitude + growth_rate_scaled * age + log_age_scaled + (1|species), data = .) %>% summary()

# filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone + Latitude + growth_rate_scaled * max_mass_scaled + growth_rate_scaled * age + (1|species), data = .) %>% anova()


# with only high temp chosticks, zone is almost sig...
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% anova()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% summary()
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope_trim ~ 1 + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% summary() # retains sig

# hist(temp_slopes$slope)
#### explore zone and diet combinations ####

temp_slopes %>% filter(chopstick == "high") %>% 
  # filter(age == "Immature") %>%
ggboxplot(x = "Diet", y = "slope",  color = "slope_type", id = "species", 
  ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(rows = vars(Zone), cols = vars(age),
    scales = "free") +
  stat_compare_means(
    ref.group = "Zooplankton",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
    ) + theme(legend.position = "none")

# Specialists
temp_slopes %>% filter(chopstick == "high") %>% 
  # filter(age == "Immature") %>%
  ggboxplot(x = "Specialist", y = "slope",  color = "slope_type", id = "species", 
    ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  # scale_y_continuous(trans = fourth_root_power) +
  facet_grid(rows = vars(Zone), cols = vars(age),
    scales = "free") +
  stat_compare_means(
    ref.group = "Specialist",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
  ) + theme(legend.position = "none")

# trophic level 
ggboxplot(filter(temp_slopes, chopstick == "high"), x = "Trophic", y = "slope",  color = "slope_type", id = "species", 
  ylab = "Slope", repel = T, facet.by = c("Zone")) + 
  scale_colour_manual(values = c("Red 3", "royalblue4", "goldenrod1", "darkcyan")) + 
  geom_point() +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  facet_grid(rows = vars(Zone), #cols = vars(Diet), 
    scales = "free") + 
  scale_y_continuous(trans = fourth_root_power) +
  stat_compare_means(
    ref.group = "Low",
    aes(label = paste0("p = ", ..p.format..)),
    # aes(label = paste0(..p.signif..)), 
    # label.x = c(0.1,0.25,0.5,0.75,0.9), #sort(unique(temp_slopes$Diet))# method = "t.test",
    method = "wilcox.test"
  ) + theme(legend.position = "none")

#### try adding to above 
filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic * Zone + Specialist * Zone + Latitude + max_mass_scaled + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic * Zone + Specialist + Zone + Latitude + max_mass_scaled + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Trophic + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% anova()

filter(temp_slopes, chopstick == "high") %>% lmerTest::lmer(slope ~ 1 + log_age_scaled + Specialist + Zone + Latitude + growth_rate_scaled * age + (1|species), data = .) %>% anova()


#### DO WITH CONTIUOUS ####

# bigger adults = more negative
# faster growing juvi = more negative
# but collapsing outliers loses sig

#### stepwise ####
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + log_age_scaled  + (1|species) + (1|species_age), data = .) %>% anova() # .
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + (1|species) + (1|species_age), data = .) %>% anova() # .
# 
# do_slopes %>% lmerTest::lmer(slope ~ 1 + chopstick + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + (1|species) + (1|species_age), data = .) %>% summary() # .
######
do_slopes %>% lmerTest::lmer(slope ~ 0 + chopstick + max_mass_scaled * age + growth_rate_scaled * age + (1|species) + (1|species_age), data = .) %>% summary() 
do_slopes %>% lmerTest::lmer(slope_trim ~ 0 + chopstick + max_mass_scaled * age + growth_rate_scaled * age + (1|species) + (1|species_age), data = .) %>% summary() ### not sig anymore

### only for low DO = same answer but stronger effects ####

filter(do_slopes, chopstick == "low")  %>% lmerTest::lmer(slope ~ 1 + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + log_age_scaled  + (1|species), data = .) %>% anova() 

filter(do_slopes, chopstick == "low")  %>% lmerTest::lmer(slope ~ 1 + max_mass_scaled * age + growth_rate_scaled * age + max_mass_scaled * growth_rate_scaled + (1|species), data = .) %>% summary() 

filter(do_slopes, chopstick == "low")  %>% lmerTest::lmer(slope ~ 1 + max_mass_scaled * age + growth_rate_scaled * age + (1|species), data = .) %>% summary() 

filter(do_slopes, chopstick == "low")  %>% lmerTest::lmer(slope_trim ~ 1 + max_mass_scaled * age + growth_rate_scaled * age + 
    (1|species), data = .) %>% summary()  ### not sig anymore

##############################
#### OLD PLOT CODE
##############################
### split by maturity too ####


p1 <- ggplot(data = filter(long_slopes, type == "temp"), aes(slope, Latitude, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") )+ scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  # ggtitle("Range limits") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    strip.text.y = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    legend.position = "none"
    # legend.title = element_blank(),
    # legend.position = c(.15, .15)
  ) 
p1d <- ggplot(data = filter(long_slopes, type == "DO"), aes(slope, Latitude, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("goldenrod1", "darkcyan") )+ scale_fill_manual(values = c("goldenrod1", "darkcyan")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0.1, "cm"),
    # legend.position = "none"
    legend.title = element_blank(),
    legend.position = c(.15, .15)
  ) 

p0 <- ggplot(data = filter(long_slopes, type == "temp"), aes(slope, Rockfish, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") )+ scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  # ggtitle("Sociality") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 
p0d <- ggplot(data = filter(long_slopes, type == "DO"), aes(slope, Rockfish, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("goldenrod1", "darkcyan") )+ scale_fill_manual(values = c("goldenrod1", "darkcyan")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 
p2 <- ggplot(data = filter(long_slopes, type == "temp"), aes(slope, Schooling, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") )+ scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  # ggtitle("Sociality") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 
p2d <- ggplot(data = filter(long_slopes, type == "DO"), aes(slope, Schooling, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("goldenrod1", "darkcyan") )+ scale_fill_manual(values = c("goldenrod1", "darkcyan")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p3 <- ggplot(data = filter(long_slopes, type == "temp"), aes(slope, Zone, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") )+ scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  # ggtitle("Foraging zone") +
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 
p3d <- ggplot(data = filter(long_slopes, type == "DO"), aes(slope, Zone, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("goldenrod1", "darkcyan") )+ scale_fill_manual(values = c("goldenrod1", "darkcyan")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  gfplot:::theme_pbs() + 
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
    axis.text.x = element_blank(),
    # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p4 <- ggplot(data = filter(long_slopes, type == "temp"), aes(slope, Diet, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("Red 3", "royalblue4") )+ scale_fill_manual(values = c("Red 3", "royalblue4")) + 
  geom_vline(xintercept = 0, colour = "grey") +
  #  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
  # ggtitle("Diet") +
  gfplot:::theme_pbs() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

p4d <- ggplot(data = filter(long_slopes, type == "DO"), aes(slope, Diet, colour = slope_type, fill = slope_type )) + 
  scale_colour_manual(values = c("goldenrod1", "darkcyan") )+ scale_fill_manual(values = c("goldenrod1", "darkcyan")) + 
  geom_vline(xintercept = 0, colour = "grey") +
#  geom_boxplot(alpha = 0.12, outlier.colour = "white", notch = F) +
  geom_violin(alpha = 0.1) + #scale = "width",
  geom_point(position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.075), alpha=0.8) +
  facet_grid(cols = vars(age), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) +
  gfplot:::theme_pbs() + 
  theme(#axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) 

# p1 + p2 + p3 + p4 + p1d + p2d + p3d + p4d + patchwork::plot_layout(ncol = 4, nrow = 2, widths = c(1, 0.66, 0.66, 0.9, 1.5)) 
p1 + p1d + p0 + p0d + p2 + p2d + p3 + p3d + p4 + p4d + patchwork::plot_layout(ncol = 2, nrow = 5, widths = c(1, 0.9), heights = c(1, 0.66, 0.66, 1, 1.5)) 

ggsave(here::here("ms", "figs", "behav-slope-boxplots-trim.pdf"), width = 7, height = 6 )
# Cggsave(here::here("ms", "figs", "behav-slope-boxplots-imm.pdf"), width = 12, height = 5)


###################
### basic boxplots ####
# p0 <- ggplot(long_slopes, aes(age, slope, colour =slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Maturity") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     
#     # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# 
# p2 <- ggplot(long_slopes, aes(Schooling, slope, colour =slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Sociality") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     
#     # plot.subtitle = element_text(hjust = 0.5, vjust = 0.4),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# p3 <- ggplot(long_slopes, aes(Zone, slope, colour = slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) + 
#   ggtitle("Foraging zone") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     strip.text.y = element_blank(),
#     plot.margin = margin(0.1, 0, 0.1, 0, "cm"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# 
# p4 <- ggplot(long_slopes, aes(Diet, slope, colour = slope_type, fill = slope_type )) + 
#   # geom_violin( alpha = 0.1) + #scale = "width",
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_boxplot(alpha = 0.2, outlier.colour = "white", notch = F) +
#   geom_point(position=position_jitterdodge(jitter.width = 0.075), alpha=0.8) +
#   # scale_y_continuous(trans = fourth_root_power) +
#   scale_colour_brewer(palette = "Spectral", direction = 1) + scale_fill_brewer(palette = "Spectral", direction = 1) +
#   # scale_colour_viridis_d(begin = .2, end = .8) + scale_fill_viridis_d(begin = .2, end = .8) +
#   facet_grid(rows = vars(type), scales = "free") + scale_x_continuous(expand = c(0.2,0.2)) +
#   ggtitle("Diet") +
#   gfplot:::theme_pbs() + 
#   theme(plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),
#     plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none"
#   ) 
# p1 + p0 + p2 + p3 + p4 + patchwork::plot_layout(nrow = 1, widths = c(1, 0.66, 0.66, 0.9, 1.5))
# 
# ggsave(here::here("ms", "figs", "behav-slope-boxplots.pdf"), width = 13, height = 5)
# ggsave(here::here("ms", "figs", "behav-slope-boxplots-imm.pdf"), width = 12, height = 5)


###################