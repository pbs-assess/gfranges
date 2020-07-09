# test ogive
library(tidyverse)
library(ggplot2)

# # if make-figs not just run
setwd(here::here())
species <- c("Bocaccio")
species <- c("Redbanded Rockfish")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
fish <- readRDS(paste0("analysis/VOCC/raw/bio-data-", spp, ""))
events <- readRDS(paste0("analysis/VOCC/data/", spp,"/data-by-maturity-", spp, "-1n3n4n16.rds"))
m1 <- fit_mat_ogive_re(fish, type = "length", sample_id_re = F, year_re = TRUE)

m2 <- fit_mat_ogive_re(fish, type = "length", sample_id_re = TRUE, year_re = TRUE)


cor(glmmTMB::ranef(m1$model)$cond$year[,1],glmmTMB::ranef(m2$model)$cond$year[,1])

summary(m1$model)
summary(m2$model)
plot_mat_ogive(m1)
plot_mat_ogive(m2) 

# # change code so sample_id_re applies ssid as random effect
# m3 <- fit_mat_ogive_re(fish, type = "length", sample_id_re = TRUE, year_re = TRUE)
# 
# gfranges::plot_mat_ogive(m3) 
# 
# m5 <- fit_mat_ogive_re(filter(fish, year > 2006), type = "length", sample_id_re = F, year_re = TRUE)
# gfranges::plot_mat_ogive(m5) 

data <- readRDS("data/bocaccio/data-by-maturity-bocaccio-1n3n4n16.rds")
ggplot(maturity$data, aes(length, weight, colour = as.factor(sex))) +
  geom_point(size = 1.5, alpha = 0.35, shape = 1) +
  geom_point(aes(length, weight), shape = 16, size = 1.25, alpha = 0.65) +
  scale_color_viridis_d(begin = 0.1, end = 0.6) +
  facet_wrap(~year) + gfplot::theme_pbs() +
  xlab("") + ylab("Weight (open circles are estimates)") + labs(colour = "Sex") 


ggplot(filter(fish, !is.na(length)), aes(forcats::fct_reorder(as.factor(fishing_event_id), length, mean), length)) + 
  geom_boxplot() + 
  geom_hline(yintercept = m1$mat_perc$mean$f.mean.p0.5, colour = "red") +
  geom_hline(yintercept = m1$mat_perc$mean$m.mean.p0.5, colour = "blue") +
  # annotate("rect", xmin=min(dat$fishing_event_id), xmax=max(dat$fishing_event_id), ymin = 45.5, ymax = 57.7, alpha = 0.1) + 
  facet_wrap(~year, scales = "free_x", nrow = 3) + 
  gfplot::theme_pbs() + theme(axis.text.x = element_blank()) 




# simulated example
set.seed(123)
df = expand.grid("year"=1:10,"tow"=1:50)
df$year_tow = as.numeric(as.factor(paste0(df$tow,df$year)))

dat = data.frame("year_tow"=sample(1:500,size=10000,replace=T),
  length = rnorm(10000,50,10))
dat = dplyr::left_join(dat,df)

dat$pred = plogis(-5 + rnorm(10,0.3,sd=0.2)[dat$year] + 0.1*dat$length)
dat$y = ifelse(runif(nrow(dat)) < dat$pred,1,0)

fit1 = glmmTMB::glmmTMB(y ~ length + (1|year), family="binomial", data=dat)
fit2 = glmmTMB::glmmTMB(y ~ length + (1|year) + (1|year_tow), family="binomial", data=dat)
cor(glmmTMB::ranef(fit1)$cond$year[,1],glmmTMB::ranef(fit2)$cond$year[,1])
hist(dat$length)

# try to cluster sizes
unique(dat$year_tow)
dat <- dat %>% #group_by(year_tow) %>% 
  mutate(skew_val = year_tow/400, new_length = length*skew_val)
dat$new_pred = plogis(-5 + rnorm(10,0.3,sd=0.2)[dat$year] + 0.1*dat$new_length)
dat$new_y = ifelse(runif(nrow(dat)) < dat$new_pred,1,0)

ggplot(dat, aes(forcats::fct_reorder(as.factor(tow), length), new_length)) + geom_boxplot() + facet_wrap(~year)

fit3 = glmmTMB::glmmTMB(new_y ~ new_length + (1|year), family="binomial", data=dat)
fit4 = glmmTMB::glmmTMB(new_y ~ new_length + (1|year) + (1|year_tow), family="binomial", data=dat)
cor(glmmTMB::ranef(fit3)$cond$year[,1],glmmTMB::ranef(fit4)$cond$year[,1])
hist(dat$length)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
