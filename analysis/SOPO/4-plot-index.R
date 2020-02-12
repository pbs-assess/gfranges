getwd()
setwd(here::here("/analysis/SOPO"))
library("dplyr")
library("ggplot2")


all_indices <- readRDS(paste0("data/sopo-2019-indices.rds"))
# View(all_indices)

all_indices <- filter(all_indices, covs == "-no-covs")
# all_indices <- filter(all_indices, covs != "-no-covs")

scale <- 1000000 * 4 /1000 # if density was in kg/m2: m2 to km2 * 4 km2 grid size and kg to tonnes

all_indices$upr[is.na(all_indices$se) ] <- NA
all_indices$upr[all_indices$se > 1.25 ] <- NA

all_indices$reliable_est <- all_indices$est
all_indices$reliable_est[all_indices$se > 1.25 ] <- NA

# filter years after 2004 because incomplete spatial sampling prior causes errors to blow up
indices <- all_indices %>% filter(year > 2004) %>%
  # and calculate index to base ratio on (max, mean, or median)
  group_by(species, maturity) %>% mutate( max_est = max(est), min_est = min(est), mean_est = mean(est)) %>% ungroup()

# add back in est for years prior because potentially still interesting despite uncertainty
est_only <- all_indices %>% select(year, species, maturity, est) %>% filter(year < 2005)
indices <- full_join(indices, est_only , by=c("species", "year", "maturity", "est")) 

indices <- indices %>% group_by(species, maturity) %>% mutate (max_upr = max(upr, na.rm = T), max_est2 = max(est), axis_max = pmax(max_upr, max_est2)) %>% ungroup()

# calculate ratio of immature est to mature est for plotting on same figure
ind <- indices %>% filter(maturity != "immature")
ind_imm <- indices %>% filter(maturity == "immature")

ind_spp <- list()
for(i in unique(ind$species)) {
  rm(ind_i)
  rm(ind_ii)
  ind_i <- filter(ind, species == i) %>% select(mean_est)
  ind_ii <- filter(ind_imm, species == i) %>% select(max_est)
   # browser()
  if(nrow(ind_ii)<1) { ind_ii <- 0 }
  
  ratio <- ind_i[[1]]/ind_ii[[1]]
  
  ind_spp[[i]] <- filter(ind, species == i)
  ind_spp[[i]]$ratio <- ratio[[1]]
}

ind2 <- do.call(rbind, ind_spp)

all_indices1 <- left_join(ind2, ind_imm, by=c("species", "year", "ssid", "covs"))

# add life history stats for sorting variables
stats <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/life-history-stats.rds")

all_indices2 <- left_join(all_indices1, stats)
# joint[is.na(joint)] <- 0
# View(all_indices2)

# function to plot with facet wrap
plot_indices <- function(data, 
  ad_col = "deepskyblue4",
  imm_col = "orangered"
  ) {
  
  # make dataframe of ratios by species for plotting labels on facets
  ratio_lab <- data %>% select(species, ratio, axis_max.x, axis_max.y) %>% unique()
  ratio_lab$ratio <- round(ratio_lab$ratio)
  ratio_lab <- mutate(ratio_lab, max.y = axis_max.y*ratio, maxy = pmax(axis_max.x, max.y))
  
  ratio_lab$ratio[ ratio_lab$ratio == Inf ] <- NA
  ratio_lab <- na.omit(ratio_lab)
  # browser()
  ggplot(data, aes(year, est.x*scale)) + 
    geom_line(col = ad_col, linetype = 2) +
    geom_line(aes(year, reliable_est.x*scale), col = ad_col) +
    geom_line(aes(year, est.y*scale*ratio), col = imm_col, linetype = 2) +
    geom_line(aes(year, reliable_est.y*scale*ratio), col = imm_col) +
    geom_ribbon(aes(ymin = lwr.x*scale, ymax = upr.x*scale), fill = ad_col, alpha = 0.4) +
    facet_wrap(~species, scales="free_y") +  
    # adding the relative humidity data, transformed to match roughly the range of the temperature
    geom_ribbon(aes(ymin = lwr.y*scale*ratio, ymax = upr.y*scale*ratio), 
      fill = imm_col, alpha = 0.4) + 
    # if adding the secondary axis must revert the above transformation, but doesn't work with facets
    # scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Immature biomass estimate (metric tonnes)")) +
    xlab('Year') + 
    ylab('Mature biomass estimate (metric tonnes)') + 
    geom_text(data = ratio_lab, aes(x = 2012,  y = maxy*scale, label = paste("relative biomass of immatures = 1/", ratio))) +
    scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(limits = c(2004, 2020)) +
    gfplot::theme_pbs(base_size = 16)
}

# plot different subsets of species


data <- all_indices2
plot_indices(data)

data <- filter(all_indices2, group == "ROCKFISH")
plot_indices(data)

data <- filter(all_indices2, group != "ROCKFISH") %>% filter(group != "FLATFISH")
plot_indices(data)

# data <- filter(all_indices2, group == "FLATFISH")
# plot_indices(data)

# grad <- all_indices %>% group_by(species, maturity) %>% select(species, maturity, covs, max_gradient, bad_eig) %>% unique()
# sort(grad$max_gradient)
# View(grad)
# 
# grad_depth <- filter(grad, covs != "-no-covs") #%>% filter(maturity == "mature")

