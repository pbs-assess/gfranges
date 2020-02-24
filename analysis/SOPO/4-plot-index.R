getwd()
setwd(here::here("/analysis/SOPO"))
library("dplyr")
library("ggplot2")
library("grid")
options(scipen = 999)
# options("scipen"=100, "digits"=8)

unit_conversion <- 1000000 * 4 /1000 # if density was in kg/m2: m2 to km2 * 4 km2 grid size and kg to tonnes

all_indices <- readRDS(paste0("data/sopo-2019-indices2.rds"))
# View(all_indices)

# all_indices <- filter(all_indices, covs == "-no-covs")
# all_indices <- filter(all_indices, covs != "-no-covs")
all_indices <- filter(all_indices, covs == "-no-covs-300") %>%
  # filter unreliable imm data for 
  filter(!(species == "Dusky Rockfish" & maturity == "immature")) %>% 
  filter(!(species == "Shortbelly Rockfish" & maturity == "immature")) %>% 
  filter(!(species == "Shortraker Rockfish" & maturity == "immature")) %>% 
  filter(!(species == "Starry Flounder" & maturity == "immature")) %>% 
  filter(!(species == "Pygmy Rockfish" & maturity == "immature"))


all_indices$se_test <- all_indices$se/all_indices$est
all_indices$upr[all_indices$max_gradient > 0.01 ] <- NA

all_indices$upr[is.na(all_indices$se) ] <- NA
all_indices$upr[all_indices$se > 1.25 ] <- NA
# all_indices$upr[all_indices$se_test > 10 ] <- NA

all_indices$reliable_est <- all_indices$est
all_indices$reliable_est[is.na(all_indices$se)] <- NA
all_indices$reliable_est[all_indices$max_gradient > 0.01 ] <- NA

all_indices$reliable_est[all_indices$se > 1.25 ] <- NA
# all_indices$reliable_est[all_indices$se_test > 	10 ] <- NA

# filter years after 2004 because incomplete spatial sampling prior causes errors to blow up
indices1 <- all_indices %>% filter(year > 2004) %>%
  # and calculate index to base ratio on (max, mean, or median)
  group_by(species, maturity) %>% mutate( max_est = max(est), min_est = min(est), mean_est = mean(est)) %>% ungroup()

# add back in est for years prior because potentially still interesting despite uncertainty
est_only <- all_indices %>% select(year, species, maturity, est) %>% filter(year < 2005) 

#est_only$est[ (est_only$species == "Sand Sole") & (est_only$maturity == "immature") ] <- NA
est_only$est[ (est_only$species == "Sand Sole") ] <- NA

indices <- full_join(indices1, est_only , by=c("species", "year", "maturity", "est")) 

# remove upr that aren't plotted anyway due to no adjacent values
indices$upr[ indices$species == "Shortbelly Rockfish" ] <- NA
indices$upr[ indices$species == "Dusky Rockfish" ] <- NA
indices$upr[ indices$species == "Brown Cat Shark" ] <- NA


indices <- indices %>% group_by(species, maturity) %>% mutate (max_upr = max(upr, na.rm = T), max_est2 = max(est, na.rm = T))

indices$max_upr[ indices$max_upr == "-Inf" ] <- NA
indices$max_est2[ indices$max_est2 == "-Inf" ] <- NA

indices <- indices %>% mutate(axis_max = pmax(max_upr, max_est2)) %>% ungroup() #, na.rm = T
# View(filter(indices, species =="Shortbelly Rockfish"))


# calculate ratio of immature est to mature est for plotting on same figure
ind <- indices %>% filter(maturity != "immature")
ind_imm <- indices %>% filter(maturity == "immature") 

### ind_imm$upr[ ind_imm$species == "Harlequin Rockfish" ] <- NA

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

all_indices2 <- left_join(all_indices1, stats) %>%
  mutate(species=forcats::fct_reorder(species, -total_kg_2019)) #, na.rm = TRUE

all_indices2$ratio[ all_indices2$ratio == Inf ] <- 1
all_indices2$ratio[ is.na(all_indices2$ratio) ] <- 1
all_indices2$ratio <- signif((all_indices2$ratio), digits = 1)


# function to plot with facet wrap
plot_index_facets <- function(data, 
  scale = unit_conversion, 
  ad_col = "deepskyblue4",
  imm_col = "orangered"
  ) {
  
  # make dataframe of ratios by species for plotting labels on facets
  ratio_lab <- data %>% select(species, ratio, axis_max.x, axis_max.y, total_kg_2019) %>% unique()
  ratio_lab$ratio <- signif((ratio_lab$ratio), digits = 1)
  ratio_lab <- mutate(ratio_lab, max.y = axis_max.y*ratio, maxy = pmax(axis_max.x, max.y))
  
  ratio_lab$ratio[ ratio_lab$ratio == Inf ] <- NA
  # ratio_lab <- na.omit(ratio_lab)
  
  ggplot(data, aes(year, est.x*scale)) + 
    geom_line(col = ad_col, linetype = 2) +
    geom_line(aes(year, reliable_est.x*scale), col = ad_col) +
    geom_line(aes(year, est.y*scale*ratio), col = imm_col, linetype = 2) +
    geom_line(aes(year, reliable_est.y*scale*ratio), col = imm_col) +
    geom_ribbon(aes(ymin = lwr.x*scale, ymax = upr.x*scale), fill = ad_col, alpha = 0.4) +
    # facet_grid(species~covs, scales="free_y") +
    facet_wrap(~species, scales="free_y") +
    geom_ribbon(aes(ymin = lwr.y*scale*ratio, ymax = upr.y*scale*ratio), 
      fill = imm_col, alpha = 0.4) + 
    xlab('Year') + 
    ylab('Mature biomass estimate (metric tonnes)') + 
    geom_text(data = ratio_lab, aes(x = 2012,  y = maxy*scale, 
      label = paste(#"catch weight (kg) =", total_kg_2019, 
        "\nimmature biomass = 1/", ratio))) +
    scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(limits = c(2004, 2020)) +
    gfplot::theme_pbs(base_size = 16)
}

# Remove 3 species with smallest total catch in 2019 (<1.5 kg total)
# data <- all_indices2 %>% 
#   #filter(species != "Dusky") %>% # messy 
#   filter(species != "Cabezon") %>% 
#   filter(species != "Shiner Perch") %>% 
#   filter(species != "C-O Sole")

# data <- filter(all_indices2, group == "ROCKFISH") #%>% filter(total_kg_2019)
# data <- filter(all_indices2, group != "ROCKFISH") %>% filter(group != "FLATFISH")
# data <- filter(all_indices2, group == "FLATFISH")
# plot_index_facets(data)


# function to plot with facet wrap
make_plot_list <- function(data, 
  scale = unit_conversion, 
  ad_col = "deepskyblue4",
  imm_col = "orangered"
) {
  
  spp <- data$species[1]
  total_kg <- round(data$total_kg_2019[1])
  imm_ratio <- data$ratio[1]
  
  grob <- grid::grobTree(grid::textGrob(paste0(spp, " ( ", total_kg, " kg )"), 
    x=0.05,  y=0.85, hjust=0, gp=grid::gpar(col="black", fontsize=8)))  #, fontface="italic"

  ggplot(data, aes(year, est.x*scale)) + 
    geom_line(col = ad_col, linetype = 2) +
    geom_line(aes(year, reliable_est.x*scale), col = ad_col) +
    geom_line(aes(year, est.y*scale*ratio), col = imm_col, linetype = 2) +
    geom_line(aes(year, reliable_est.y*scale*ratio), col = imm_col) +
    geom_ribbon(aes(ymin = lwr.x*scale, ymax = upr.x*scale), 
      fill = ad_col, alpha = 0.4) +
    geom_ribbon(aes(ymin = lwr.y*scale*ratio, ymax = upr.y*scale*ratio), 
      fill = imm_col, alpha = 0.4) + 
    # if adding the secondary axis must revert the above transformation
    # note this won't work in a loop; must use apply function
    scale_y_continuous(sec.axis = ~ (./imm_ratio), 
      name = "", expand = expand_scale(mult = c(0.05, .2))
      ) + 
    expand_limits(y = 0) +
    ylab("") + 
    annotation_custom(grob) +
    gfplot::theme_pbs(base_size = 8) + 
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
      plot.margin = unit(c(0,0,0,0), "cm")) #, panel.border=element_blank()
  }
  
plot_indices <- function(data, 
  scale = unit_conversion, 
  ad_col = "deepskyblue4",
  imm_col = "orangered"
) {
  data <- droplevels(data)
  grouped_data <- split(data, data$species)
  
  plots <- lapply(grouped_data, make_plot_list, scale = scale, ad_col =ad_col, imm_col =imm_col)
  
  gbm::grid.arrange(
    gridExtra::arrangeGrob(
      ggpubr::ggarrange(plotlist = plots, ncol = 1, align = "v"),
        bottom=grid::textGrob("Year", gp=gpar(fontsize=10)), 
        left=grid::textGrob("Mature biomass (metric tonnes)", gp=gpar(fontsize=10), rot=90),
        right=grid::textGrob("Immature biomass (metric tonnes)", gp=gpar(fontsize=10), rot=-90))
  )
}



all_indices3 <- all_indices2[order(-all_indices2$total_kg_2019),]
kg_totals <- sort(unique(all_indices3$total_kg_2019), decreasing = T)
#(unique(all_indices3$species))

kg_totals[1:10]
alldata1 <- all_indices3 %>% filter(total_kg_2019 > 6500) 

kg_totals[11:20]
alldata2 <- all_indices3 %>% filter(total_kg_2019 <= 6500) %>% 
  filter(total_kg_2019 > 2400)

kg_totals[21:30]
alldata3 <- all_indices3 %>% filter(total_kg_2019 <= 2400) %>% 
  filter(total_kg_2019 > 1000) 

kg_totals[31:40]
alldata4 <- all_indices3 %>% filter(total_kg_2019 <= 1000) %>% 
  filter(total_kg_2019 > 107) 

kg_totals[41:50]
alldata5 <- all_indices3 %>% filter(total_kg_2019 <= 107) %>% 
  filter(total_kg_2019 > 50) 

kg_totals[51:60]
alldata6 <- all_indices3 %>% filter(total_kg_2019 <= 50) %>% 
  filter(total_kg_2019 > 5) 

kg_totals[61:68]
alldata7 <- all_indices3 %>% filter(total_kg_2019 <= 5) 

# # save as pdfs 4x10 inch portraits
# p <- plot_indices(alldata1)
# p <- plot_indices(alldata2)
# p <- plot_indices(alldata3)
# p <- plot_indices(alldata4)
# p <- plot_indices(alldata5)
# p <- plot_indices(alldata6)
# p <- plot_indices(alldata7)
# p
# ggsave("figs/panel7.pdf", plot = p, device = "pdf", width = 4, height = 10, units = c("in"),
#   dpi = 300)

# ### Shallow species
# all_indices4 <- all_indices2[order(all_indices2$depth),]
# # kg_totals <- sort(unique(all_indices3$total_kg_2019), decreasing = T)
# 
# shallow <- all_indices4 %>% filter(depth <= 100) %>% filter(group != "FLATFISH") %>% filter(group != "SKATE") %>% filter(total_kg_2019 >0.5) 
# 
# p <- plot_indices(shallow)
# ggsave("figs/shallow.pdf", plot = p, device = "pdf", width = 4, height = 10, units = c("in"),
#   dpi = 300)


# ### top 20 species
# alldata20 <- all_indices3 %>% 
#   filter(total_kg_2019 > 2300)
# # plot_indices(alldata20)
# 
# ### top 40 species
# kg_totals[1:40]
# alldata40 <- all_indices3 %>% 
#   filter(total_kg_2019 > 107)
# 
# alldata40 <- droplevels(alldata40)
# top_40 <- unique(alldata40$species)
# top_40 <- as.vector(top_40)
# vocc_spp <- as.vector(list_species) # list of species used in vocc analysis
# both <- top_40 %in% vocc_spp #which in both
# cbind(top_40, both) # compare lists