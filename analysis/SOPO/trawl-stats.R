getwd()
setwd(here::here("/analysis/SOPO"))
library("dplyr")
library("ggplot2")
library("grid")
library("gfranges")
options(scipen = 999)


complex <- readRDS("~/github/dfo/gfranges/analysis/VOCC/raw/event-data-rougheye-blackspotted-rockfish-complex")

data <- readRDS("data/all_trawl_catch_2019.rds")
data <- rbind(data, complex) %>% filter(year==2019)


all_catch <- readRDS("data/all_trawl_catch.rds")
all_catch <- rbind(all_catch, complex)

shortb <- data %>% filter(species_common_name == "shortbelly rockfish")
sum(shortb$catch_weight)
all_shortb <- all_catch %>% filter(species_common_name == "shortbelly rockfish") %>% View()
sum(shortb$catch_weight)/sum(all_shortb$catch_weight)

chili <- data %>% filter(species_common_name == "chilipepper")
sum(chili$catch_weight)
all_chili <- all_catch %>% filter(species_common_name == "chilipepper") %>% View()
sum(chili$catch_weight)/sum(all_chili$catch_weight)


inverts <- readRDS("data/all_trawl_invert.rds")

# number of tows
length(unique(data$fishing_event_id))

data2 <- data %>% select(fishing_event_id, species_common_name, catch_weight) %>% unique()

sum(data2$catch_weight)

# split by survey area
HS <- data %>% select(survey_series_id, fishing_event_id, species_common_name, catch_weight) %>% unique() %>% filter(survey_series_id == 3)

sum(HS$catch_weight)
sum(HS$catch_weight)/length(unique(HS$fishing_event_id))

HS <- HS %>% group_by(species_common_name) %>% mutate(total_catch = sum(catch_weight)) 
HS %>% select(species_common_name, total_catch) %>% unique() %>% View



# split by survey area
QCS <- data %>% select(survey_series_id, fishing_event_id, species_common_name, catch_weight) %>% unique() %>% filter(survey_series_id == 1)

sum(QCS$catch_weight)
sum(QCS$catch_weight)/length(unique(QCS$fishing_event_id))

QCS <- QCS %>% group_by(species_common_name) %>% mutate(total_catch = sum(catch_weight)) 
QCS %>% select(species_common_name, total_catch) %>% unique() %>% View




unique(sort(data$species_common_name))
View(data)

pred_dat <- readRDS("data/arrowtooth-flounder/sopo-predictions-arrowtooth-flounder-no-covs-300-mat-biomass-ar1-TRUE-reml.rds")

# mod <- readRDS("data/arrowtooth-flounder/mod-mat-biomass-arrowtooth-flounder-no-covs-300-1n3-ar1-TRUE-reml.rds")

raw_data <- readRDS("data/arrowtooth-flounder/check-mod-predictions-arrowtooth-flounder-no-covs-300-1n3-ar1-TRUE-reml.rds")

pred_dat <- filter(pred_dat, year == 2019) %>% mutate (combined = depth, # est_exp, # for pred densities
  bin = NA, pos = NA, akima_depth = depth)
# pred_dat <- tibble::rowid_to_column(pred_dat, "id")
raw_dat <- filter(raw_data, year == 2019)
nrow(raw_dat)
View(raw_dat)
length(unique(raw_dat$fishing_event_id))

hbll <- readRDS("data/all_hbll_in.rds") 
n_grid <- load("~/github/dfo/gfplot/data/hbll_n_grid.rda")

hbll1 <- filter(hbll, species_code == 394 & survey_abbrev == "HBLL INS N")  
View(hbll1)
all_years <- unique(hbll1$year)
# hbll1 <- filter(hbll, species_common_name == "yelloweye rockfish" & year == 2019)  

hbll_tidy <- #gfplot::
  tidy_survey_sets(hbll1, survey = "HBLL INS N",
  years = all_years, 
    density_column = "density_ppkm2") %>% mutate(akima_depth = depth)
# View(hbll_tidy)

hbll_tidy <- gfplot:::scale_survey_predictors(hbll_tidy)
tidy_grid <- hbll_tidy
tidy_grid$year <- 2019

pg <- make_prediction_grid(tidy_grid, survey = NULL, draw_boundary = FALSE
)$grid 
pg <- as.data.frame(pg)
pg <- pg %>% mutate(depth = akima_depth, combined = depth)

hbll_2019 <- hbll_tidy %>% filter(year == 2019)

##### HBLL MAP
plot_survey_sets(pg, hbll_2019, fill_column = "combined",
  fill_scale =
    ggplot2::scale_fill_viridis_c( option = "D", direction = -1), #trans= "sqrt",
  colour_scale =
    ggplot2::scale_colour_viridis_c( option = "D", direction = -1),
  pos_pt_col = "black", #"#FFFFFF60",
  bin_pt_col = "black",#"#FFFFFF40",
  pos_pt_fill = "black",#"#FFFFFF05",
  pt_size_range = c(1, 1),
  show_legend = T,
  extrapolate_depth = T,
  extrapolation_buffer = 0,
  show_model_predictions = F,
  show_raw_data = TRUE,
  utm_zone = 9,
  fill_label = "Depth (m)",
  pt_label = "Tow density (kg/km^2)",
  rotation_angle = 0,
  rotation_center = c(500, 5700),
  show_axes = TRUE,
  xlim = NULL,
  ylim = NULL,
  x_buffer = c(-5, 5),
  y_buffer = c(-5, 5),
  north_symbol = F,
  north_symbol_coord = c(810, 5630),
  north_symbol_length = 10,
  cell_size = 2, circles = T)




View(s_grid["grid"])
plot_survey_sets(pred_dat, raw_dat, fill_column = "combined",
  fill_scale =
    ggplot2::scale_fill_viridis_c( option = "D", direction = -1), #trans= "sqrt",
  colour_scale =
    ggplot2::scale_colour_viridis_c( option = "D", direction = -1),
  pos_pt_col = "black", #"#FFFFFF60",
  bin_pt_col = "black",#"#FFFFFF40",
  pos_pt_fill = "black",#"#FFFFFF05",
  pt_size_range = c(0.8, 0.8),
  show_legend = T,
  extrapolate_depth = T,
  extrapolation_buffer = 0,
  show_model_predictions = T,
  show_raw_data = TRUE,
  utm_zone = 9,
  fill_label = "Depth (m)",
  pt_label = "Tow density (kg/km^2)",
  rotation_angle = 0,
  rotation_center = c(500, 5700),
  show_axes = TRUE,
  xlim = NULL,
  ylim = NULL,
  x_buffer = c(-5, 5),
  y_buffer = c(-5, 5),
  north_symbol = T,
  north_symbol_coord = c(550, 6000),
  north_symbol_length = 30,
  cell_size = 0.6, circles = T)
