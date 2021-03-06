library(dplyr)
library(ggplot2)
library(TMB)
library(gfranges)
ggplot2::theme_set(gfplot::theme_pbs())

manual_colours <- FALSE

getwd()
# setwd(here::here("/analysis/VOCC"))

# files <- list.files("../rockfish-vocc-temp/perc_50/0.75/", full.names = TRUE)

climate <- "temperature"
# files <- list.files("data/_all/temperature/perc_50/0.25/mature/", full.names = TRUE)
files <- list.files("data/_all/temperature/perc_50/0.5/mature/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_50/0.75/mature/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_50/1/mature/", full.names = TRUE)

# files <- list.files("data/_all/temperature/perc_50/0.25/imm/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_50/0.5/imm/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_50/0.75/imm/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_50/1/imm/", full.names = TRUE)

# files <- list.files("data/_all/temperature/perc_25/0.25/mature/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_25/0.5/mature/", full.names = TRUE)

# files <- list.files("data/_all/temperature/perc_25/0.25/imm/", full.names = TRUE)
# files <- list.files("data/_all/temperature/perc_25/0.5/imm/", full.names = TRUE)


climate <- "do"
files <- list.files("data/_all/do/perc_50/0.25/mature/", full.names = TRUE) # no sig, but mostly negative
# files <- list.files("data/_all/do/perc_50/0.5/mature/", full.names = TRUE) # no sig
files <- list.files("data/_all/do/perc_25/0.25/mature/", full.names = TRUE) # no sig, but interesting
# files <- list.files("data/_all/do/perc_25/0.5/mature/", full.names = TRUE) #

# files <- list.files("data/_all/do/perc_50/0.25/imm/", full.names = TRUE) # not converging
# files <- list.files("data/_all/do/perc_50/0.5/imm/", full.names = TRUE) # no sig
# files <- list.files("data/_all/do/perc_25/0.25/imm/", full.names = TRUE) # doesn't converge
# files <- list.files("data/_all/do/perc_25/0.5/imm/", full.names = TRUE)

# files <- list.files("data/_all/do/perc_50/0.75/mature/", full.names = TRUE) # only 2015 changes this much
# files <- list.files("data/_all/do/perc_75/0.5/imm/", full.names = TRUE)

knots <- 150

model_type <- gsub("/", " ", gsub("//vocc..*", " ", gsub("data/_all/", " ", files[1])))

.d <- purrr::map_dfr(files, readRDS)

# if do remove severe outliers
.d <- filter(.d, !(start_time == "2014" & ssid == 4))
.d <- filter(.d, !(start_time == "2016" & ssid == 4))

.d <- .d %>%
  group_by(species, start_time) %>%
  mutate(count = n()) %>%
  filter(count > 30)
unique(.d$count)

if (climate == "do") {
  spp_values <- .d %>%
    group_by(species) %>%
    select(species, mature, var_1_min) %>%
    distinct() %>% arrange(-var_1_min)
}

if (climate == "temperature") {
  spp_values <- .d %>%
    group_by(species) %>%
    select(species, mature, var_1_max) %>%
    distinct() %>% arrange(var_1_max)
}

# spp_values

rm(d)
d <- select(.d, species, log_density, after, cell_type, log_depth, icell, start_time, ssid, X, Y, vect_dist, matchobs)
d <- mutate(d, source = ifelse(cell_type == "source", 1, 0))

unique(d$start_time)

if (manual_colours) {
  
species <- c(
  "Bocaccio", 
  "Pacific Ocean Perch", 
  "Redbanded Rockfish",
  "Quillback Rockfish",
  "Canary Rockfish",
  "Yelloweye Rockfish",
  "Greenstriped Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish",
  "Yellowtail Rockfish",
  "Shortspine Thornyhead"
)

## To choose specific colours for specific species 
# RColorBrewer::brewer.pal(n = 10, name = 'Spectral')
# RColorBrewer::display.brewer.pal(n = 10, name = 'Spectral')
# gfutilities::rich.colors(n = 20, alpha = 1)

colours <- c( 
  "#9E0142", #Bocaccio
  "#D53E4F", #Pacific Ocean Perch 
  "#FF3300", #Redbanded Rockfish
  "#F46D43", #Quillback Rockfish
  "#FF9500", #"#FDAE61", #Canary Rockfish
  "#FFF200", #Yelloweye Rockfish
  "#bcf60c", #"#E6F598", #Greenstriped Rockfish 
  "#ABDDA4", #"Rougheye/Blackspotted Rockfish Complex",
  "#66C2A5", #Sharpchin Rockfish
  "#3288BD", #Silvergray Rockfish
  "#0049FF", #Splitnose Rockfish
  "#0000A9", #"Widow Rockfish" 
  "#5E4FA2", #"Yellowtail Rockfish"
  "#000041" # "Shortspine Thornyhead"
)

colour_key <- as_tibble(cbind(species, colours))
d <- left_join(d, colour_key, by = "species") 
missing_colours <- d$species[is.na(d$colours)] 

if (length(missing_colours)>0) {
  stop(paste(missing_colours, "need a colour assigned."))
}

} else {
  
N <- length(unique(d$species))
species <- unique(d$species)
colours <- gfutilities::rich.colors(n = N, alpha = 1)
colour_key <- as_tibble(cbind(species, colours))
d <- left_join(d, colour_key, by = "species") 

}

d <- arrange(d, species)
colour_list <- c(unique(d$colours))

nrow(d)

p1 <- ggplot(d, aes(X, Y, colour = cell_type)) +
  geom_point(size = 0.3, alpha = 0.05) +
  scale_colour_manual(values = c("yellow", "slateblue4")) +
  facet_wrap(~species) + coord_fixed()
#+ ggtitle(paste(model_type))
p1

p2 <- ggplot(d, aes(X, Y, colour = log_density)) +
  geom_point(size = 0.4, alpha = 0.3) +
  facet_grid(species ~ start_time) + coord_fixed() + scale_color_viridis_c() +
  theme(strip.text.y = element_text(hjust = 0))
#+ ggtitle(paste(model_type))
p2


d$species_year <- paste(d$species, d$start_time)

data <- d
# formula <- log_density ~ after * source + scale(log_depth) # + as.factor(species)
# formula <- log_density ~ after * source + scale(log_depth) + scale(vect_dist)
formula <- log_density ~ after + source + scale(log_depth) * as.factor(species_year) +
  I((scale(log_depth))^2) * as.factor(species_year)

species_k <- as.integer(as.factor(d$species_year))
cell_m <- as.integer(as.factor(paste(d$matchobs, d$species_year, d$ssid)))

X_ij <- model.matrix(formula, data)
# colnames(X_ij)
mf <- model.frame(formula, data)
y_i <- model.response(mf, "numeric")

# Calculate the species indexes for the species year random effects:
x <- dplyr::distinct(select(d, species, species_year))
species_id_k <- as.integer(as.factor(x$species))

spde <- sdmTMB::make_spde(d$X, d$Y, n_knots = knots)
sdmTMB::plot_spde(spde)
# data$sdm_spatial_id <- 1:nrow(data)
n_s <- nrow(spde$mesh$loc)
n_k <- length(unique(species_k))
n_re <- 4

data$sdm_orig_id <- seq(1, nrow(data))
data$sdm_x <- spde$x
data$sdm_y <- spde$y
fake_data <- unique(data.frame(sdm_x = spde$x, sdm_y = spde$y))
fake_data[["sdm_spatial_id"]] <- seq(1, nrow(fake_data))
data <- base::merge(data, fake_data,
  by = c("sdm_x", "sdm_y"),
  all.x = TRUE, all.y = FALSE
)
data <- data[order(data$sdm_orig_id), , drop = FALSE]
A_sk <- INLA::inla.spde.make.A(spde$mesh,
  loc = as.matrix(fake_data[, c("sdm_x", "sdm_y"), drop = FALSE])
)

tmb_data <- list(
  y_i = y_i,
  X_ij = X_ij,
  A_sk = A_sk,
  A_spatial_index = data$sdm_spatial_id - 1L,
  spde = spde$spde$param.inla[c("M0", "M1", "M2")],
  k_i = species_k - 1L,
  intercept_i = rep(1, nrow(d)),
  after_i = d$after,
  source_i = d$source,
  n_k = n_k,
  m_i = cell_m - 1L, # random effect of match ids
  species_id_k = species_id_k - 1L, # random effect of species
  n_just_species = max(species_id_k),
  n_years_per_species = as.numeric(table(species_id_k))
  # interaction_position = grep("after:source", colnames(X_ij)) - 1
)

tmb_params <- list(
  b_j = rep(0, ncol(tmb_data$X_ij)),
  ln_tau_E = rep(0, max(species_id_k)),
  ln_kappa = 0,
  ln_phi = 0,
  epsilon_sk = matrix(0, nrow = n_s, ncol = n_k),
  b_re = matrix(0, nrow = n_k, ncol = n_re),
  b_re_sp = rep(0, tmb_data$n_just_species),
  log_gamma = rep(0, n_re - 1 - 1),
  log_omega = 0,
  # log_omega = rep(0, max(species_id_k)),
  b_cell = rep(0, length(unique(tmb_data$m_i))),
  log_varphi = 0
)

suppressWarnings(file.remove(
  "basic_spatial_re.o",
  "basic_spatial_re.so"
)) # to avoid crashing R
TMB::compile("basic_spatial_re.cpp", )
dyn.load(dynlib("basic_spatial_re"))

# First just fit the fixed effects:
tmb_map <- list(
  ln_tau_E = as.factor(rep(NA, length(tmb_params$ln_tau_E))),
  ln_kappa = as.factor(NA),
  epsilon_sk = factor(rep(NA, length(tmb_params$epsilon_sk))),
  b_re = factor(matrix(NA, nrow = n_k, ncol = n_re)),
  # b_re_sp = factor(rep(NA, length(tmb_params$b_re_sp))),
  log_gamma = factor(rep(NA, length(tmb_params$log_gamma))),
  log_omega = factor(rep(NA, length(tmb_params$log_omega))),
  log_varphi = as.factor(NA),
  b_cell = factor(rep(NA, length(tmb_params$b_cell)))
)

tmb_obj <- TMB::MakeADFun(
  data = tmb_data, parameters = tmb_params, map = tmb_map,
  random = NULL, DLL = "basic_spatial_re"
)

tmb_opt <- stats::nlminb(
  start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,
  control = list(eval.max = 1e4, iter.max = 1e4)
)

# ML:
# tmb_random <- c("epsilon_sk", "b_re", "b_cell")
# REML:
tmb_random <- c("epsilon_sk", "b_re", "b_cell", "b_re_sp", "b_j")

# Initialize the fixed effects from the first stage:
set_par_value <- function(opt, par) {
  as.numeric(opt$par[par == names(opt$par)])
}
tmb_params$b_j <- set_par_value(tmb_opt, "b_j")
tmb_params$ln_phi <- set_par_value(tmb_opt, "ln_phi")
tmb_params$b_re_sp <- set_par_value(tmb_opt, "b_re_sp")

tmb_obj <- TMB::MakeADFun(
  data = tmb_data, parameters = tmb_params,
  random = tmb_random, DLL = "basic_spatial_re"
)
tictoc::tic()
tmb_opt <- stats::nlminb(
  start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,
  control = list(eval.max = 1e4, iter.max = 1e4)
)
sdr <- TMB::sdreport(tmb_obj)
tictoc::toc()
sdr

s <- summary(sdr)

mutate(as.data.frame(s[row.names(s) == "b_j", ]), coefficient = colnames(X_ij)) %>%
  select(coefficient, Estimate, `Std. Error`)

s[grep("ln|log", row.names(s)), ]
s[grep("sigma", row.names(s)), , drop = FALSE]

r <- tmb_obj$report()
r$range

co <- as.data.frame(s[row.names(s) == "b_baci_interaction", ])
co$species_year <- as.character(unique(as.factor(d$species_year)))
co <- co %>% 
  mutate(just_species = gsub(" [0-9]+$", "", species_year)) %>%
  group_by(just_species) %>% 
  mutate(mean_baci_int = mean(Estimate)) %>%
  ungroup() %>%
  arrange(mean_baci_int, species_year) %>%
  mutate(myorder = seq_len(n()))

p3 <- ggplot(co, aes(forcats::fct_reorder(species_year, -myorder), Estimate,
  colour = just_species,
  ymin = Estimate - 2 * `Std. Error`,
  ymax = Estimate + 2 * `Std. Error`
)) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = colour_list) + 
  geom_pointrange() + coord_flip() + xlab("") +
  theme(legend.position = "none")
#+ ggtitle(paste(model_type))
p3

meta <- s[grep("b_re_sp", row.names(s)), ]
meta <- as.data.frame(meta)
row.names(meta) <- NULL
meta$just_species <- unique(x$species)

p4 <- ggplot(meta, aes(forcats::fct_reorder(just_species, -Estimate), Estimate,
  colour = just_species,
  ymin = Estimate - 2 * `Std. Error`,
  ymax = Estimate + 2 * `Std. Error`
)) +
  geom_hline(yintercept = 0, colour = "darkgray") +
  scale_colour_manual(values = colour_list) + 
  geom_pointrange() + coord_flip() + xlab("") +
  ggtitle(paste(model_type)) +
  theme(legend.position = "none")
p4

png(
  file = paste0(
    "figs/", gsub("/", "-", gsub("//vocc..*", "", gsub("data/_all/", "", files[1]))),
    knots, "knot.png"
  ),
  res = 600,
  units = "in",
  width = 11,
  height = 8
)
gridExtra::grid.arrange(
  p4, p1, p3, p2,
  nrow = 2
)
dev.off()
p1
p2
p3
p4
spp_values