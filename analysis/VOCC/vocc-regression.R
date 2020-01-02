library(TMB)
library(dplyr)
library(ggplot2)

setwd(here::here("analysis", "VOCC"))
compile("vocc_regression.cpp")
dyn.load(dynlib("vocc_regression"))

# model <- "multi-spp-biotic-vocc"
model <- "multi-spp-biotic-vocc-mature"
# model <- "multi-spp-biotic-vocc-immature"

d <- readRDS(paste0("data/", model, ".rds"))
d <- na.omit(d) %>% as_tibble()

stats <- readRDS(paste0("data/life-history-stats.rds"))
stats$rockfish <- if_else(stats$group == "ROCKFISH", "ROCKFISH", "OTHER")

d <- left_join(d, stats) %>% filter(species != "Longspine Thornyhead")

# unique(d$species)

vocc_regression <- function(y_i, X_ij, knots = 150) {

  # y_i <- d$biotic_vel
  # X_ij <- model.matrix(~scale(temp_vel), data = d)
  #
  # hist(X_ij[,2], breaks = 100)
  # range(X_ij[,2])
  #
  # hist(y_i, breaks = 100)
  # range(y_i)
  #
  # ggplot(d, aes(x, y, colour = temp_vel)) + geom_point() +
  #   facet_wrap(~species) +
  #   scale_color_viridis_c()
  #
  # ggplot(d, aes(x, y, colour = biotic_vel)) + geom_point() +
  #   facet_wrap(~species) #+ scale_color_viridis_c()
  #
  # -----------------------------

  d$species_id <- as.integer(as.factor(d$species))

  spde <- sdmTMB::make_spde(d$x, d$y, n_knots = knots)
  # spde <- sdmTMB::make_spde(d$x, d$y, n_knots = 100)
  # map <- sdmTMB::plot_spde(spde)

  n_s <- nrow(spde$mesh$loc)
  n_k <- length(unique(d$species))

  data <- d
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
    k_i = d$species_id - 1L,
    n_k = n_k,
    nu = 7 # Student-t DF
  )

  tmb_param <- list(
    b_j = rep(0, ncol(X_ij)),
    log_gamma = rep(-1, ncol(X_ij)),
    ln_tau_O = 1,
    ln_kappa = -2,
    ln_phi = -1,
    omega_sk = matrix(0, nrow = n_s, ncol = n_k),
    b_re = matrix(0, nrow = n_k, ncol = ncol(X_ij))
  )

  obj <- MakeADFun(tmb_data, tmb_param, DLL = "vocc_regression", random = c("b_j", "omega_sk", "b_re"))
  opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(eval.max = 1e4, iter.max = 1e4))
  sdr <- sdreport(obj)
  sdr

  s <- summary(sdr)

  mutate(as.data.frame(s[row.names(s) == "b_j", ]), coefficient = colnames(X_ij)) %>%
    select(coefficient, Estimate, `Std. Error`)

  s[grep("ln|log", row.names(s)), ]
  s[grep("sigma", row.names(s)), , drop = FALSE]

  ids <- distinct(select(d, species, species_id)) %>% arrange(species_id)
  n_spp <- nrow(ids)
  n_coefs <- ncol(X_ij)
  ids <- do.call("rbind", replicate(n_coefs, ids, simplify = FALSE))
  ids[["coefficient"]] <- rep(colnames(X_ij), each = n_spp)

  b_re <- as.data.frame(s[grep("b_re", row.names(s)), , drop = FALSE])
  b_re <- bind_cols(ids, b_re)

  model <- list(obj = obj, sdr = sdr, coefs = b_re)
}

add_colours <- function(coefs, species_data = stats, add_spp_data = TRUE, manual_colours = TRUE) {
  if (add_spp_data) {
    coefs <- left_join(coefs, species_data)
  }

  if (manual_colours) {

    # sort(unique(d$species))
    # sort(unique(coefs$species))

    species <- c(
      "Arrowtooth Flounder",
      "Canary Rockfish",
      "Curlfin Sole",
      "Darkblotched Rockfish",
      "Dover Sole",
      "English Sole",
      "Flathead Sole",
      "Greenstriped Rockfish",
      "Lingcod",
      "Longspine Thornyhead",
      "North Pacific Spiny Dogfish",
      "Pacific Cod",
      "Pacific Halibut",
      "Pacific Ocean Perch",
      "Petrale Sole",
      "Quillback Rockfish",
      "Redbanded Rockfish",
      "Rex Sole",
      "Sablefish",
      "Sand Sole",
      "Sharpchin Rockfish",
      "Shortspine Thornyhead",
      "Silvergray Rockfish",
      "Southern Rock Sole",
      "Splitnose Rockfish",
      "Walleye Pollock",
      "Widow Rockfish",
      "Yelloweye Rockfish",
      "Yellowmouth Rockfish",
      "Yellowtail Rockfish"
    )

    #   ## To choose specific colours for specific species
    #   # RColorBrewer::brewer.pal(n = 10, name = 'Spectral')
    #   # RColorBrewer::display.brewer.pal(n = 10, name = 'Spectral')
    #   # gfutilities::rich.colors(n = 20, alpha = 1)

    colours <- c(
      "#3288BD", # "Arrowtooth Flounder",
      "#9E0142", # "Canary Rockfish",
      "#3288BD", # "Curlfin Sole",
      "#9E0142", # "Darkblotched Rockfish",
      "#3288BD", # "Dover Sole",
      "#3288BD", # "English Sole",
      "#3288BD", # "Flathead Sole",
      "#9E0142", # "Greenstriped Rockfish",
      "#66C2A5", # ""Lingcod",
      "#D53E4F", # "Longspine Thornyhead",
      "#FDAE61", # "North Pacific Spiny Dogfish",
      "#ABDDA4", # ""Pacific Cod",
      "#5E4FA2", # "Pacific Halibut",
      "#9E0142", # "Pacific Ocean Perch",
      "#3288BD", # "Petrale Sole",
      "#9E0142", # "Quillback Rockfish",
      "#9E0142", # "Redbanded Rockfish",
      "#3288BD", # "Rex Sole",
      "#ABDDA4", # ""Sablefish",
      "#3288BD", # "Sand Sole",
      "#9E0142", # "Sharpchin Rockfish",
      "#D53E4F", # "Shortspine Thornyhead",
      "#9E0142", # "Silvergray Rockfish",
      "#3288BD", # "Southern Rock Sole",
      "#9E0142", # "Splitnose Rockfish",
      "#ABDDA4", # "Walleye Pollock",
      "#9E0142", # "Widow Rockfish",
      "#9E0142", # "Yelloweye Rockfish",
      "#9E0142", # "Yellowmouth Rockfish",
      "#9E0142" # "Yellowtail Rockfish"
    )

    colour_key <- as_tibble(cbind(species, colours))
    colour_key$species <- as.factor(colour_key$species)

    out <- left_join(coefs, colour_key, by = "species")
    missing_colours <- out$species[is.na(out$colours)]

    if (length(missing_colours) > 0) {
      stop(paste(missing_colours, "need a colour assigned."))
    }
  } else {
    N <- length(unique(coefs$species))
    # species <- unique(coefs$species)
    species <- levels(coefs$species)
    colours <- gfutilities::rich.colors(n = N, alpha = 1)
    colour_key <- as_tibble(cbind(species, colours))
    colour_key$species <- as.factor(colour_key$species)
    out <- left_join(coefs, colour_key, by = "species")
  }
  out <- arrange(out, species)
  out
}

plot_coefs <- function(coloured_coefs, order_by = "Estimate") {

  # coloured_coefs <- out
  coloured_coefs <- filter(coloured_coefs, coefficient != "(Intercept)")
  coloured_coefs$order_by <- coloured_coefs[[order_by]]
  colour_list <- coloured_coefs$colours # c(unique(b_re$colours))


  p <- ggplot(coloured_coefs, aes(
    forcats::fct_reorder(species, -order_by), #-Estimate), 
    Estimate,
    colour = species,
    ymin = Estimate - 2 * `Std. Error`,
    ymax = Estimate + 2 * `Std. Error`
  )) +
    geom_hline(yintercept = 0, colour = "darkgray") +
    scale_colour_manual(values = colour_list) +
    geom_pointrange() + coord_flip() + xlab("") +
    # facet_wrap(~group) +
    theme(legend.position = "none")
  p
}


### Local biomass velocity from 2007-2018

y <- d$biotic_vel
x <- model.matrix(~ scale(temp_vel), data = d)

# hist(x[,2], breaks = 100)
# range(x[,2])
# hist(y, breaks = 100)
# range(y)

bio_temp <- vocc_regression(y, x, knots = 100)

bio_temp2 <- add_colours(bio_temp$coefs)
bio_temp3 <- plot_coefs(bio_temp2)
bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC"))
bio_temp_plot

## Ordered by increasing max weight and split by rockfish or not
# bio_temp3 <- plot_coefs(bio_temp2, order_by = "max_weight")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC ordered by increasing max weight")) +
#   facet_wrap(~rockfish, scales="free_y")
# bio_temp_plot

## Ordered by increasing max age and split by rockfish or not
# bio_temp_age <- na.omit(bio_temp2)
# bio_temp3 <- plot_coefs(bio_temp_age, order_by = "max_age")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of max age")) + facet_wrap(~rockfish, scales = "free_y")
# bio_temp_plot


# ## Ordered by increasing depth and split by rockfish or not
# bio_temp3 <- plot_coefs(bio_temp2, order_by = "large_depth")
# bio_temp_plot <- bio_temp3 + ggtitle(paste("Biotic velocity by thermal VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# bio_temp_plot


y <- d$biotic_vel
x <- model.matrix(~ scale(DO_vel), data = d)

bio_do <- vocc_regression(y, x, knots = 100)
bio_do2 <- add_colours(bio_do$coefs)
bio_do3 <- plot_coefs(bio_do2)
bio_do_plot <- bio_do3 + ggtitle(paste("Biotic velocity by DO VOCC"))
bio_do_plot

# ## Ordered by increasing depth and split by rockfish or not
# bio_do3 <- plot_coefs(bio_do2, order_by = "large_depth")
# bio_do_plot <- bio_do3 + ggtitle(paste("Biotic velocity by DO VOCC in order of mean depth")) + facet_wrap(~rockfish, scales = "free_y")
# bio_do_plot



### Local biomass trend from 2007-2018

y <- d$biotic_trend
x <- model.matrix(~ scale(temp_vel), data = d)

trend_temp <- vocc_regression(y, x, knots = 100)
trend_temp2 <- add_colours(trend_temp$coefs)
trend_temp3 <- plot_coefs(trend_temp2)
trend_temp_plot <- trend_temp3 + ggtitle(paste("Biotic trend by thermal VOCC"))
trend_temp_plot


y <- d$biotic_trend
x <- model.matrix(~ scale(DO_vel), data = d)

trend_do <- vocc_regression(y, x, knots = 100)
trend_do2 <- add_colours(trend_do$coefs)
trend_do3 <- plot_coefs(trend_do2)
trend_do_plot <- trend_do3 + ggtitle(paste("Biotic trend by DO VOCC"))
trend_do_plot

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
