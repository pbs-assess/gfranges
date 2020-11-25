#' Plot coefficients from vocc_regression
#'
#' @param coloured_coefs Coefficient dataframe with colours column.
#' @param order_by Coefficient by which to order species in plot.
#' @param manipulate Logical to allow manipulation in R studio.
#'
#' @export
plot_coefs <- function(coloured_coefs,
  grouping_taxa = "species",
  order_by_trait = FALSE,
  order_by = "temp_vel",
  increasing = F,
  grid_facets = F,
  add_grey_bars = F,
  fixed_scales = TRUE
) {
  if(is.null(coloured_coefs$age)) {
    coloured_coefs$age <- "both"
    coloured_coefs <- coloured_coefs %>% 
    mutate(coefficient = shortener(coefficient)) %>%
    mutate(coefficient = factor(coefficient, levels = c("Intercept", "log_biomass",
        "temp", "temp_trend", "temp_vel",  "temp_trend:temp", "temp_vel:temp",
        "DO", "DO_trend", "DO_vel", "DO_trend:DO", "DO_vel:DO",
        "log_effort", "fishing_trend", "log_effort:fishing_trend", "fishing_vel", "log_effort:fishing_vel", 
        "log_catch", "catch_trend", "catch_vel", "log_catch:catch_vel",
        "age", "age:temp_trend", "age:temp", 
        "age:DO_trend", "age:DO", 
        "age:temp_trend:temp", "age:DO_trend:DO", 
        "age:temp_vel", "age:DO_vel", 
        "age:temp_vel:temp", "age:DO_vel:DO"
        )))
  } else {
  coloured_coefs <- coloured_coefs %>% #filter(coefficient != "(Intercept)") %>% 
    mutate(coefficient = shortener(coefficient)) %>%
    mutate(coefficient = factor(coefficient, levels = c("Intercept", "log_biomass", 
      "temp", "temp_trend", "temp_vel",  "temp_trend:temp", "temp_vel:temp", 
      "DO", "DO_trend", "DO_vel", "DO_trend:DO", "DO_vel:DO",
      "log_effort", "fishing_trend", "log_effort:fishing_trend", "fishing_vel", "log_effort:fishing_vel", 
      "log_catch", "catch_trend", "catch_vel", "log_catch:catch_vel",
      "age", "age:temp_trend", "age:temp", 
      "age:DO_trend", "age:DO", 
      "age:temp_trend:temp", "age:DO_trend:DO", 
      "age:temp_vel", "age:DO_vel", 
      "age:temp_vel:temp", "age:DO_vel:DO")), 
      age = factor(age, levels = c("mature", "immature"))) 
  }
  
  if (order_by_trait) {
    order_values <- coloured_coefs %>% rename(order = !!order_by) %>% select(!!grouping_taxa, order)
    coloured_coefs <- inner_join(coloured_coefs, order_values)
    coloured_coefs <- filter(coloured_coefs, order != "NA")
  } else {
    order_by <- shortener(order_by)
    order_values <- filter(coloured_coefs, coefficient == !!order_by) %>%
      select(!!grouping_taxa, Estimate) %>% rename(order = Estimate) 
    coloured_coefs <- left_join(coloured_coefs, order_values)
  }
  coloured_coefs <- coloured_coefs %>% arrange(col_var)
  colour_list <- unique(coloured_coefs$colours)
  coloured_coefs$group <- coloured_coefs[[grouping_taxa]]
  p <- ggplot(coloured_coefs, aes(
    forcats::fct_reorder(group, order, .desc = increasing), #-Estimate),
    #forcats::fct_reorder(species,, -coloured_coefs[coloured_coefs$coefficient == "do_vel", ]$Estimate),
    Estimate,
    colour = col_var,
    shape = age,
    ymin = Estimate + qnorm(0.025) * `Std. Error`,
    ymax = Estimate + qnorm(0.975) * `Std. Error`
  )) + scale_colour_manual(values = colour_list, name = "Group") +
    geom_hline(yintercept = 0, colour = "darkgray") + geom_pointrange() +
    coord_flip() + xlab("") + 
    gfplot:::theme_pbs()  
  
  if(add_grey_bars) {
    .n <- length(unique(coloured_coefs[[grouping_taxa]]))
    .w <- 0.5
    p <- p + annotate(
      geom = "rect", xmin = seq(1, .n, by = 2) - .w, xmax = seq(1, .n, by = 2) + .w,
      ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.15
    )
  }
  
  
  if (grid_facets) {
    if(length(unique(coloured_coefs$age))>1){
    p <- p + facet_grid(age~coefficient, scales = "free") +
      scale_shape_manual(values=c(16, 16), guide = F)
    } else {
      p <- p + facet_grid(~coefficient, scales = "free") +
        scale_shape_manual(values=c(16, 16), guide = F)
    }
    
  } else {
    
    if(length(unique(coloured_coefs$age))>1) {  
      p <- p + #scale_linetype_manual(values=c("solid", "solid"), guide = F) +
        scale_shape_manual(values=c(21, 19), guide = F)
    } else {
      p <- p + #scale_linetype_manual(values=c("solid"), guide = F) +
        scale_shape_manual(values=c(16), guide = F)
    }  
    
  if (fixed_scales) {
    p <- p + facet_wrap(~coefficient, scales = "fixed")
  } else {
    p <- p + facet_wrap(~coefficient, scales = "free_x")
  }
  }  
  p 
}


#' Plot coefficients against species traits
#'
#' @param model_coefs Coefficient dataframe joined with species traits.
#' @param x Trait variable for x-axis.
#' @param coef Coefficient to plot.
#' @param group Colour by grouping.
#' @param point_size 
#' @param point_alpha 
#' @param point_shapes Vector of shapes. Defaults to open and closed circles.
#' @param regression Logical for adding regression line.
#'
#' @export
coef_scatterplot <- function(model_coefs, x, 
  coef = c("temp_trend_scaled", "DO_trend_scaled") , 
  group = "age",
  point_size = 0.75, 
  point_alpha = 0.65,
  point_shapes = c(21, 19),
  pointrange = T,
  regression = T
){
  p <- filter(model_coefs, coefficient %in% !!coef) %>% 
    ggplot(aes_string(x, "Estimate", shape = group, colour = group)) +
    geom_hline(yintercept = 0, colour = "black", alpha = 0.75, linetype = "dashed")
  if (regression) {
    p <- p + geom_smooth(method = "lm", colour = "darkgray", fill = "lightgray") 
  }
    p <- p + geom_point(size = point_size, alpha = point_alpha) 
    if (pointrange) {
      p <- p + geom_pointrange(aes(ymin = (Estimate - `Std. Error`* 1.96),
        ymax = (Estimate + `Std. Error` * 1.96)), alpha = point_alpha, fatten = 1)
    }
    p <- p + scale_color_viridis_d(direction = 1) +
      scale_shape_manual(values = point_shapes) +
      guides(fill = F) +
      ylab(coef) + gfplot:::theme_pbs() 
  p
}


#' Function for adding colours and species traits
#'
#' @param coefs Coefficient dataframe from TMB model output.
#' @param col_var Variable to colour by.
#' @param species_data Species trait dataframe.
#' @param add_spp_data Logical for if above is to be joined.
#' @param manual_colours Logical for if custom colours in this function.
#' @param last_used Apply colours from previous model. 
#'
#' @export
add_colours <- function(coefs, col_var = "group", 
  species_data = stats, add_spp_data = TRUE, 
  manual_colours = FALSE, 
  last_used = FALSE
  ) {

  if(!is.null(coefs$species)){
  coefs <- coefs %>% 
    mutate(age = if_else(gsub(" .*", "", species) == "immature", "immature", "mature"))
  coefs <- coefs %>% mutate(species = stringr::str_replace(species, ".*mature ", ""))
  }
  
  if (add_spp_data) {
    coefs <- left_join(coefs, species_data)
  }
  coefs$col_var <- coefs[[col_var]]
  
  if (manual_colours) {
    col_var <- c(
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
      "Yellowtail Rockfish",
      "Big Skate",
      "Bocaccio",
      "Redstripe Rockfish",
      "Longnose Skate",
      "Spotted Ratfish"
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
      "#9E0142", # "Yellowtail Rockfish"
      "#FDAE61", # "Big Skate",
      "#9E0142", # "Bocaccio",
      "#9E0142", # "Redstripe Rockfish",
      "#FDAE61", # "Longnose Skate",
      "#FDAE61" # "Spotted Ratfish"
    )
    
    colour_key <- as_tibble(cbind(col_var, colours))
    colour_key$col_var <- as.factor(colour_key$col_var)
    
    out <- left_join(coefs, colour_key, by = "col_var")
    missing_colours <- out$col_var[is.na(out$colours)]
    
    if (length(missing_colours) > 0) {
      stop(paste(missing_colours, "need a colour assigned."))
    }
    
  } else {
    if (last_used) {
      col_var <- unique(model2[[col_var]])
      colours <- unique(model2$colours)
    } else {
      
      if (add_spp_data) {
        coefs <- left_join(coefs, species_data)
      }

      coefs$col_var <- coefs[[col_var]]
      N <- length(unique(coefs$col_var))
      colourCount = N
      col_var <- sort(unique(coefs$col_var), decreasing = TRUE)
      # colours <- gfutilities::rich.colors(n = N, alpha = 1)
      # colours <- RColorBrewer::brewer.pal(n = N, name = 'Spectral')
      getPalette <- colorRampPalette(RColorBrewer::brewer.pal(n = N, name = 'Spectral'))
      colours <- getPalette(colourCount)
    }
    colour_key <- as_tibble(cbind(col_var, colours))
    colour_key$col_var <- as.factor(colour_key$col_var)
    out <- left_join(coefs, colour_key)
  }
  out <- arrange(out, col_var)
  out
}


#' Strip qualifiers from variable names
#' @export
shortener <- function(string) {
  out <- gsub("\\(", "", string) #coef_names[1:length()]
  out <- gsub("\\, center = F)", "", out)
  out <- gsub("_scaled", "", out)
  out <- gsub("scale", "", out)
  out <- gsub("\\)", "", out)
  out <- gsub("squashed_", "", out)
  out <- gsub("mean_", "", out)
  out <- gsub("immature", "", out)
  out <- gsub("mature ", "", out)
  #out <- gsub("_", "", out)
  out
}

#' Add capitals to start of strings
#' @export
firstup <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
