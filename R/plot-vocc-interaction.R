#' Plot interaction for vocc regression models
#'
#' @param model Model from vocc_regression function. 
#'    Requires named data element and coefs table. 
#' @param species Species effect to be plotted. 
#' @param variables List which variables are interacting.
#' @param scaled Logical for whether variables are scaled.
#'
#' @export
plot_interaction <- function(model, species = NULL, 
  variables = c("temp_vel_squashed", "do_vel_squashed"), 
  choose_x = NULL,
  scaled = TRUE  
  ){

d <- model$data
coefs <- model$coefs

var_1 <- variables[1]
var_2 <- variables[2]

if(is.null(species)) {species <- unique(model$coefs$species)}
x_variable <- c(var_1, var_1, var_1, var_1, var_2, var_2, var_2, var_2)

effect1 <- paste0(var_1, " at min(",var_2, ")")
effect2 <- paste0(var_1, " at max(",var_2, ")")
effect3 <- paste0(var_2, " at min(",var_1, ")")
effect4 <- paste0(var_2, " at max(",var_1, ")")
marg_effect <- c(effect1, effect1, effect2, effect2, effect3, effect3, effect4, effect4)

# effect1 <- paste0("min")
# effect2 <- paste0("max")
# effect3 <- paste0("min")
# effect4 <- paste0("max")
# marg_effect <- c(effect1, effect1, effect2, effect2, effect3, effect3, effect4, effect4)

all_species <- purrr::map_df(species, function(spp) {

  sp_coef <- filter (coefs, species == !!spp)
  sp_coef
  
  spp_d <- filter(d, species == !!spp)
  
  if (scaled) {
    x1_range <- range(scale(spp_d[[var_1]]))
    x2_range <- range(scale(spp_d[[var_2]]))
    
    b2 <- filter (sp_coef, coefficient == !!paste0("scale(", var_1, ")"))
    b3 <- filter (sp_coef, coefficient == !!paste0("scale(", var_2, ")"))
    interaction_name <- paste0("scale(", var_1, "):scale(", var_2, ")")
    b4 <- filter (sp_coef, coefficient == !!interaction_name)
    if(nrow(b4)==0) {  
      interaction_name <- paste0("scale(", var_2, "):scale(", var_1, ")")
      b4 <- filter (sp_coef, coefficient == !!interaction_name) 
    }
  } else {
    x1_range <- range(spp_d[[var_1]])
    x2_range <- range(spp_d[[var_2]])
    
    b2 <- filter (sp_coef, coefficient == !!var_1)
    b3 <- filter (sp_coef, coefficient == !!var_1)
    interaction_name <- paste0(var_1, ":", var_2)
    b4 <- filter (sp_coef, coefficient == !!interaction_name)
    
  }
  
  x <- c(x1_range[1],x1_range[2], x1_range[1],x1_range[2],x2_range[1],x2_range[2], x2_range[1],x2_range[2])
  x1 <- c(x1_range[1],x1_range[2], x1_range[1],x1_range[2],x1_range[1],x1_range[1], x1_range[2],x1_range[2])
  x2 <- c(x2_range[1],x2_range[1], x2_range[2],x2_range[2],x2_range[1],x2_range[2], x2_range[1],x2_range[2])
  
  b1 <- sp_coef$Estimate[1]

  y_hat_df <- purrr::map_df(seq_len(length(x)), function(i) {
     y_hat <- b1 + b2$Estimate*x1[i] + b3$Estimate*x2[i] + b4$Estimate*x1[i]*x2[i]
     data_frame(
       species = spp,
       x_var = x_variable[i],
       effect = marg_effect[i],
       x = x[i],
       x1 = x1[i],
       x2 = x2[i],
       y_hat = y_hat
     )
  })
 y_hat_df
})

if (is.null(choose_x)) {
  p <- ggplot(all_species, aes(x, y_hat, colour = effect)) + geom_line() + 
    facet_wrap(~species) +
    xlab(paste0("Climate variable (scaled)")) +
    scale_colour_manual(values = c( "#D53E4F", "#3288BD", "#5E4FA2", "#FDAE61")) +
    gfplot::theme_pbs() 
} else {
  
if (choose_x == 1)  { all_species <- filter(all_species, x_var == !!variables[1]) %>% 
  mutate(effect = gsub(paste(var_1, "at"), "", effect))
}
if (choose_x == 2)  { all_species <- filter(all_species, x_var == !!variables[2]) %>% 
  mutate(effect = gsub(paste(var_2, "at"), "", effect)) 
}

label_x <- all_species$x_var[1]
p <- ggplot(all_species, aes(x, y_hat, colour = effect)) + geom_line() + 
  facet_wrap(~species) +
  xlab(paste0(label_x)) + 
  scale_colour_manual(values = c( "#D53E4F", "#3288BD")) +
  #facet_grid(x_effect~species) + 
  gfplot::theme_pbs() 
}
p +  theme(legend.position = "top",
  legend.title = element_blank(),
  legend.text = element_text(size=10),
  legend.direction = "vertical") +
  guides(colour = guide_legend(nrow= 2, ncol= 2)) 
p
}



# p <- plot_interaction (model= bio_temp, #species = "North Pacific Spiny Dogfish",
#   variables = c("temp_vel_squashed", "do_vel_squashed"),
#   scaled = TRUE)
# p + theme(legend.position = "top",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.direction = "vertical") +
#   guides(colour = guide_legend(nrow= 2, ncol= 2)) +
#   ggtitle("Interation plots for immature abundance")

  