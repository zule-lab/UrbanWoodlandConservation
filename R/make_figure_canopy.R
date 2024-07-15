make_figure_canopy <- function(can_park, data_park){
  
  p <- plot_predictions(can_park, by = "Conservation.area") + 
    geom_point(aes(x = Conservation.area, y = percan), data = data_park, position = position_dodge2(width = 0.1),
               colour = "grey21", alpha = 0.5)+
    theme_classic() + 
    labs(x = "Conservation Area", y = "Canopy Cover (%)")
  
  
  ggsave('figures/canopy.png', p, width = 8, height = 6, units = "in")
  
  
}