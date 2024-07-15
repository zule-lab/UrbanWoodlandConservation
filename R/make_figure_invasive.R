make_figure_invasive <- function(inv_stems_park, data_park){
  
  p <- plot_predictions(inv_stems_park, by = "Conservation.area") + 
    geom_point(aes(x = Conservation.area, y = PropInv), data = data_park, position = position_dodge2(width = 0.1),
               colour = "grey21", alpha = 0.5)+ 
    theme_classic() + 
    labs(x = "Conservation Area", y = "Proportion of Invasive Stems")
  
  
  ggsave('figures/invasive.png', p, width = 8, height = 6, units = "in")
  
  
}