make_figure_complexity <- function(complexity_park, data_park){
  
  p <- plot_predictions(complexity_park, by = "Conservation.area") + 
    geom_point(aes(x = Conservation.area, y = MeanComplexity), data = data_park, position = position_dodge2(width = 0.1),
               colour = "grey21", alpha = 0.5)+
    theme_classic() + 
    labs(x = "Conservation Area")
  
  
  ggsave('figures/complexity.png', p, width = 8, height = 6, units = "in")
  
  
}