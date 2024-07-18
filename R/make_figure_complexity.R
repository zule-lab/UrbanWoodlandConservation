make_figure_complexity <- function(complexity_park, data_park){
  
  
  p <- plot_predictions(complexity_park, by = "Conservation.area") + 
    geom_point(aes(x = Conservation.area, y = MeanComplexity), data = data_park, position = position_dodge2(width = 0.1),
               colour = "grey21", alpha = 0.5)+
    scale_x_discrete(labels = c("Non-Status Woodland", "Nature Park")) + 
    theme_classic() + 
    labs(x = "", y = "Mean Vegetative Complexity") +
    theme(axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(size = 12))
  
  
  ggsave('figures/complexity.png', p, width = 8, height = 6, units = "in")
  
  
}