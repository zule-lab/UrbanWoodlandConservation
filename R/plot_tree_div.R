plot_tree_div <- function(trees_clean){
  
  
  # Plot diversity  ---------------------------------------------------------
  
  # plots are 100% sampled and are the same size, so we will not calculate diversity indices with iNEXT
  
  plots <- trees_clean %>% 
    unite(Scientific.Name, c("Genus", "Species"), sep = " ") %>% 
    select(c(PlotID, Scientific.Name)) %>% 
    group_by(PlotID, Scientific.Name) %>% 
    summarize(n = n()) %>% 
    pivot_wider(id_cols = "PlotID", names_from = 'Scientific.Name', values_from = "n", values_fill = 0,
                values_fn = first) %>% 
    mutate_if(is.numeric, ~1 * (. != 0)) %>% 
    select(-(" ")) %>% 
    column_to_rownames("PlotID")
  
  sr <- rowSums(plots)
  shan <- diversity(plots, index = "shannon")
  
  plot_div <- plots %>% 
    rownames_to_column("PlotID") %>% 
    select(PlotID)
  
  plot_div <- cbind(plot_div, sr)
  plot_div <- cbind(plot_div, shan)
  rownames(plot_div) <- NULL
  
  return(plot_div)
  
}