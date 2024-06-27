park_tree_div <- function(trees_clean){
  
  # Diversity Indices -------------------------------------------------------
  # Full Tree Dataset
  trees_list_inext <- trees_clean %>% 
    group_by(Park) %>% 
    group_map(~ group_by(.x, PlotID, SpCode) %>% 
                tally() %>% 
                pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
                mutate_if(is.numeric, ~1 * (. != 0)) %>%
                column_to_rownames("SpCode")) %>%
    setNames(unique(trees_for$Park))
  
  out <- iNEXT(trees_list_inext, datatype = "incidence_raw", q=0)
  
  ggiNEXT(out, type = 3)+ xlim(c(0,1)) + 
    theme_classic(base_size = 15) + 
    scale_fill_discrete() + 
    scale_color_discrete()
  
  div <- estimateD(trees_list_inext, datatype="incidence_raw",
                   base="coverage", level=0.7828, conf=0.95)
  
  return(div)
  
}