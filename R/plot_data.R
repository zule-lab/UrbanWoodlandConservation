plot_data <- function(parks, diversity, canopy, invasive){
  
  parks_sel_plot <- parks_for %>%
    select(c(Park, PlotID, Park.size, Park.age, Established, Conservation.area, VegetationLevels)) %>%
    distinct() %>% 
    mutate(Complexity = stri_count_words(VegetationLevels))
  
  cdp <- full_join(canopy_plots, diversity_plots, by = join_by("Park" == "PlotID"))
  cdip <- full_join(cd, invasive_plots, by = join_by("Park" == "PlotID"))
  full_plots <- full_join(cdip, parks_sel_plot, by = join_by("Park" == "PlotID"))
  
  return(full_plots)
  
}