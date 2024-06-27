plot_data <- function(parks, diversity, canopy, invasive){
  
  parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
  
  parks_sel_plot <- parks_for %>%
    select(c(Park, PlotID, Park.size, Park.age, Established, Conservation.area, VegetationLevels)) %>%
    distinct() %>% 
    mutate(Complexity = stri_count_words(VegetationLevels))
  
  cdp <- full_join(canopy, diversity, by = join_by("Park" == "PlotID"))
  cdip <- full_join(cdp, invasive, by = join_by("Park" == "PlotID"))
  full_plots <- full_join(cdip, parks_sel_plot, by = join_by("Park" == "PlotID"))
  
  return(full_plots)
  
}