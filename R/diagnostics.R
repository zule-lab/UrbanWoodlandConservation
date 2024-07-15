diagnostics <- function(sr_park, shan_park, complexity_park, inv_sp_park, inv_stems_park){

  
  pdf('figures/diagnostics/model_diagnostics.pdf')
  
  print(resid_plots(sr_park, "Species Richness"))
  print(resid_plots(shan_park, "Shannon Diversity"))
  print(resid_plots(complexity_park, "Vegetative Complexity"))
  print(resid_plots(inv_sp_park, "Invasive Species"))
  print(resid_plots(inv_stems_park, "Invasive Stems"))
  
  dev.off()
  
  
  
}