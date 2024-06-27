# Source ------------------------------------------------------------------
library(targets)
tar_source('R')

# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')

# Stan
options(mc.cores = 2,
        scipen = 999,
        digits = 2)

# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()


# Targets -----------------------------------------------------------------

c(
  
  tar_file_read(
    trees_raw,
    "input/trees_openrefine.csv",
    read.csv(!!.x)
  ),
  
  tar_file_read(
    trees_ranges, 
    "input/species_ranges.csv",
    read.csv(!!.x)
  ),
  
  tar_file_read(
    parks,
    "input/Park.densiometer.csv",
    read.csv(!!.x)
  ),
  
  tar_target(
    trees_clean,
    trees_cleaned(trees_raw)
  ),
  
  tar_target(
    tree_div_plot,
    plot_tree_div(trees_clean)
  ),
  
  tar_target(
    tree_div_park,
    park_tree_div(trees_clean)
  ),
  
  tar_target(
    canopy_plot,
    plot_canopy("input/660_IndiceCanopee_2021.tif", "input/gps_sppts.gpkg", "input/study_parks_spatial.gpkg")
  ),
  
  tar_target(
    canopy_park,
    park_canopy("input/660_IndiceCanopee_2021.tif", "input/gps_sppts.gpkg", "input/study_parks_spatial.gpkg")
  ),
  
  tar_target(
    invasive_plot,
    plot_invasive(trees_clean, trees_ranges)
  ),
  
  tar_target(
    invasive_park,
    park_invasive(trees_clean, trees_ranges)
  ),
  
  tar_target(
    data_plot,
    plot_data(parks, tree_div_plot, canopy_plot, invasive_plot)
  ),
  
  tar_target(
    data_park,
    park_data(parks, tree_div_park, canopy_park, invasive_park)
  )
  
  
  
  
)
