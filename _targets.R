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
    data_plot,
    plot_data(trees_raw, trees_ranges, parks, "input/660_IndiceCanopee_2021.tif", "input/gps_sppts.gpkg")
  ),
  
  tar_target(
    data_park,
    park_data(trees_raw, trees_ranges, parks, "input/660_IndiceCanopee_2021.tif", "input/study_parks_spatial.gpkg")
  ),
  
  tar_target(
    sr_park,
    glm(SR ~ 1 + Conservation.area + Park.size_s + PropInv_s, 
        family = gaussian(), 
        data = data_park)
  ),
  
  tar_target(
    shan_park,
    glm(formula = Shannon ~ 1 + Conservation.area + Park.size_s + PropInv_s,
        family = gaussian(),
        data = data_park)
  ),
  
  tar_target(
    complexity_park,
    glm(formula = MeanComplexity ~ 1 + Conservation.area + Park.size_s + PropInv_s,
        family = gaussian(), 
        data = data_park)
  ),
  
  tar_target(
    inv_sp_park,
    glm(formula = PropInvSp ~ 1 + Conservation.area + Park.size,
        family = gaussian(), 
        data = data_park)
  ),
  
  tar_target(
    inv_stems_park,
    glm(formula = PropInv ~ 1 + Conservation.area + Park.size,
        family = gaussian(), 
        data = data_park)
  ),
  
  tar_target(
    model_diagnostics,
    diagnostics(sr_park, shan_park, complexity_park, 
                inv_sp_park, inv_stems_park)
  ),
  
  tar_target(
    figure_1,
    make_figure_1("input/study_parks_spatial.gpkg")
  )
  
  
)


## TODO 
# plot model outcomes 
# add code for figure 1 - map 

# switch to beta regression?
# model plot data?
# NOTE: if using plot data - missing some canopy and have 96 plots instead of 92