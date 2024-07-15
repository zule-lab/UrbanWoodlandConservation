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
    glm(SR_s ~ 1 + Conservation.area + Park.size_s + PropInv_s, 
        family = gaussian(), 
        data = data_park)
    ),
  
  tar_target(
    shan_park,
    glm(formula = Shannon_s ~ 1 + Conservation.area + Park.size_s + PropInv_s,
        family = gaussian(),
        data = data_park)
    ),
  
  tar_target(
    complexity_park,
    glm(formula = MeanComplexity_s ~ 1 + Conservation.area + Park.size_s + PropInv_s,
        family = gaussian(), 
        data = data_park)
    ),
  
  tar_target(
    inv_sp_park,
    glm(formula = PropInvSp_s ~ 1 + Conservation.area + Park.size_s,
        family = gaussian(), 
        data = data_park)
    ),
  
  tar_target(
    inv_stems_park,
    glm(formula = PropInv_s ~ 1 + Conservation.area + Park.size_s,
        family = gaussian(), 
        data = data_park)
    )
  
  #tar_render(
  #  model_diagnostics,
  #  'figures/diagnostics/model_diagnostics.qmd'
  #)
  
  
)


## TODO 
# model 
# check model diagnostics 
# plot model outcomes 
# add code for figure 1 - map 
# model plot data?
# NOTE: if using plot data - missing some canopy and have 96 plots instead of 92