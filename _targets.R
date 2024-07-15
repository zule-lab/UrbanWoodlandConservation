# Source ------------------------------------------------------------------
library(targets)
tar_source('R')


# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')


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
    can_park,
    betareg(formula = percan ~ 1 + Conservation.area + Park.size_s + PropInv_s,
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
    betareg(formula = PropInvSp ~ 1 + Conservation.area + Park.size,
        data = data_park)
  ),
  
  tar_target(
    inv_stems_park,
    betareg(formula = PropInv ~ 1 + Conservation.area + Park.size,
        data = data_park)
  ),
  
  tar_target(
    model_diagnostics,
    diagnostics(sr_park, shan_park, complexity_park,
                can_park, inv_sp_park, inv_stems_park)
  ),
  
  tar_target(
    figure_1,
    make_figure_1("input/study_parks_spatial.gpkg")
  ),
  
  tar_target(
    figure_complexity,
    make_figure_complexity(complexity_park, data_park)
  ),
  
  tar_target(
    figure_invasive,
    make_figure_invasive(inv_stems_park, data_park)
  )
  
  
)


## TODO 
# supplementary info
# model plot data?
# NOTE: if using plot data - missing some canopy and have 96 plots instead of 92