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
  
  zar_brms(
    sr_park,
    formula = SR_s ~ 1 + Conservation.area + Park.size_s + PropInv_s,
    family = gaussian(), 
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")),
    backend = 'cmdstanr',
    data = data_park,
    chains = 4,
    iter = 1000,
    cores = 4),
  
  zar_brms(
    shan_park,
    formula = Shannon_s ~ 1 + Conservation.area + Park.size_s + PropInv_s,
    family = gaussian(), 
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")),
    backend = 'cmdstanr',
    data = data_park,
    chains = 4,
    iter = 1000,
    cores = 4),
  
  zar_brms(
    complexity_park,
    formula = MeanComplexity_s ~ 1 + Conservation.area + Park.size_s + PropInv_s,
    family = gaussian(), 
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")),
    backend = 'cmdstanr',
    data = data_park,
    chains = 4,
    iter = 1000,
    cores = 4),
  
  zar_brms(
    inv_sp_park,
    formula = PropInvSp_s ~ 1 + Conservation.area + Park.size_s,
    family = gaussian(), 
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")),
    backend = 'cmdstanr',
    data = data_park,
    chains = 4,
    iter = 1000,
    cores = 4),
  
  zar_brms(
    inv_stems_park,
    formula = PropInv_s ~ 1 + Conservation.area + Park.size_s,
    family = gaussian(), 
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(exponential(1), class = "sigma")),
    backend = 'cmdstanr',
    data = data_park,
    chains = 4,
    iter = 1000,
    cores = 4),
  
  tar_target(
    model_list,
    list(sr_park_brms_sample, shan_park_brms_sample, complexity_park_brms_sample, inv_sp_park_brms_sample, inv_stems_park_brms_sample) %>% 
      setNames(., c('SR', 'Shannon', 'Complexity', 'Invasive_Species', 'Invasive_Stems'))
  ),
  
  tar_target(
    prior_model_list,
    list(sr_park_brms_sample_prior, shan_park_brms_sample_prior, complexity_park_brms_sample_prior, inv_sp_park_brms_sample_prior, inv_stems_park_brms_sample_prior) %>% 
      setNames(., c('SR', 'Shannon', 'Complexity', 'Invasive_Species', 'Invasive_Stems'))
  ),
  
  tar_render(
    prior_predictive,
    'figures/diagnostics/prior_predictive.qmd'
  ),
  
  tar_render(
    model_diagnostics,
    'figures/diagnostics/model_diagnostics.qmd'
  )
  
  
)


## TODO 
# model 
# check priors 
# check model diagnostics 
# plot model outcomes 
# add code for figure 1 - map 
# model plot data?
# NOTE: if using plot data - missing some canopy and have 96 plots instead of 92