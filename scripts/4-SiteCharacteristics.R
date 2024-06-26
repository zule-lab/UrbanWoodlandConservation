
# Packages ----------------------------------------------------------------

library(dplyr)
library(sf)
library(tidyr)
library(stringi)


# Data --------------------------------------------------------------------

parks <- read.csv("input/Park.densiometer.csv")
canopy <- read_sf("output/canopy_parks.gpkg")
canopy_plots <- read_sf("output/canopy_points.gpkg")
invasive <- read.csv("output/ProportionInvasives.csv")
invasive_plots <- read.csv("output/ProportionInvasivesPlot.csv")
diversity <- read.csv("output/treediversity_park.csv")
diversity_plots <- read.csv("output/treediversity_plot.csv")



# Park level --------------------------------------------------------------

parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
parks_sel <- parks_for %>%
  select(c(Park, Park.size, Park.age, Established, Conservation.area)) %>%
  distinct()

diversity$Order.q[diversity$Order.q == 0] <- 'SR'
diversity$Order.q[diversity$Order.q == 1] <- 'Shannon'
diversity <- filter(diversity, Order.q != 2)

diversity_w <- pivot_wider(diversity, id_cols = c(Assemblage), 
                           names_from = Order.q, 
                           values_from = c(qD)) %>%
  rename(Park = Assemblage)


canopy$Park[canopy$Park == 'Boisé-du-Saint-Sulpice'] <- 'Boisé-de-Saint-Sulpice'
canopy$Park[canopy$Park == 'Cap Saint-Jacques'] <- 'Cap-Saint-Jacques'
canopy$Park[canopy$Park == 'Coulée Grou'] <- 'Coulée-Grou'
canopy$Park[canopy$Park == "Ile-Bizard"] <- "Île-Bizard"
canopy$Park[canopy$Park == "L'Anse-À-L'Orme"] <- "L'Anse-à-l'Orme"

complexity <- parks_for %>% 
  select(Park, PlotID, VegetationLevels) %>% 
  mutate(Complexity = stri_count_words(VegetationLevels)) %>% 
  group_by(Park) %>% 
  summarize(MeanComplexity = mean(Complexity))

cd <- inner_join(canopy, diversity_w, by = "Park")
cdi <- inner_join(cd, invasive, by = "Park")
cdic <- inner_join(cdi, complexity, by = "Park")
full <- inner_join(cdic, parks_sel, by = "Park")


# Plot level --------------------------------------------------------------
parks_sel_plot <- parks_for %>%
  select(c(Park, PlotID, Park.size, Park.age, Established, Conservation.area, VegetationLevels)) %>%
  distinct() %>% 
  mutate(Complexity = stri_count_words(VegetationLevels))

cdp <- full_join(canopy_plots, diversity_plots, by = join_by("Park" == "PlotID"))
cdip <- full_join(cd, invasive_plots, by = join_by("Park" == "PlotID"))
full_plots <- full_join(cdip, parks_sel_plot, by = join_by("Park" == "PlotID"))
# missing some plot geometries + have 2 extra

# Save --------------------------------------------------------------------

write_sf(full, "output/FullDataset_Parks.gpkg")
write_sf(full_plots, "output/FullDataset_Plots.gpkg")
