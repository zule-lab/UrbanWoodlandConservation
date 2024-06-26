
# Packages ----------------------------------------------------------------

library(dplyr)
library(sf)
library(tidyr)


# Data --------------------------------------------------------------------

parks <- read.csv("input/Park.densiometer.csv")
canopy <- read_sf("output/canopy_parks.gpkg")
invasive <- read.csv("output/ProportionInvasives.csv")
diversity <- read.csv("output/treediversity.csv")


# Cleaning ----------------------------------------------------------------

parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
parks_for <- parks_for %>%
  select(c(Park, Park.size, Park.age, Established, Conservation.area)) %>%
  distinct()

diversity$Order.q[diversity$Order.q == 0] <- 'SR'
diversity$Order.q[diversity$Order.q == 1] <- 'Shannon'
diversity <- filter(diversity, Order.q != 2)

diversity_w <- pivot_wider(diversity, id_cols = c(Assemblage), 
                           names_from = Order.q, 
                           values_from = c(qD, qD_Over, qD_Under)) %>%
  rename(Park = Assemblage)


canopy$Park[canopy$Park == 'Boisé-du-Saint-Sulpice'] <- 'Boisé-de-Saint-Sulpice'
canopy$Park[canopy$Park == 'Cap Saint-Jacques'] <- 'Cap-Saint-Jacques'
canopy$Park[canopy$Park == 'Coulée Grou'] <- 'Coulée-Grou'
canopy$Park[canopy$Park == "Ile-Bizard"] <- "Île-Bizard"
canopy$Park[canopy$Park == "L'Anse-À-L'Orme"] <- "L'Anse-à-l'Orme"


# Join --------------------------------------------------------------------

cd <- inner_join(canopy, diversity_w, by = "Park")
cdi <- inner_join(cd, invasive, by = "Park")
full <- inner_join(cdi, parks_for, by = "Park")

# Save --------------------------------------------------------------------

write_sf(full, "output/FullDataset.gpkg")

