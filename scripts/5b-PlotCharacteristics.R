
# Packages ----------------------------------------------------------------

library(dplyr)
library(sf)
library(tidyr)


# Data --------------------------------------------------------------------

parks <- read.csv("input/Park.densiometer.csv")
invasive <- read.csv("output/ProportionInvasivesPlot.csv")


# Cleaning ----------------------------------------------------------------

parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
parks_for <- parks_for %>%
  select(c(Park, PlotID, Park.size, Park.age, Established, Conservation.area, Complexity)) %>%
  distinct()



# Join --------------------------------------------------------------------

fullplot <- inner_join(parks_for, invasive, by = "PlotID")


# Save --------------------------------------------------------------------

saveRDS(fullplot, "output/FullDatasetPlot.rds")

