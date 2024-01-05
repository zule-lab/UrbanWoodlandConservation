
# Packages ----------------------------------------------------------------
library(dplyr)


# Load Data ---------------------------------------------------------------

ranges <- read.csv("input/species_ranges.csv")
trees <- read.csv("output/cleanedtrees.csv")

ranges <- rename(ranges, SpCode = Species.Code)


# Removing Mini-Plot ------------------------------------------------------

# filter out trees with DBH < 5 cm
trees_adult <- trees %>%
  filter(DBH != "2" & 
           DBH != "4" &  
           DBH != "" & 
           DBH != "1.3" &
           DBH != "1.5+1.7" & 
           DBH != "04-Jan" & 
           DBH != "05-Jan" & 
           DBH != "06-Jan" &
           DBH != "07-Jan")



# Assign Invasives --------------------------------------------------------

# attribute each tree with invasive status based on species code 
trees_adult <- left_join(trees_adult, ranges, by = "SpCode")

# remove individuals that only have genus assigned for elms, pears, and willows
trees_adult <- trees_adult %>%
  filter(SpCode != "SASP" &
           SpCode != "PYSP" &
           SpCode != "ULSP")
#remove dead individuals
trees_adult <- trees_adult %>%
  filter(CommonName != "Dead")

# Calculate Proportion Invasive -------------------------------------------

# per park 
inv_prop <- trees_adult %>%
  group_by(Park) %>%
  summarize(PropInv = sum(Invasive == "Y", na.rm = T)/n())

# per plot
inv_prop_plot <- trees_adult %>%
  group_by(PlotID) %>%
  summarize(PropInv = sum(Invasive == "Y", na.rm = T)/n())


# Extra  ------------------------------------------------------------------
#Proportion of Non-native species per park
inv_Non_Native <- trees_adult %>%
  group_by(Park) %>%
  summarize(PropNN = sum(Native_ETF == "N" & Invasive == "N", na.rm = T)/n())

#Proportion of Native per Park
inv_Native <- trees_adult %>%
  group_by(Park) %>%
  summarize(PropNative = sum(Native_ETF == "Y", na.rm = T)/n())


# Save --------------------------------------------------------------------
write.csv(inv_prop, "output/ProportionInvasives.csv")
write.csv(inv_prop_plot, "output/ProportionInvasivesPlot.csv")
write.csv(inv_Native, "output/ProportionNatives.csv")
write.csv(inv_Non_Native, "output/ProportionNonNative.csv")
