
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


# Calculate Proportion Invasive Species -----------------------------------
inv_prop_sp <- trees_adult %>% 
  group_by(Park) %>% 
  distinct(Scientific.Name, .keep_all = T) %>% 
  select(c(Park, Scientific.Name, Invasive)) %>% 
  summarize(PropInvSp = sum(Invasive == "Y", na.rm = T)/n())


# Calculate Proportion Invasive Stems -------------------------------------

inv_prop <- trees_adult %>%
  group_by(Park) %>%
  summarize(PropInv = sum(Invasive == "Y", na.rm = T)/n())


inv <- inner_join(inv_prop_sp, inv_prop)

# Save --------------------------------------------------------------------
write.csv(inv, "output/ProportionInvasives.csv")
