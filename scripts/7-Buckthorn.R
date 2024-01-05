
# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)


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
#remove dead trees
trees_adult <- trees_adult %>%
  filter(CommonName != "Dead")

# Proportion per Species (Invasive)--------------------------------------------------
inv_prop_sp <- trees_adult %>%
  group_by(Park) %>%
  mutate(nTrees = n()) %>%
  group_by(Park, Scientific.Name) %>%
  summarize(PropSpecies = n()/nTrees,
            Invasive = first(Invasive)) %>%
  filter(Invasive == 'Y') %>%
  distinct()


# Proportion per Species (Native) --------------------------------------------------
native_prop_sp <- trees_adult %>%
  group_by(Park) %>%
  mutate(nTrees = n()) %>%
  group_by(Park, Scientific.Name) %>%
  summarize(PropSpecies = n()/nTrees,
            Native_ETF = first(Native_ETF)) %>%
  filter(Native_ETF == 'Y') %>%
  distinct()


# Proportion Species - Type -----------------------------------------------

prop_sp <- trees_adult %>%
  group_by(Park, Scientific.Name) %>%
  summarize(Scientific.Name = first(Scientific.Name),
            Invasive = first(Invasive),
            Native = first(Native_ETF)) %>%
  na.omit() %>%
  group_by(Park) %>% 
  summarize(propN = sum(Native == "Y")/n(),
         propNN = sum(Native == "N" & Invasive == "N")/n(),
         propI = sum(Invasive == "Y")/n()) %>%
  pivot_longer(cols = propN:propI)
  
  

# Save --------------------------------------------------------------------

write.csv(inv_prop_sp, "output/ProportionInvasiveperSpecies.csv")


# Proportion of Species per Park  -----------------------------------------
prop <- read.csv("input/proportiondata.csv")

library(ggplot2)
ggplot(prop, aes(x = Parks, y = Proprtion, fill = Variable)) + 
  geom_bar(position = "fill", stat= "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = "Proportion")


ggplot(prop_sp, aes(x = Park, y = value, fill = name)) + 
  geom_bar(position = "fill", stat= "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = "Proportion")

