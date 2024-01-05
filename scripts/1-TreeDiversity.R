# Packages ----------------------------------------------------------------
p <- c("stringr", "dplyr", "tidyr", "tibble", "ggplot2", "iNEXT", "sf", "stars")
lapply(p, library, character.only = T)


# Data Input --------------------------------------------------------------
trees <- read.csv("input/trees_openrefine.csv")


# Data Cleaning -----------------------------------------------------------
trees_for <- trees[grepl("FOR", trees[["PlotID"]]), ]

# fix Excel-formatted dates in DBH columns
trees_for$DBH[trees_for$DBH %in% c('03-Jan', '05-Mar')] <- c(2, 4)

# create species codes
trees_for <- trees_for %>%
  mutate(SpCode = toupper(paste(str_sub(trees_for$Genus, start = 1, end = 2), str_sub(trees_for$Species, start = 1, end = 2), sep = "")))
# create unique code for dead trees as they don't have genus/species names
trees_for$SpCode[trees_for$CommonName == "Dead"] <- 'DEAD'
# create unique code for unknown species as they don't have genus/species names
trees_for$SpCode[trees_for$CommonName == "Unknown"] <- 'UNK'
# replace sugar maple code since it is the same as silver maple
trees_for$SpCode[trees_for$CommonName == "Sugar Maple"] <- 'ACSC' 



# Species Subset ----------------------------------------------------------
# make sure park, plots, and species are factors
trees_for$Park <- as.factor(trees_for$Park)
trees_for$PlotID <- as.factor(trees_for$PlotID)
trees_for$SpCode <- as.factor(trees_for$SpCode)

trees_for_overstory <- trees_for[trees_for$Overstory.Understory == "Overstory", ]
trees_for_understory <- trees_for[trees_for$Overstory.Understory == "Understory", ]


# Diversity Indices -------------------------------------------------------
# Full Tree Dataset
trees_list_inext <- trees_for %>% 
  group_by(Park) %>% 
  group_map(~ group_by(.x, PlotID, SpCode) %>% 
              tally() %>% 
              pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
              mutate_if(is.numeric, ~1 * (. != 0)) %>%
              column_to_rownames("SpCode")) %>%
  setNames(unique(trees_for$Park))

out <- iNEXT(trees_list_inext, datatype = "incidence_raw", q=0)

ggiNEXT(out, type = 3)+ xlim(c(0,1)) + 
  theme_classic(base_size = 15) + 
  scale_fill_discrete() + 
  scale_color_discrete()

div <- estimateD(trees_list_inext, datatype="incidence_raw",
                 base="coverage", level=0.7828, conf=0.95)

# overstory trees 
trees_list_inext_overstory <- trees_for_overstory %>% 
  group_by(Park) %>% 
  group_map(~ group_by(.x, PlotID, SpCode) %>% 
              tally() %>% 
              pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
              mutate_if(is.numeric, ~1 * (. != 0)) %>%
              column_to_rownames("SpCode")) %>%
  setNames(unique(trees_for_overstory$Park))

out_over <- iNEXT(trees_list_inext_overstory, datatype = "incidence_raw", q=0)

ggiNEXT(out_over, type = 3)+ xlim(c(0,1)) + 
  theme_classic(base_size = 15) + 
  scale_fill_discrete() + 
  scale_color_discrete()

div_overstory <- estimateD(trees_list_inext_overstory, datatype="incidence_raw",
                           base="coverage", level=0.7159, conf=0.95)

# understory trees 
trees_list_inext_understory <- trees_for_understory %>% 
  group_by(Park) %>% 
  group_map(~ group_by(.x, PlotID, SpCode) %>% 
              tally() %>% 
              pivot_wider(id_cols = SpCode, names_from = PlotID, values_from = n, values_fill = 0) %>%
              mutate_if(is.numeric, ~1 * (. != 0)) %>%
              column_to_rownames("SpCode")) %>%
  setNames(unique(trees_for_understory$Park))

out_under <- iNEXT(trees_list_inext_understory, datatype = "incidence_raw", q=0)

ggiNEXT(out_under, type = 3)+ xlim(c(0,1)) + 
  theme_classic(base_size = 15) + 
  scale_fill_discrete() + 
  scale_color_discrete()

div_understory <- estimateD(trees_list_inext_understory, datatype="incidence_raw",
                            base="coverage", level=0.6343, conf=0.95)

# join 
div_full <- left_join(div, div_overstory, suffix = c("", "_Over"), by = c("Assemblage", "Order.q"))
div_full <- left_join(div_full, div_understory, suffix = c("", "_Under"), by = c("Assemblage", "Order.q"))


# Save --------------------------------------------------------------------

write.csv(trees_for, "output/cleanedtrees.csv")
write.csv(div_full, "output/treediversity.csv")
