# Packages ----------------------------------------------------------------
p <- c("stringr", "dplyr", "tidyr", "tibble", "ggplot2", "iNEXT", "vegan")
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

# Plot diversity  ---------------------------------------------------------

# plots are 100% sampled and are the same size, so we will not calculate diversity indices with iNEXT

plots <- trees_for %>% 
  unite(Scientific.Name, c("Genus", "Species"), sep = " ") %>% 
  select(c(PlotID, Scientific.Name)) %>% 
  group_by(PlotID, Scientific.Name) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = "PlotID", names_from = 'Scientific.Name', values_from = "n", values_fill = 0,
              values_fn = first) %>% 
  mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  select(-(" ")) %>% 
  column_to_rownames("PlotID")

sr <- rowSums(plots)
shan <- diversity(plots, index = "shannon")

plot_div <- plots %>% 
  rownames_to_column("PlotID") %>% 
  select(PlotID)

plot_div <- cbind(plot_div, sr)
plot_div <- cbind(plot_div, shan)
rownames(plot_div) <- NULL

# Save --------------------------------------------------------------------

write.csv(trees_for, "output/cleanedtrees.csv")
write.csv(div, "output/treediversity_park.csv")
write.csv(plot_div, "output/treediversity_plot.csv")
