make_figure_invasive <- function(inv_stems_park, data_park, trees_raw, trees_ranges){
  
  # A) invasive proportions plot
  
  # Data Cleaning -----------------------------------------------------------
  trees_for <- trees_raw[grepl("FOR", trees_raw[["PlotID"]]), ]
  
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
  
  # Removing Mini-Plot ------------------------------------------------------
  
  ranges <- rename(trees_ranges, SpCode = Species.Code)
  
  # filter out trees with DBH < 5 cm
  trees_adult <- trees_for %>%
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
  
  # remove individuals that only have genus assigned for elms, pears, hawthorn, and viburnum
  trees_adult <- trees_adult %>%
    filter(SpCode != "VISP" &
             SpCode != "PYSP" &
             SpCode != "ULSP" & 
             SpCode != "CRSP")
  #remove dead individuals
  trees_adult <- trees_adult %>%
    filter(CommonName != "Dead")
  
  # Calculate Proportion Invasive Stems -------------------------------------
  
  inv_prop <- trees_adult %>%
    drop_na() %>% 
    group_by(Park) %>%
    summarize(PropInv = sum(Invasive_QC == "Y", na.rm = T)/n(),
              PropNat = sum(Native_QC == "Y", na.rm = T)/n(),
              PropNonNat = (sum(Native_QC == "N" & Invasive_QC == "N", na.rm = T)/n())) %>% 
    mutate(Park_Type = case_when(Park == "Angrignon" ~ "Non-Status Woodland",
                                 Park == "Bois-de-Liesse" ~ "Nature Park",
                                 Park == "Bois-de-Saraguay" ~ "Nature Park", 
                                 Park == "Boisé-de-Saint-Sulpice" ~ "Non-Status Woodland",
                                 Park == "Cap-Saint-Jacques" ~ "Nature Park", 
                                 Park == "Coulée-Grou" ~ "Non-Status Woodland", 
                                 Park == "Île-Bizard" ~ "Nature Park", 
                                 Park == "Jean-Drapeau" ~ "Non-Status Woodland", 
                                 Park == "L'Anse-à-l'Orme" ~ "Nature Park",
                                 Park == "Pointe-aux-Prairies" ~ "Nature Park",
                                 Park == "Thomas-Chapais" ~ "Non-Status Woodland"))
  
  b <- inv_prop %>% 
    pivot_longer(!c(Park, Park_Type)) %>%
    ggplot(aes(x = Park, y = value, fill = name)) + 
    geom_col() + 
    theme_classic() + 
    scale_fill_manual(values= c("#7fa074", "#2c4b27", "#0e2810"), labels = c("Invasive", "Native", "Non-Native")) + 
    labs(x = "", y = "Proportion of Invasive Stems (%)", fill = "") + 
    theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 10, colour = "black"),
          legend.position = "top") + 
    facet_wrap(~ Park_Type, scales = "free_x")
  
  
  
  
  # B) model plot 
  
  p <- plot_predictions(inv_stems_park, by = "Conservation.area") + 
    geom_point(aes(x = Conservation.area, y = PropInv), data = data_park, position = position_dodge2(width = 0.1),
               colour = "grey21", alpha = 0.5)+ 
    scale_x_discrete(labels = c("Non-Status Woodland", "Nature Park")) + 
    theme_classic() + 
    labs(x = "", y = "Proportion of Invasive Stems (%)")+
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.text.x = element_text(angle = 90),
          axis.title = element_text(size = 12))
  
  # Full plot
  
  f <- b + p + plot_annotation(tag_levels = "a", tag_suffix = ")")
  
  ggsave('figures/invasive.png', f, width = 10, height = 6, units = "in", dpi = 450)
  
  
}