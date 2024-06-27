park_data <- function(trees_raw, ranges, parks, canopy, parks_spatial){
  
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
  
  div <- estimateD(trees_list_inext, datatype="incidence_raw",
                   base="coverage", level=0.7828, conf=0.95)
  
  
  # Canopy Cover ------------------------------------------------------------
  # use 2021 LiDAR data from CMM to recalculate canopy cover of each plot + park
  cancov <- read_stars(canopy)
  # park polygons
  parks_spatial <- read_sf(parks_spatial)
  
  # convert to same projection as canopy cover 
  parks_trans <- st_transform(parks_spatial, st_crs(cancov))
  
  # select forested past land use 
  parks_for <- parks_trans[parks_trans$PastLandUse == "Forested", ]
  
  parks_can <- aggregate(cancov, parks_for, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>% 
    rename(percan = `660_IndiceCanopee_2021.tif`) %>%
    mutate(Park = parks_for$Name)
  
  
  # Removing Mini-Plot ------------------------------------------------------
  
  ranges <- rename(ranges, SpCode = Species.Code)
  
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
  
  
  # Park level --------------------------------------------------------------
  
  parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
  parks_sel <- parks_for %>%
    select(c(Park, Park.size, Park.age, Established, Conservation.area)) %>%
    distinct()
  
  div$Order.q[div$Order.q == 0] <- 'SR'
  div$Order.q[div$Order.q == 1] <- 'Shannon'
  div <- filter(div, Order.q != 2)
  
  diversity_w <- pivot_wider(div, id_cols = c(Assemblage), 
                             names_from = Order.q, 
                             values_from = c(qD)) %>%
    rename(Park = Assemblage)
  
  
  parks_can$Park[parks_can$Park == 'Boisé-du-Saint-Sulpice'] <- 'Boisé-de-Saint-Sulpice'
  parks_can$Park[parks_can$Park == 'Cap Saint-Jacques'] <- 'Cap-Saint-Jacques'
  parks_can$Park[parks_can$Park == 'Coulée Grou'] <- 'Coulée-Grou'
  parks_can$Park[parks_can$Park == "Ile-Bizard"] <- "Île-Bizard"
  parks_can$Park[parks_can$Park == "L'Anse-À-L'Orme"] <- "L'Anse-à-l'Orme"
  
  complexity <- parks_for %>% 
    select(Park, PlotID, VegetationLevels) %>% 
    mutate(Complexity = stri_count_words(VegetationLevels)) %>% 
    group_by(Park) %>% 
    summarize(MeanComplexity = mean(Complexity))
  
  cd <- inner_join(parks_can, diversity_w, by = "Park")
  cdi <- inner_join(cd, inv, by = "Park")
  cdic <- inner_join(cdi, complexity, by = "Park")
  full <- inner_join(cdic, parks_sel, by = "Park")
  
  full_s <- full %>% 
    select(-c(Park.age, Established)) %>% 
    mutate(across(where(is.numeric), ~ scale(.x)[,1])) %>% 
    st_set_geometry(NULL) %>%
    inner_join(., full, by = "Park", suffix = c("_s", ""))
  
  return(full_s)
  
}