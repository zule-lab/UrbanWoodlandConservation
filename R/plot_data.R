plot_data <- function(trees_raw, ranges, parks, canopy, plots_spatial){
  
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
  
  
  # Canopy Cover ------------------------------------------------------------
  # use 2021 LiDAR data from CMM to recalculate canopy cover of each plot + park
  cancov <- read_stars(canopy)
  # sampling points from GPS
  pts <- read_sf(plots_spatial)
  
  # convert to same projection as canopy cover 
  pts_trans <- st_transform(pts, st_crs(cancov))
  
  # select forested past land use 
  pts_for <- pts_trans[grepl("FOR", pts_trans[["Name"]]), ]
  
  # create 20 m square buffers around sampling points representing plots 
  plots <- st_buffer(pts_for, dist = 20, nQuadSegs=1, endCapStyle = "SQUARE")
  
  # calculate canopy cover by dividing number of pixels == 4 (canopy) by total number of pixels within a buffer
  pts_can <- aggregate(cancov, plots, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>% 
    rename(percan = `660_IndiceCanopee_2021.tif`) %>%
    mutate(Park = plots$Name)
  
  
  
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
  
  inv_prop_sp_plot <- trees_adult %>% 
    group_by(PlotID) %>% 
    distinct(Scientific.Name, .keep_all = T) %>% 
    select(c(Park, Scientific.Name, Invasive)) %>% 
    summarize(PropInvSp = sum(Invasive == "Y", na.rm = T)/n())
  
  
  # Calculate Proportion Invasive Stems -------------------------------------
  
  inv_prop_plot <- trees_adult %>%
    group_by(PlotID) %>%
    summarize(PropInv = sum(Invasive == "Y", na.rm = T)/n())
  
  inv_plot <- inner_join(inv_prop_sp_plot, inv_prop_plot)
  
  # full dataset
  
  parks_for <- parks[grepl("FOR", parks[["PlotID"]]), ]
  
  parks_sel_plot <- parks_for %>%
    select(c(Park, PlotID, Park.size, Park.age, Established, Conservation.area, VegetationLevels)) %>%
    distinct() %>% 
    mutate(Complexity = stri_count_words(VegetationLevels))
  
  cdp <- full_join(pts_can, plot_div, by = join_by("Park" == "PlotID"))
  cdip <- full_join(cdp, inv_plot, by = join_by("Park" == "PlotID"))
  full_plots <- full_join(cdip, parks_sel_plot, by = join_by("Park" == "PlotID"))
  
  
  full_s <- full_plots %>% 
    select(-c(Park.age, Established)) %>% 
    mutate(across(where(is.numeric), ~ scale(.x)[,1])) %>% 
    st_set_geometry(NULL) %>%
    inner_join(., full_plots, by = "Park", suffix = c("_s", ""))
  
  return(full_s)
  
}