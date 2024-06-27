cleaned_trees <- function(trees_raw) {
  
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
  
  
  return(trees_for)
}