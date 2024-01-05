
# Packages ----------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(sf)


# Model for Invasive Species  ---------------------------------------------
D <- readRDS("output/FullDataset.rds")
D <- st_set_geometry(D, NULL)

ggboxplot(data = D, x = "Conservation.area", y = "PropInv", add = "jitter")+
  labs( x = "Conservation Status", y = "Proportion of Invasives")

#Model for Native Species  ----------------------------------------------
ggboxplot(data = D, x = "Conservation.area", y = "PropNative", add = "jitter")+
  labs( x = "Conservation Status", y = "Proportion of Native")
#But Proportion of Native Species is highly correlated to Invasive Prop 



