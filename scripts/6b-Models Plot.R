
# Packages  ---------------------------------------------------------------
library(performance)
library(visreg)
library(ggplot2)

 
# Complexity   ----------------------------------------------------
PI <- readRDS("output/FullDatasetPlot.rds")
PI$Complexity <- as.factor(PI$Complexity)

model.PI <- glm(Complexity ~ Conservation.area + Park.size + PropInv, family = binomial(), data = PI)
check_model(model.PI)
summary(model.PI)

visreg(model.PI, xvar = "Conservation.area", gg =T) + 
  labs(y = "Complexity", x = "Conservation Status") +
  theme_classic()

