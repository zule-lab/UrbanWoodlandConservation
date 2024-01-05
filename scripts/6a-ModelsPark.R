#pakages 
library(performance)
library(visreg)

#Testing Collinearity 
read <- readRDS("output/FullDataset.rds")
age.size <- cor.test(read$Park.size, read$Park.age)
age.size
age.propin<- cor.test(read$Park.size, read$PropInv)
age.propin
size.propin <- cor.test(read$Park.age, read$PropInv)
size.propin
#none of them are highly correlated


# Model for Species Richness ----------------------------------------------
#Model for Species Richness for All Trees 
model.sr <- glm(qD_SR ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sr)
summary(model.sr) #Statistically Insignificant 

#model for species richness for Overstory trees 
model.sr.over <- glm(qD_Over_SR ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sr.over)
summary(model.sr.over) #Statistically insignificant

#Models for Species Richness for Understory
model.sr.under <- glm(qD_Under_SR ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sr.under)
summary(model.sr.under) #Statistically insignificant 


# Models for Species Diversity-----------------------------
#Model for species Diversity for all trees 
model.sd <- glm(qD_Shannon ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sd)
summary(model.sd) #Statistically Insignificant 

#Model for Species Diversity for overstory trees 
model.sd.over <- glm(qD_Over_Shannon ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sd.over)
summary(model.sd.over) #Insignificant 

#Model for Species Diversrity for Understory Trees 
model.sd.under <- glm(qD_Under_Shannon ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.sd.under)
summary(model.sd.under) #Insignificant 


# Model for Canopy Cover  -------------------------------------------------
model.canopycover <- glm(percan ~ Conservation.area + Park.size + Park.age + PropInv, data =read)
check_model(model.canopycover)
summary(model.canopycover) #Conservation status statistically significant 

visreg(model.canopycover, xvar = "Conservation.area", gg =T) + 
  labs(y = "Canopy Cover (%)", x = "Conservation Status") +
  theme_classic()
library(ggplot2)


