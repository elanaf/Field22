load("clean_dfs.RData")
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(multcompView)
library(gridExtra)
options(contrasts = c("contr.sum", "contr.poly"))

fb$Group <- as.factor(fb$Group)
fb$Density <- as.factor(fb$Density)
ul$Group <- as.factor(ul$Group)
ul$Density <- as.factor(ul$Density)

####FB
#only need the last date
mdf <- fb %>%
  filter(Date == "2022-09-16") %>%
  select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m1 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) #must have data for every factor level

mdf.m2 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= useData$Density) #must have data for every factor level

emmip(mdf.m2, Group~Density, CIs = T) #looks significant without CI but the CI shows it isn't
car::Anova(mdf.m2) #no interaction but at least one group different from another
#seems like 1 + 2 are same, 3+4 are the same, and they are different from each other, 5 probably in the middle 

emmip(mdf.m2, Group~Density, CIs = T, type = "response")

emmeans(mdf.m2, pairwise~Group) #not significant when adjusted for the tukey test
emmeans(mdf.m2, pairwise~Group, adjust = "none") #a more liberal test shows what we expected above with the groups
#there is some suggestion that there might be some differences but the study fails to support these differences - suggestion without support
####UL

##Model - does invasive cover change with plot? 
mdf <- ul %>%
  filter(Date == "2022-09-16")

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

mdf.m3 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3)
simulateResiduals(mdf.m3, plot = T) 
plotResiduals(mdf.m3, form= useData$Density)


#compare a model where you nudge the 100s down to one where you just do a log normal and see if there is a difference
#this log normal one is good enough and shows nothing significant 
mdf.m4 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

mdf.m4 <- glmmTMB(log(Native.Cover) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= mdf$Density)
car::Anova(mdf.m4) #nothing significant
emmip(mdf.m4, Group~Density)
emmip(mdf.m4, Density~Group, CIs = T)

####To compare the treatments to the control
mdf$gd <- factor(mdf$Group:mdf$Density)
mdf.m1 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary 
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$gd)

#Dunnetts test - type 1 error control, comparing the control to each of 10 other means
#We have inflated type 1 error because we use the control mean 10 times
#We could use a tukeys but that would control for 55 tests and we only need 10 - we will lose all power
#Allows us to just look at control against every other treatment - only the comparisons we want
emmeans(mdf.m1, specs = trt.vs.ctrlk~gd,ref = 3) #reference group is the third option (10:C)
#4L is the closest to being different, but nothing else
#4L is the largest mean and 10C is pretty small 

mdf$gd <- factor(mdf$Group:mdf$Density)
mdf.m1 <- glmmTMB(Native.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$gd)

#Dunnetts test - type 1 error control, comparing the control to each of 10 other means
#We have inflated type 1 error because we use the control mean 10 times
#We could use a tukeys but that would control for 55 tests and we only need 10 - we will lose all power
#Allows us to just look at control against every other treatment - only the comparisons we want
emmeans(mdf.m1, specs = trt.vs.ctrlk~gd,ref = 3) #reference group is the third option (10:C)
#4L is the closest to being different, but nothing else
#4L is the largest mean and 10C is pretty small 