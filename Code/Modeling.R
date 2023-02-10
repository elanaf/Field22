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

mdf.m1 <- glmmTMB(Invasive.Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

mdf.m2 <- glmmTMB(Native.Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2)


####UL

##Model - does invasive cover change with plot? 
mdf <- ul %>%
  filter(Date == "2022-09-16")

#there's not enough data to do the interaction
#i also figured there wasn't enoguh phrag so I did all invasives
mdf.m3 <- glmmTMB(Invasive.Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m3)


mdf.m4 <- glmmTMB(Native.Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m4)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)
