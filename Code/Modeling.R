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
#summarize the cover of all the natives and invasives (from graphing code)
#Pivot table so columns are response but species are rows
fb_short <- fb[-c(28,6, 23, 24, 29, 30, 31)] #removing notes column and things we don't want calculated (unknowns and total cover and measurements)
fb_end <- fb_short %>% #make it so it is only the final date
  filter(Date == "2022-09-16") 

fb_long <- fb_end %>% #pivot the table
  tidyr::pivot_longer(
    cols = 6:24, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  )

#groups natives and invasives
fb_split<- fb_long %>%
  dplyr::mutate(Status = 
                  dplyr::if_else(
                    SPP %in%
                      c("PHAU", "Typha", "Rust", "Tamarisk"), 
                    "Invasive", "Native"))


#make all cover values 0 < y < 1
fb_split$Percent_Cover <- fb_split$Percent_Cover/100

#get rid of NAs
fb_split$Percent_Cover[is.na(fb_split$Percent_Cover)] <- 0.005 #make 0s a trace amount - could be half the smallest amount

##Model - does invasive cover change with plot? 
mdf <- fb_split %>%
  filter(Status == "Invasive")


#there's not enough data to do the interaction
#i also figured there wasn't enoguh phrag so I did all invasives
mdf.m1 <- glmmTMB(Percent_Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m1)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

##Model - does native cover change with plot?
mdf <- fb_split%>%
  filter(Status == "Native")

#there's not enough data to do the interaction
#i also figured there wasn't enoguh phrag so I did all invasives
mdf.m2 <- glmmTMB(Percent_Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m2)


simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

####UL
ul_short <- ul[-c(6, 7, 9, 21, 35, 36, 37, 38)] #removing notes column and things we don't want calculated (unknowns and total cover and measurements)
ul_end <- ul_short %>% #make it so it is only the final date
  filter(Date == "2022-09-16") 

ul_wide <- ul_end %>% #pivot the table
  tidyr::pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  )

#groups natives and invasives
ul_split<- ul_wide %>%
  dplyr::mutate(Status = 
                  dplyr::if_else(
                    SPP %in%
                      c("PHAU", "TYPHA", "Rust", "Tamarisk", "ALPR", "CYDA", "BY", 
                        "BASC", "LASE"), 
                    "Invasive", "Native"))


#make all cover values 0 < y < 1
ul_split$Percent_Cover <- ul_split$Percent_Cover/100

#get rid of NAs?

##Model - does invasive cover change with plot? 
mdf <- ul_split%>%
  filter(Status == "Invasive")

#there's not enough data to do the interaction
#i also figured there wasn't enoguh phrag so I did all invasives
mdf.m3 <- glmmTMB(Percent_Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m3)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

##Model - does native cover change with plot?
mdf <- ul_split%>%
  filter(Status == "Native")


mdf.m4 <- glmmTMB(Percent_Cover ~ Group + Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



summary(mdf.m4)
#model specification probably okay because 12 obs and 3 blocks

simulateResiduals(mdf.m1, plot = T) 
#model fit is not very good - might be better if I don't change the 100s?
#the regular transformations don't work because beta so may need to try something else
plotResiduals(mdf.m1, form= mdf$Density)

