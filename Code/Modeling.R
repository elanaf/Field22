load("clean_dfs.RData")
library(tidyverse)
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

#FB ####
# These first 2 are good examples to follow

#only need the last date
mdf <- fb %>%
  filter(Date == "2022-09-16") %>%
  select(Block, Group, Density, Date, Invasive.Cover, Native.Cover)

useData <- filter(mdf, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

##FB Native ####
mdf.m1 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) #must have data for every factor level

emmip(mdf.m1, Group~Density, CIs = T) #looks significant without CI but the CI shows it isn't
car::Anova(mdf.m1) #no interaction but at least one group different from another
#seems like 1 + 2 are same, 3+4 are the same, and they are different from each other, 5 probably in the middle 

emmip(mdf.m1, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m1, pairwise~Group) #not significant when adjusted for the tukey test
emmeans(mdf.m1, pairwise~Group, adjust = "none") #a more liberal test shows what we expected above with the groups
#there is some suggestion that there might be some differences but the study fails to support these differences - suggestion without support

##FB Invasive trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m2) #don't use this summary 
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= mdf$gd)

#Dunnetts test - type 1 error control, comparing the control to each of 10 other means
#We have inflated type 1 error because we use the control mean 10 times
#We could use a tukeys but that would control for 55 tests and we only need 10 - we will lose all power
#Allows us to just look at control against every other treatment - only the comparisons we want
emmeans(mdf.m2, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#4L is the closest to being different, but nothing else
#4L is the largest mean and 10C is pretty small, so that's why they are different

#so nothing is different from the control so doesn't necessarily seem like any differences are related to treatment

##FB native trt vs ctl ####
mdf$gd <- factor(mdf$Group:mdf$Density)
mdf.m3 <- glmmTMB(Native.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m3) #don't use this summary
simulateResiduals(mdf.m3, plot = T)  #residuals not great on this one
plotResiduals(mdf.m3, form= mdf$gd)

emmeans(mdf.m3, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences between treatment and control 

##FB Invasive ####
mdf.m4 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= useData$Density) 


emmip(mdf.m4, Group~Density, CIs = T) #looks significant without CI but the CI shows it isn't
car::Anova(mdf.m4) #no interaction but densities different and at least one group different

emmip(mdf.m4, Group~Density, CIs = T, type = "response") #shows on the level of the response

emmeans(mdf.m4, pairwise~Group) #only 4 and 5 different when adjusted for tukey
emmeans(mdf.m4, pairwise~Group, adjust = "none") #same as with the tukey
#seems like trt 4 had lower cover overall compared to trt 5 but neither are different from the control so idk

emmeans(mdf.m4, pairwise~Density) #not significant but almost significant when adjusted for with tukey
emmeans(mdf.m4, pairwise~Density, adjust = "none") #same p-value with the more liberal test
#also, low density seems to have higher invasive cover than high density, but again neither different from the control 

#UL ####
mdf1 <- ul %>%
  filter(Date == "2022-09-16")

useData <- filter(mdf1, Density != "C") #to make the plotResiduals work
useData$Group <- factor(useData$Group)
useData$Density <- factor(useData$Density)

##UL Invasive trt vs ctl ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m5 <- glmmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m5) #don't use this summary 
simulateResiduals(mdf.m5, plot = T) 
plotResiduals(mdf.m5, form= mdf$gd)

emmeans(mdf.m5, specs = trt.vs.ctrlk~gd,ref = 3) #reference group is the third option (10:C)
#no significant differences, just a slight different with 4H

##UL Invasive ####
mdf.m6 <- glmmTMB(Invasive.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m6)
simulateResiduals(mdf.m6, plot = T) 
plotResiduals(mdf.m6, form= useData$Density)

emmip(mdf.m6, Group~Density, CIs = T) 
car::Anova(mdf.m6) #nothing significant

## UL Native ####
#compare a model where you nudge the 100s down to one where you just do a log normal and see if there is a difference

###Log normal ####
#this log normal one is good enough and shows nothing significant 
mdf.m7 <- glmmTMB(log(Native.Cover) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


summary(mdf.m7)
simulateResiduals(mdf.m7, plot = T) 
plotResiduals(mdf.m7, form= useData$Density)
car::Anova(mdf.m7) #nothing significant

emmip(mdf.m7, Group~Density, CIs = T)

###Beta with nudged values ####
useData$Native.Cover[useData$Native.Cover >= 1] <- .999
mdf.m8 <- glmmTMB(Native.Cover ~ Group * Density #* for interaction
                  + (1|Block),
                  data = useData,
                  family = beta_family, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m8)
simulateResiduals(mdf.m8, plot = T) #residuals look a little worse so I am inclined to use the log normal
plotResiduals(mdf.m8, form= useData$Density)

emmip(mdf.m8, Group~Density, CIs = T)
car::Anova(mdf.m8) #however, this does show a significant interaction

emmip(mdf.m8, Group~Density, CIs = T, type = "response") #shows on the level of the response

mdf.m8.emm <- emmeans(mdf.m8, ~Group * Density)
pairs(mdf.m8.emm, simple = "Density") #looks like Group 3 has the interaction, higher native cover in the low density

##UL Native trt vs ctl ####
### Dunnetts gaussian ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf.m9 <- glmmTMB(log(Native.Cover) ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = gaussian,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m9) #don't use this summary 
simulateResiduals(mdf.m9, plot = T) 
plotResiduals(mdf.m9, form= mdf$gd)

emmeans(mdf.m9, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences, 5H and 2H seem kind of different 

###Dunnetts beta ####
mdf1$gd <- factor(mdf1$Group:mdf1$Density) #compares every combination of treatment and control
mdf1$Native.Cover[mdf1$Native.Cover >= 1] <- .999

mdf.m10 <- glmmTMB(Native.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf1,
                  family = beta_family,
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

summary(mdf.m10) #don't use this summary 
simulateResiduals(mdf.m10, plot = T)#residuals look pretty bad, don't use
plotResiduals(mdf.m10, form= mdf$gd)

emmeans(mdf.m10, specs = trt.vs.ctrlk~gd,ref = 3, type = "response") #reference group is the third option (10:C)
#no significant differences

# Do it again but only with the seeded, not the control ####
#Can't compare the treatments to the control in terms of seeded the way I currently have it,
#so just did the comparison between the treatments

##fb ####
fb2 <-fb %>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, Cheno, Typha, 
         BOMA, DISP, EUMA, SYCI, LEFA, SCAC, BICE, BIFR, EUOC, MUAS, SCAM, RUMA,
         RUST, Unk_Bulrush, SARU, Tamarisk) %>%  #remove unnecessary columns
  filter(Date == "2022-09-16") %>%  #only the last sampling date
  pivot_longer(
    cols = 6:24, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", "Tamarisk") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP", "MUAS") & Group == 3 ~ "Seeded",
    SPP %in% c("EUOC", "EUMA") & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"))%>%  #make a new column for species status
  group_by(Block, Plot, Density, Group, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) %>%  #calculate totals
  pivot_wider(names_from = Status,
              values_from = PC) %>%  #make wide again
  filter(Density != "C") #idk I did this before, I think to converge

#remove NAs and 0s - make one half the smallest value
fb2$Seeded[fb2$Seeded %in% c(0, NA)] <- 0.0025

fb2$Group <- factor(fb2$Group)
fb2$Density <- factor(fb2$Density)

mdf.m1 <- glmmTMB(log(Seeded) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = fb2,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)

                  
summary(mdf.m1) #don't use this summary
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= useData$Density) #must have data for every factor level
#not the best fit - may need to fix with Susan

emmip(mdf.m1, Group~Density, CIs = T) 
car::Anova(mdf.m1) 
emmeans(mdf.m1, pairwise~Group, type = 'response', adjust = 'tukey')
#significant differences between groups - 
#perennial forb and juncus are similar,
#annual forb and grass are similar, 
#grass and bulrush are similar
emmeans(mdf.m1, pairwise~Density, type = "response", adjust = "tukey")
#marginally significant differences in density - higher seeded cover in high density than low density

###how to add the letters to a graph ####
library(multcomp)
emm1 <- emmeans(mdf.m1, pairwise~Group, CIs = T, type = 'response', adjust = 'tukey')
data1 <- multcomp::cld(emm1, alpha = 0.1, Letters = letters)

ggplot(data = data1, aes(x = Group, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Seed Mix", y = "Model predicted percent cover") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

ggsave("model_means_seeded_fb.jpeg")

emm2 <- emmeans(mdf.m1, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data2 <- multcomp::cld(emm2, alpha = 0.1, Letters = letters)
ggplot(data = data2, aes(x = Density, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Density", y = "Model predicted percent cover") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

ggsave("model_means_density_fb.jpeg")

##ul ####
ul2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
         Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
         EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>%
  filter(Date == "2022-09-16") %>% 
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% 
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>% 
  mutate(Plot = case_when(
    Plot == "1H" ~ "Perennial forbs (high)",
    Plot == "1L" ~ "Perennial forbs (low)",
    Plot == "2H" ~ "Rushes (high)",
    Plot == "2L" ~ "Rushes (low)",
    Plot == "3H" ~ "Grasses (high)",
    Plot == "3L" ~ "Grasses (low)",
    Plot == "4H" ~ "Bulrushes (high)",
    Plot == "4L" ~ "Bulrushes (low)",
    Plot == "5H" ~ "Annual forbs (high)",
    Plot == "5L" ~ "Annual forbs (low)",
    Plot == "C" ~ "Control",
    TRUE ~ "PROBLEM"
  )) %>%  #make a new column for species status
  group_by(Block, Plot, Density, Group, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) %>%  #calculate totals
  pivot_wider(names_from = Status,
              values_from = PC) %>%  #make wide again
  filter(Density != "C") #idk I did this before, I think to converge

#remove NAs and 0s - make one half the smallest value
ul2$Seeded[ul2$Seeded %in% c(0, NA)] <- 0.025

ul2$Group <- factor(ul2$Group)
ul2$Density <- factor(ul2$Density)

mdf.m2 <- glmmTMB(log(Seeded) ~ Group * Density #* for interaction
                  + (1|Block),
                  data = ul2,
                  family = gaussian, #because cover
                  control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


summary(mdf.m2) #don't use this summary
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= useData$Density) #must have data for every factor level

emmip(mdf.m2, Group~Density, CIs = T) 
car::Anova(mdf.m2) 
#Group and density significant with one interaction
#In general, annual forbs and bulrushes higher
#In general, high density higher
#Interaction must be in grass, which is much higher in high than low whereas other groups are mostly the same
emmeans(mdf.m2, pairwise~Group|Density, type = 'response', adjust = 'tukey')

###how to add the letters to a graph ####
emm3 <- emmeans(mdf.m2, pairwise~Group|Density, type = 'response', adjust = 'tukey')
data3 <- multcomp::cld(emm3, alpha = 0.1, Letters = letters)

ggplot(data = data3, aes(x = Group, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  facet_wrap(~Density) +
  labs(x="Seed Mix", y = "Model predicted percent cover") +
  geom_text(aes(label = .group,  y = response * 100),
          nudge_x = 0.2) 

ggsave("model_means_interaction_ul.jpeg")
