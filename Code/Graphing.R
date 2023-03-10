load("clean_dfs.RData")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

####FB####

####General graph#####
#trt x control to see how they look compared to the control 
#see if it looks liek things came up the way they were supposed to 
fb_1 <- fb %>% filter(Date == "2022-09-16") 

#things I planted that came up: BOMA, DISP, EUMA, SYCI, SCAC, BICE, BIFR, EUOC, MUAS, SCAM, RUMA
fb_1 %>% ggplot(aes(x = Plot, y = SYCI
                    )) +
  geom_point()

#Things that only came up where they were supposed to: BIFR, BICE, EUMA, SYCI, SCAC, EUOC, SCAM
#Things that look like more came up where planted:DISP (maybe slightly)
#Things that didn't come up in control: BOMA - but came up in places where it wasn't planted as much as where was planted so idk,
                                        #RUMA - came up the same amount in both 5 and 1
#PHAU might be slightly more in 4 and none in 5 but same for everything else

#look to see if the measurements seem to be related to anything
fb_1%>% ggplot(aes(x = Plot, y = Measurement.3)) +
  geom_point()

#Measurement 1 highest in 2L and 3H
#Measurement 2 highest in 1H, 1L, and 2H
#Measurement 3 highest in 1H, 3H, and C

####Invasive vs native####
#Pivot table so columns are response but species are rows
fb_short <- fb %>%
  select(Block, Plot, Group, Density, Date, PHAU, Cheno, Typha, 
         BOMA, DISP, EUMA, SYCI, LEFA, SCAC, BICE, BIFR, EUOC, MUAS, SCAM, RUMA,
         RUST, Unk_Bulrush, SARU, Tamarisk)#removing notes column and things we don't want calculated (unknowns and total cover and measurements)
fb_end <- fb_short %>% #make it so it is only the final date
  filter(Date == "2022-09-16") 

fb_wide <- fb_end %>% #pivot the table
  tidyr::pivot_longer(
    cols = 6:24, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  )

#groups natives and invasives
fb_split<- fb_wide %>%
  dplyr::mutate(Status = 
                dplyr::if_else(
                  SPP %in%
                  c("PHAU", "Typha", "RUST", "Tamarisk"), 
                  "Invasive", "Native"))

fb_split<-fb_split %>% #and now calculate the total cover for invasive and native for each block and plot
  dplyr::group_by(Block, Plot, Status) %>%
  dplyr::summarise(PC = sum(Percent_Cover, na.rm = TRUE))

plot_names <- c("Perennial forbs (high)", "Perennial forbs (high)", 
                         "Perennial forbs (low)", "Perennial forbs (low)",
                         "Rushes (high)", "Rushes (high)", "Rushes (low)", "Rushes (low)",
                         "Grasses (high)", "Grasses (high)", "Grasses (low)", "Grasses (low)",
                         "Bulrushes (high)", "Bulrushes (high)", "Bulrushes (low)", "Bulrushes (low)",
                         "Annual forbs (high)", "Annual forbs (high)", "Annual forbs (low)", "Annual forbs (low)",
                         "Control", "Control")
fb_split$plot_names <- rep(plot_names, 6)

#Graph of all the plots and total cover - split invasive and native
fb_split$plot_names <- factor(fb_split$plot_names, 
                             levels = c("Annual forbs (high)", "Annual forbs (low)", "Perennial forbs (high)", "Perennial forbs (low)",
                                        "Bulrushes (high)", "Bulrushes (low)","Rushes (high)", "Rushes (low)","Grasses (high)", "Grasses (low)", 
                                        "Control"))

fb_split %>% 
  ggplot(aes(x = plot_names, y = PC, color = Status)) +
  stat_summary(aes(group = Status),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = Status, width = 0),
               fun.data = mean_se, geom = "errorbar", size = 1) +
  labs(x = "Functional Group", y = "Cover (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  ggtitle("Great Salt Lake") +
  ylim(0, 100)

ggsave("fb_invasive_native.jpeg",
       device = jpeg)

####Graph what came up####
#Graph of the species that I know came up and what they did over time
##This needs to be compared to above calculations in trt x control - I made the intial graph quickly and not sure if these species are totally correct
my_species <- c("BOMA", 'DISP', 'SYCI', 'SCAC', 'BICE', 'RUMA')
#EUMA, EUOC, SCAM, or MUAS because not enough sightings
#need to combine BICE and BIFR

fb_short2 <- fb_short %>%
  select(c('Block', 'Plot', 'Group', 'Density', 'Date', my_species))


fb_wide2 <- fb_short2 %>% #pivot the table
  tidyr::pivot_longer(
    cols = 6:length(fb_short2), 
    names_to = "Species",
    values_to = "Percent_Cover"
  )

#display.brewer.all()

#add a columm with the name I want as the facet labels
fb_wide2$facet_label <- as.character(fb_wide2$Density)
fb_wide2$facet_label[fb_wide2$facet_label == "H"] <- "High Density"
fb_wide2$facet_label[fb_wide2$facet_label == "L"] <- "Low Density"
fb_wide2$facet_label[fb_wide2$facet_label == "C"] <- "Control"

#put the facet labels in the order that you want
fb_wide2$facet_label <- factor(fb_wide2$facet_label, levels = c("Control", "Low Density", "High Density"))

#Change species names to what I want the label to show
fb_wide2$Species[fb_wide2$Species == "BICE"] <- "Beggartick (Annual forb)"
fb_wide2$Species[fb_wide2$Species == "BOMA"] <- "Alkali bulrush (Bulrush)"
fb_wide2$Species[fb_wide2$Species == "DISP"] <- "Saltgrass (Grass)"
fb_wide2$Species[fb_wide2$Species == "RUMA"] <- "Golden dock (Annual forb)"
fb_wide2$Species[fb_wide2$Species == "SCAC"] <- "Hardstem bulrush (Bulrush)"
fb_wide2$Species[fb_wide2$Species == "SYCI"] <- "Rayless aster (Annual forb)"

fb_wide2 %>%
  ggplot(aes(x = Date, y = Percent_Cover, color = Species)) +
  stat_summary(aes(group = Species),
               fun = mean, geom = "point", size = 1) +
  stat_summary(aes(group = Species),
               fun = mean, geom = "line", size = 1.5) +
  facet_wrap(~facet_label) +
  #make colors the same for both
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")) +
  theme_bw() +
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Cover (%)", color = "Species")

ggsave(filename = "Fb_plot.jpeg", 
       width = 8,
       height = 5,
       device = "jpeg")

 ####UL####
####General graph####
#trt x control to see how they look compared to the control 
ul_1 <- ul %>% filter(Date == "2022-09-16") 

#things I planted that came up: BOMA, BICE, RUMA, SCAC, SCAM, DISP, SYCI, EUOC, 
ul_1 %>% ggplot(aes(x = Plot, y = EUOC
                    )) +
  geom_point()

#Things that look like more came up where planted: BOMA(maybe, not really), BICE (maybe), DISP
#Things that didn't come up in control: SCAC (but came up lots elsewhere), same as SCAC
#PHAU is slightly higher in 3 and 5 and same as control elsewhere
#RUMA was higher not in my seeded plots, and EUOC

#look to see if the measurements seem to be related to anything
ul_1%>% ggplot(aes(x = Plot, y = Measurement.1)) +
  geom_point()

#Measurement 1 highest in 4H
#Measurement 2 highest in C
#Measurement 3 highest in 1L, 2H, 4H

#### invasive vs native####
#Pivot table so columns are response but species are rows
ul_short <- ul%>%
  select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
         Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
         EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE)#removing notes column and things we don't want calculated (unknowns and total cover and measurements)
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
                      c("PHAU", "TYPHA", "RUST", "Tamarisk", "ALPR", "CYDA", "BY", 
                        "BASC", "LASE"), 
                    "Invasive", "Native"))

ul_split<-ul_split %>% #and now calculate the total cover for invasive and native for each block and plot
  dplyr::group_by(Block, Plot, Status) %>%
  dplyr::summarise(PC = sum(Percent_Cover, na.rm = TRUE))

plot_names <- c("Perennial forbs (high)", "Perennial forbs (high)", 
                "Perennial forbs (low)", "Perennial forbs (low)",
                "Rushes (high)", "Rushes (high)", "Rushes (low)", "Rushes (low)",
                "Grasses (high)", "Grasses (high)", "Grasses (low)", "Grasses (low)",
                "Bulrushes (high)", "Bulrushes (high)", "Bulrushes (low)", "Bulrushes (low)",
                "Annual forbs (high)", "Annual forbs (high)", "Annual forbs (low)", "Annual forbs (low)",
                "Control", "Control")
ul_split$plot_names <- rep(plot_names, 6)

#Graph of all the plots and total cover - split invasive and native
ul_split$plot_names <- factor(ul_split$plot_names, 
                              levels = c("Annual forbs (high)", "Annual forbs (low)", "Perennial forbs (high)", "Perennial forbs (low)",
                                         "Bulrushes (high)", "Bulrushes (low)","Rushes (high)", "Rushes (low)","Grasses (high)", "Grasses (low)", 
                                         "Control"))

ul_split %>% 
  ggplot(aes(x = plot_names, y = PC, color = Status)) +
  stat_summary(aes(group = Status),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = Status, width = 0),
               fun.data = mean_se, geom = "errorbar", size = 1) +
  labs(x = "Functional Group", y = "Cover (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  ggtitle("Utah Lake") +
  ylim(0, 100)

ggsave("ul_invasive_native.jpeg",
       device = jpeg)

#Graphs so I know what came up and what they did over time
##This needs to be compared to above calculations in trt x control - I made the intial graph quickly and not sure if these species are totally correct
my_species <- c("BOMA", 'DISP', 'BICE', 'SCAM', 'EUOC')
#tried to only include things that may have possibly come up at my sites
ul_short <- ul %>%
  select(c('Block', 'Plot', 'Group', 'Density', 'Date', my_species))


ul_wide <- ul_short %>% #pivot the table
  tidyr::pivot_longer(
    cols = 6:length(ul_short), 
    names_to = "Species",
    values_to = "Percent_Cover"
  )

#groups natives and invasives
ul_split<- ul_wide %>%
  dplyr::mutate(Status = 
                  dplyr::if_else(
                    SPP %in%
                      c("PHAU", "Typha", "Rust", "Tamarisk"), 
                    "Invasive", "Native"))

#RUMA, SYCI, and SCAC were highest in the control
#add a columm with the name I want as the facet labels
ul_wide$facet_label <- as.character(ul_wide$Density)
ul_wide$facet_label[ul_wide$facet_label == "H"] <- "High Density"
ul_wide$facet_label[ul_wide$facet_label == "L"] <- "Low Density"
ul_wide$facet_label[ul_wide$facet_label == "C"] <- "Control"


#put the facet labels in the order that you want
ul_wide$facet_label <- factor(ul_wide$facet_label, levels = c("Control", "Low Density", "High Density"))

#change the species names to what I want the legend to show
ul_wide$Species[ul_wide$Species == "BICE"] <- "Beggartick (Annual forb)"
ul_wide$Species[ul_wide$Species == "BOMA"] <- "Alkali bulrush (Bulrush)"
ul_wide$Species[ul_wide$Species == "DISP"] <- "Saltgrass (Grass)"
ul_wide$Species[ul_wide$Species == "EUOC"] <- "Western goldentop (Perennial forb)"
ul_wide$Species[ul_wide$Species == "SCAM"] <- "Three-square bulrush (Bulrush)"


ul_wide %>%
  ggplot(aes(x = Date, y = Percent_Cover, color = Species)) +
  stat_summary(aes(group = Species),
               fun = mean, geom = "point", size = 1) +
  stat_summary(aes(group = Species),
               fun = mean, geom = "line", size = 1.5) +
  facet_wrap(~facet_label) +
  #make colors the same for both
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#FFD92F", "#BEBADA", "#80B1D3")) +
  theme_bw() +
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Cover (%)", color = "Species") 

ggsave(filename = "Ul_plot.jpeg", 
       width = 8,
       height = 5,
       device = "jpeg")
