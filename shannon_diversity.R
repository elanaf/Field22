#Import####
load("clean_dfs.RData")
library(vegan)
library(tidyverse)
library(patchwork)

#Farmington Bay ####

View(fb)

#only want the final cover
final.dat <- fb %>% 
  filter(Date == "2022-09-16")

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Plot', 'Block'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#make all percentages
final.dat2 <- mutate_if(final.dat2, is.numeric, ~.*100)

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- select(final.dat3, 6:26)
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#change the names of the groups
final.dat$Group <- as.character(final.dat$Group)
final.dat$Group[final.dat$Group == "1"] <- "Perennial Forb"
final.dat$Group[final.dat$Group == "2"] <- "Rush"
final.dat$Group[final.dat$Group == "3"] <- "Grass"
final.dat$Group[final.dat$Group == "4"] <- "Bulrush"
final.dat$Group[final.dat$Group == "5"] <- "Annual Forb"
final.dat$Group <- as.factor(final.dat$Group)

##Plot ####
final.dat %>% 
  filter(Group != 10) %>% 
  ggplot(aes(x = Group, y = shannon, fill = Density)) +
  geom_boxplot()+
  ylab("Shannon Diversity Index") +
  ggtitle("Farmington Bay")

# Utah Lake ####
View(ul)

#only want the final cover
final.dat <- ul %>% 
  filter(Date == "2022-09-16")

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Plot', 'Block'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#make all percentages
final.dat2 <- mutate_if(final.dat2, is.numeric, ~.*100)

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- select(final.dat3, 6:33)
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#change the names of the groups
final.dat$Group <- as.character(final.dat$Group)
final.dat$Group[final.dat$Group == "1"] <- "Perennial Forb"
final.dat$Group[final.dat$Group == "2"] <- "Rush"
final.dat$Group[final.dat$Group == "3"] <- "Grass"
final.dat$Group[final.dat$Group == "4"] <- "Bulrush"
final.dat$Group[final.dat$Group == "5"] <- "Annual Forb"
final.dat$Group <- as.factor(final.dat$Group)

##Plot ####
final.dat %>% 
  filter(Group != 10) %>% 
  ggplot(aes(x = Group, y = shannon, fill = Density)) +
  geom_boxplot()+
  ylab("Shannon Diversity Index") +
  ggtitle("Utah Lake")

# Plot together ####
a <- final.dat %>% 
  filter(Group != 10) %>% 
  ggplot(aes(x = Group, y = shannon, fill = Density)) +
  geom_boxplot(show.legend = FALSE)+
  ylab("Shannon Diversity Index") +
  ggtitle("Farmington Bay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = ggtext::element_markdown(size = 10))

b <- final.dat %>% 
  filter(Group != 10) %>% 
  ggplot(aes(x = Group, y = shannon, fill = Density)) +
  geom_boxplot()+
  ylab("") +
  ggtitle("Utah Lake") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = ggtext::element_markdown(size = 10),
        legend.key.size = unit(.35, 'cm'),
        legend.title = ggtext::element_markdown(size = 10))

a + b

ggsave("diversity_indices.jpeg")
