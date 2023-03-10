#import data
fb <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_fb.csv")
ul <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/Cleaned-Data/clean_ul.csv")

library(dplyr)

View(fb)
View(ul)

####fix FB####
glimpse(fb)

#remake block and make plot a factor
names(fb)[1] <- "Block"
fb$Plot <- as.factor(fb$Plot)

#change the C in block so it is not rank deficient, then make a factor
fb$Group[fb$Group == "C"] <- 10
fb$Group <- as.factor(fb$Group)

#double check density and group and then make a factor
unique(fb$Group)
fb$Group <- as.factor(fb$Group)

unique(fb$Density)
fb$Density <- as.factor(fb$Density)

#fix the date
library(lubridate)
fb$Date <- lubridate::mdy(fb$Date)

#change cover values to the 5s and make numeric
unique(fb$Total.Cover)
fb$Total.Cover[fb$Total.Cover == "<1"] <- "0.5"
fb$Total.Cover[fb$Total.Cover == 1.00] <- 5.00
fb$Total.Cover[fb$Total.Cover == 10.00] <- 15.00
fb$Total.Cover[fb$Total.Cover == 20.00] <- 25.00
fb$Total.Cover[fb$Total.Cover == 30.00] <- 35.00
fb$Total.Cover[fb$Total.Cover == 40.00] <- 45.00
fb$Total.Cover[fb$Total.Cover == 50.00] <- 55.00
fb$Total.Cover[fb$Total.Cover == 60.00] <- 65.00
fb$Total.Cover[fb$Total.Cover == 70.00] <- 75.00
fb$Total.Cover <- as.double(fb$Total.Cover)
fb$Total.Cover <- fb$Total.Cover/100

#now do it for all the other columns
unique(fb$PHAU)
fb$PHAU[fb$PHAU == "<1"] <- "0.5"
fb$PHAU[fb$PHAU == 1.00] <- 5.00
fb$PHAU[fb$PHAU == 10.00] <- 15.00
fb$PHAU <- as.double(fb$PHAU)
fb$PHAU <- fb$PHAU/100

unique(fb$Cheno)
fb$Cheno[fb$Cheno == "<1"] <- "0.5"
fb$Cheno[fb$Cheno == 1.00] <- 5.00
fb$Cheno[fb$Cheno == 10.00] <- 15.00
fb$Cheno[fb$Cheno == 20.00] <- 25.00
fb$Cheno[fb$Cheno == 30.00] <- 35.00
fb$Cheno <- as.double(fb$Cheno)
fb$Cheno <- fb$Cheno/100

unique(fb$Typha)
fb$Typha[fb$Typha == "<1"] <- "0.5"
fb$Typha[fb$Typha == 1.00] <- 5.00
fb$Typha[fb$Typha == 10.00] <- 15.00
fb$Typha <- as.double(fb$Typha)
fb$Typha <- fb$Typha/100

unique(fb$BOMA)
fb$BOMA[fb$BOMA == "<1"] <- "0.5"
fb$BOMA[fb$BOMA == 1.00] <- 5.00
fb$BOMA[fb$BOMA == 10.00] <- 15.00
fb$BOMA[fb$BOMA == 20.00] <- 25.00
fb$BOMA <- as.double(fb$BOMA)
fb$BOMA <- fb$BOMA/100

unique(fb$DISP)
fb$DISP[fb$DISP == "<1"] <- "0.5"
fb$DISP[fb$DISP == 1.00] <- 5.00
fb$DISP[fb$DISP == 10.00] <- 15.00
fb$DISP[fb$DISP == 20.00] <- 25.00
fb$DISP[fb$DISP == 30.00] <- 35.00
fb$DISP[fb$DISP == 40.00] <- 45.00
fb$DISP[fb$DISP == 50.00] <- 55.00
fb$DISP[fb$DISP == 60.00] <- 65.00
fb$DISP <- as.double(fb$DISP)
fb$DISP <- fb$DISP/100

unique(fb$EUMA)
fb$EUMA[fb$EUMA == "<1"] <- "0.5"
fb$EUMA[fb$EUMA == 1.00] <- 5.00
fb$EUMA <- as.double(fb$EUMA)
fb$EUMA <- fb$EUMA/100

unique(fb$SYCI)
fb$SYCI[fb$SYCI == "<1"] <- "0.5"
fb$SYCI[fb$SYCI == 1.00] <- 5.00
fb$SYCI[fb$SYCI == 10.00] <- 15.00
fb$SYCI <- as.double(fb$SYCI)
fb$SYCI <- fb$SYCI/100

unique(fb$LEFA)
fb$LEFA[fb$LEFA == "<1"] <- "0.5"
fb$LEFA[fb$LEFA == 1.00] <- 5.00
fb$LEFA[fb$LEFA == 10.00] <- 15.00
fb$LEFA[fb$LEFA == 20.00] <- 25.00
fb$LEFA <- as.double(fb$LEFA)
fb$LEFA <- fb$LEFA/100

unique(fb$SCAC)
fb$SCAC[fb$SCAC == "<1"] <- "0.5"
fb$SCAC[fb$SCAC == 1.00] <- 5.00
fb$SCAC <- as.double(fb$SCAC)
fb$SCAC <- fb$SCAC/100

unique(fb$BICE)
fb$BICE[fb$BICE == "<1"] <- "0.5"
fb$BICE[fb$BICE == 1.00] <- 5.00
fb$BICE[fb$BICE == 10.00] <- 15.00
fb$BICE <- as.double(fb$BICE)
fb$BICE <- fb$BICE/100

unique(fb$BIFR)
fb$BIFR[fb$BIFR == "<1"] <- "0.5"
fb$BIFR[fb$BIFR == 1.00] <- 5.00
fb$BIFR <- as.double(fb$BIFR)
fb$BIFR <- fb$BIFR/100

unique(fb$EUOC)
fb$EUOC[fb$EUOC == 1.00] <- 5.00
fb$EUOC <- as.double(fb$EUOC)
fb$EUOC <- fb$EUOC/100

unique(fb$MUAS)
fb$MUAS[fb$MUAS == 1.00] <- 5.00
fb$MUAS[fb$MUAS == 10.00] <- 15.00
fb$MUAS <- as.double(fb$MUAS)
fb$MUAS <- fb$MUAS/100

unique(fb$SCAM)
fb$SCAM[fb$SCAM == 1.00] <- 5.00
fb$SCAM <- as.double(fb$SCAM)
fb$SCAM <- fb$SCAM/100

unique(fb$RUMA)
fb$RUMA[fb$RUMA == "<1"] <- "0.5"
fb$RUMA[fb$RUMA == 1.00] <- 5.00
fb$RUMA <- as.double(fb$RUMA)
fb$RUMA <- fb$RUMA/100

unique(fb$RUST)
fb$RUST[fb$RUST == "<1"] <- "0.5"
fb$RUST[fb$RUST == 1.00] <- 5.00
fb$RUST[fb$RUST == 10.00] <- 15.00
fb$RUST <- as.double(fb$RUST)
fb$RUST <- fb$RUST/100

unique(fb$Unk_Forb)
fb$Unk_Forb[fb$Unk_Forb == "<1"] <- "0.5"
fb$Unk_Forb[fb$Unk_Forb == 1.00] <- 5.00
fb$Unk_Forb <- as.double(fb$Unk_Forb)
fb$Unk_Forb <- fb$Unk_Forb/100

unique(fb$Unk_Grass)
fb$Unk_Grass[fb$Unk_Grass == "<1"] <- "0.5"
fb$Unk_Grass[fb$Unk_Grass == 1.00] <- 5.00
fb$Unk_Grass <- as.double(fb$Unk_Grass)
fb$Unk_Grass <- fb$Unk_Grass/100

unique(fb$Unk_Rush)
fb$Unk_Rush[fb$Unk_Rush == "<1"] <- "0.5"
fb$Unk_Rush[fb$Unk_Rush == 1.00] <- 5.00
fb$Unk_Rush <- as.double(fb$Unk_Rush)
names(fb)[25] <- "Unk_Bulrush"
fb$Unk_Bulrush <- fb$Unk_Bulrush/100

unique(fb$SARU)
fb$SARU[fb$SARU == "<1"] <- "0.5"
fb$SARU[fb$SARU == 1.00] <- 5.00
fb$SARU <- as.double(fb$SARU)
fb$SARU <- fb$SARU/100

unique(fb$Tamarisk)
fb$Tamarisk[fb$Tamarisk == "<1"] <- "0.5"
fb$Tamarisk[fb$Tamarisk == 1.00] <- 5.00
fb$Tamarisk <- as.double(fb$Tamarisk)
fb$Tamarisk <- fb$Tamarisk/100

glimpse(fb)

#check measurements to make sure they make sense
min(fb$Measurement.1)
max(fb$Measurement.1)

min(fb$Measurement.2)
max(fb$Measurement.2)

min(fb$Measurement.3)
max(fb$Measurement.3)

#Add a new section for invasives and natives
df <- fb %>%
  select("PHAU", "Typha", "RUST", "Tamarisk") %>%
  mutate(Invasive = rowSums(.,na.rm = T))
df1 <- fb %>%
  select("Cheno", "BOMA", "DISP", "EUMA", "SYCI", "LEFA", "SCAC",
         "BICE", "BIFR", "EUOC", "MUAS", "SCAM", "RUMA", "Unk_Bulrush", 
         "SARU") %>%
  mutate(Native = rowSums(.,na.rm = T))

fb$Invasive.Cover <- df$Invasive
fb$Native.Cover <- df1$Native

max(fb$Invasive.Cover, na.rm = T)
max(fb$Native.Cover, na.rm = T)

#unique(fb$Invasive.Cover)
#get rid of the NAs and 0s
#min(fb$Invasive.Cover)
fb$Invasive.Cover[is.na(fb$Invasive.Cover)] <- 0.0025
fb$Invasive.Cover[fb$Invasive.Cover==0] <- 0.0025

#unique(fb$Native.Cover)
#min(fb$Native.Cover, na.rm = TRUE)
fb$Native.Cover[is.na(fb$Native.Cover)] <- 0.0025
fb$Native.Cover[fb$Native.Cover==0] <- 0.0025

####fix UL####
glimpse(ul)

#make block and plot factors, fix the C group in Block
ul$Plot <- as.factor(ul$Plot)
ul$Group[ul$Group == "C"] <- 10
ul$Group <- as.factor(ul$Group)

#check density and group and make factor 
unique(ul$Group)
ul$Group <- as.factor(ul$Group)

unique(ul$Density)
ul$Density <- as.factor(ul$Density)

#fix the date
ul$Date <- lubridate::mdy(ul$Date)

#fix all numbers and make everything numeric
unique(ul$Total.Cover)
ul$Total.Cover[ul$Total.Cover == "<1"] <- "0.5"
ul$Total.Cover[ul$Total.Cover == 1.00] <- 5.00
ul$Total.Cover[ul$Total.Cover == 10.00] <- 15.00
ul$Total.Cover[ul$Total.Cover == 20.00] <- 25.00
ul$Total.Cover[ul$Total.Cover == 30.00] <- 35.00
ul$Total.Cover[ul$Total.Cover == 40.00] <- 45.00
ul$Total.Cover[ul$Total.Cover == 50.00] <- 55.00
ul$Total.Cover[ul$Total.Cover == 60.00] <- 65.00
ul$Total.Cover[ul$Total.Cover == 70.00] <- 75.00
ul$Total.Cover[ul$Total.Cover == 80.00] <- 85.00
ul$Total.Cover[ul$Total.Cover == 90.00] <- 95.00
ul$Total.Cover <- as.double(ul$Total.Cover)
ul$Total.Cover <- ul$Total.Cover/100

names(ul)[7] <- "Unk_Forb" #fix the name
unique(ul$Unk_Forb)
ul$Unk_Forb[ul$Unk_Forb == "<1"] <- "0.5"
ul$Unk_Forb[ul$Unk_Forb == 1.00] <- 5.00
ul$Unk_Forb <- as.double(ul$Unk_Forb)
ul$Unk_Forb <- ul$Unk_Forb/100

unique(ul$PHAU)
ul$PHAU[ul$PHAU == "<1"] <- "0.5"
ul$PHAU[ul$PHAU == 1.00] <- 5.00
ul$PHAU[ul$PHAU == 10.00] <- 15.00
ul$PHAU <- as.double(ul$PHAU)
ul$PHAU <- ul$PHAU/100

unique(ul$Unk_Sedge)
ul$Unk_Sedge[ul$Unk_Sedge == "<1"] <- "0.5"
ul$Unk_Sedge[ul$Unk_Sedge == 1.00] <- 5.00
ul$Unk_Sedge[ul$Unk_Sedge == 10.00] <- 15.00
ul$Unk_Sedge[ul$Unk_Sedge == 20.00] <- 25.00
ul$Unk_Sedge[ul$Unk_Sedge == 30.00] <- 35.00
ul$Unk_Sedge[ul$Unk_Sedge == 40.00] <- 45.00
ul$Unk_Sedge <- as.double(ul$Unk_Sedge)
ul$Unk_Sedge <- ul$Unk_Sedge/100

unique(ul$BOMA)
ul$BOMA[ul$BOMA == "<1"] <- "0.5"
ul$BOMA[ul$BOMA == 1.00] <- 5.00
ul$BOMA[ul$BOMA == 10.00] <- 15.00
ul$BOMA[ul$BOMA == 20.00] <- 25.00
ul$BOMA[ul$BOMA == 30.00] <- 35.00
ul$BOMA[ul$BOMA == 40.00] <- 45.00
ul$BOMA[ul$BOMA == 50.00] <- 55.00
ul$BOMA <- as.double(ul$BOMA)
ul$BOMA <- ul$BOMA/100

unique(ul$BICE)
ul$BICE[ul$BICE == 1.00] <- 5.00
ul$BICE[ul$BICE == 10.00] <- 15.00
ul$BICE[ul$BICE == 20.00] <- 25.00
ul$BICE[ul$BICE == 30.00] <- 35.00
ul$BICE <- as.double(ul$BICE)
ul$BICE <- ul$BICE/100

unique(ul$CYER)
ul$CYER[ul$CYER == 1.00] <- 5.00
ul$CYER[ul$CYER == 10.00] <- 15.00
ul$CYER[ul$CYER == 20.00] <- 25.00
ul$CYER[ul$CYER == 30.00] <- 35.00
ul$CYER[ul$CYER == 40.00] <- 45.00
ul$CYER[ul$CYER == 60.00] <- 65.00
ul$CYER[ul$CYER == 70.00] <- 75.00
ul$CYER <- as.double(ul$CYER)
ul$CYER <- ul$CYER/100

unique(ul$RUMA)
ul$RUMA[ul$RUMA == "<1"] <- "0.5"
ul$RUMA[ul$RUMA == 1.00] <- 5.00
ul$RUMA[ul$RUMA == 10.00] <- 15.00
ul$RUMA[ul$RUMA == 20.00] <- 25.00
ul$RUMA[ul$RUMA == 30.00] <- 35.00
ul$RUMA[ul$RUMA == 40.00] <- 45.00
ul$RUMA[ul$RUMA == 50.00] <- 55.00
ul$RUMA[ul$RUMA == 60.00] <- 65.00
ul$RUMA[ul$RUMA == 70.00] <- 75.00
ul$RUMA[ul$RUMA == 80.00] <- 85.00
ul$RUMA <- as.double(ul$RUMA)
ul$RUMA <- ul$RUMA/100

unique(ul$BASC)
ul$BASC[ul$BASC == 1.00] <- 5.00
ul$BASC[ul$BASC == 10.00] <- 15.00
ul$BASC <- as.double(ul$BASC)
ul$BASC <- ul$BASC/100

unique(ul$LASE)
ul$LASE[ul$LASE == 1.00] <- 5.00
ul$LASE[ul$LASE == 10.00] <- 15.00
ul$LASE <- as.double(ul$LASE)
ul$LASE <- ul$LASE/100

unique(ul$Cheno)
ul$Cheno[ul$Cheno == "<1"] <- "0.5"
ul$Cheno[ul$Cheno == 1.00] <- 5.00
ul$Cheno[ul$Cheno == 10.00] <- 15.00
ul$Cheno[ul$Cheno == 20.00] <- 25.00
ul$Cheno[ul$Cheno == 30.00] <- 35.00
ul$Cheno[ul$Cheno == 40.00] <- 45.00
ul$Cheno[ul$Cheno == 50.00] <- 55.00
ul$Cheno <- as.double(ul$Cheno)
ul$Cheno <- ul$Cheno/100

unique(ul$SCAC)
ul$SCAC[ul$SCAC == "<1"] <- "0.5"
ul$SCAC[ul$SCAC == 1.00] <- 5.00
ul$SCAC <- as.double(ul$SCAC)
ul$SCAC <- ul$SCAC/100

unique(ul$SCPU)
ul$SCPU[ul$SCPU == "<1"] <- "0.5"
ul$SCPU[ul$SCPU == 1.00] <- 5.00
ul$SCPU <- as.double(ul$SCPU)
ul$SCPU <- ul$SCPU/100

unique(ul$SCAM)
ul$SCAM[ul$SCAM == "<1"] <- "0.5"
ul$SCAM[ul$SCAM == 1.00] <- 5.00
ul$SCAM[ul$SCAM == 10.00] <- 15.00
ul$SCAM <- as.double(ul$SCAM)
ul$SCAM <- ul$SCAM/100

unique(ul$DISP)
ul$DISP[ul$DISP == "<1"] <- "0.5"
ul$DISP[ul$DISP == 1.00] <- 5.00
ul$DISP[ul$DISP == 10.00] <- 15.00
ul$DISP <- as.double(ul$DISP)
ul$DISP <- ul$DISP/100

unique(ul$RACY)
ul$RACY[ul$RACY == "<1"] <- "0.5"
ul$RACY[ul$RACY == 1.00] <- 5.00
ul$RACY <- as.double(ul$RACY)
ul$RACY <- ul$RACY/100

unique(ul$ASIN)
ul$ASIN[ul$ASIN == "<1"] <- "0.5"
ul$ASIN[ul$ASIN == 1.00] <- 5.00
ul$ASIN <- as.double(ul$ASIN)
ul$ASIN <- ul$ASIN/100

unique(ul$Unk_Grass)
ul$Unk_Grass[ul$Unk_Grass == "<1"] <- "0.5"
ul$Unk_Grass[ul$Unk_Grass == 1.00] <- 5.00
ul$Unk_Grass[ul$Unk_Grass == 10.00] <- 15.00
ul$Unk_Grass[ul$Unk_Grass == 20.00] <- 25.00
ul$Unk_Grass <- as.double(ul$Unk_Grass)
ul$Unk_Grass <- ul$Unk_Grass/100

unique(ul$ALPR)
ul$ALPR[ul$ALPR == "<1"] <- "0.5"
ul$ALPR[ul$ALPR == 1.00] <- 5.00
ul$ALPR[ul$ALPR == 10.00] <- 15.00
ul$ALPR[ul$ALPR == 20.00] <- 25.00
ul$ALPR <- as.double(ul$ALPR)
ul$ALPR <- ul$ALPR/100

unique(ul$CYDA)
ul$CYDA[ul$CYDA == 1.00] <- 5.00
ul$CYDA[ul$CYDA == 10.00] <- 15.00
ul$CYDA <- as.double(ul$CYDA)
ul$CYDA <- ul$CYDA/100

unique(ul$POFR)
ul$POFR[ul$POFR == "<1"] <- "0.5"
ul$POFR[ul$POFR == 1.00] <- 5.00
ul$POFR <- as.double(ul$POFR)
ul$POFR <- ul$POFR/100

unique(ul$SAAM)
ul$SAAM[ul$SAAM == "<1"] <- "0.5"
ul$SAAM[ul$SAAM == 1.00] <- 5.00
ul$SAAM <- as.double(ul$SAAM)
ul$SAAM <- ul$SAAM/100

unique(ul$Unk_Bulrush)
ul$Unk_Bulrush[ul$Unk_Bulrush == "<1"] <- "0.5"
ul$Unk_Bulrush[ul$Unk_Bulrush == 1.00] <- 5.00
ul$Unk_Bulrush <- as.double(ul$Unk_Bulrush)
ul$Unk_Bulrush <- ul$Unk_Bulrush/100

unique(ul$BY)
ul$BY[ul$BY == 1.00] <- 5.00
ul$BY <- as.double(ul$BY)
ul$BY <- ul$BY/100

unique(ul$SYCI)
ul$SYCI[ul$SYCI == 1.00] <- 5.00
ul$SYCI[ul$SYCI == 10.00] <- 15.00
ul$SYCI <- as.double(ul$SYCI)
ul$SYCI <- ul$SYCI/100

unique(ul$EUOC)
ul$EUOC[ul$EUOC == "<1"] <- "0.5"
ul$EUOC[ul$EUOC == 1.00] <- 5.00
ul$EUOC[ul$EUOC == 10.00] <- 15.00
ul$EUOC <- as.double(ul$EUOC)
ul$EUOC <- ul$EUOC/100

unique(ul$TYPHA)
ul$TYPHA[ul$TYPHA == "<1"] <- "0.5"
ul$TYPHA[ul$TYPHA == 1.00] <- 5.00
ul$TYPHA <- as.double(ul$TYPHA)
ul$TYPHA <- ul$TYPHA/100

unique(ul$Tamarisk)
ul$Tamarisk[ul$Tamarisk == "<1"] <- "0.5"
ul$Tamarisk[ul$Tamarisk == 1.00] <- 5.00
ul$Tamarisk[ul$Tamarisk == 10.00] <- 15.00
ul$Tamarisk <- as.double(ul$Tamarisk)
ul$Tamarisk <- ul$Tamarisk/100

unique(ul$POPE)
ul$POPE[ul$POPE == "<1"] <- "0.5"
ul$POPE[ul$POPE == 1.00] <- 5.00
ul$POPE <- as.double(ul$POPE)
ul$POPE <- ul$POPE/100

glimpse(ul)

#check the measurements
min(ul$Measurement.1)
max(ul$Measurement.1)

min(ul$Measurement.2)
max(ul$Measurement.2)

min(ul$Measurement.3, na.rm = TRUE)
max(ul$Measurement.3, na.rm = TRUE)


#Add a new section for invasives and natives
df <- ul %>%
  select("PHAU", "TYPHA", "Tamarisk", "ALPR", "CYDA", "BY", 
         "BASC", "LASE") %>%
  mutate(Invasive = rowSums(.,na.rm = T))
df1 <- ul %>%
  select("Unk_Bulrush", "BOMA", "BICE", 'CYER', 'RUMA', 'Cheno', 'SCAC', 'SCAM',
         'SCPU', 'DISP', 'RACY', 'ASIN', 'SYCI', 'EUOC', 'POPE', 'POFR', 'SAAM') %>%
  mutate(Native = rowSums(.,na.rm = T))

ul$Invasive.Cover <- df$Invasive
ul$Native.Cover <- df1$Native

max(ul$Invasive.Cover, na.rm = TRUE)
max(ul$Native.Cover, na.rm = TRUE)

unique(ul$Invasive.Cover)
#get rid of the NAs
#min(ul$Invasive.Cover, na.rm = TRUE)
ul$Invasive.Cover[is.na(ul$Invasive.Cover)] <- 0.0025
ul$Invasive.Cover[ul$Invasive.Cover==0] <- 0.0025

#unique(ul$Native.Cover)
#min(ul$Native.Cover, na.rm = TRUE)
ul$Native.Cover[is.na(ul$Native.Cover)] <- 0.0025
ul$Native.Cover[ul$Native.Cover==0] <- 0.0025



####Save files####
save(ul, fb, file = "clean_dfs.RData")

