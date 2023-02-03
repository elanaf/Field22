#import data
fb <- read.csv("clean_fb.csv")
ul <- read.csv("clean_ul.csv")

library(dplyr)

View(fb)
View(ul)

####fix FB####
glimpse(fb)

#make block and plot a factor
fb$Block...FB <- as.factor(fb$Block...FB)
names(fb)[1] <- "Block"
fb$Plot <- as.factor(fb$Plot)

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
fb$Total.Cover <- as.numeric(fb$Total.Cover)

#now do it for all the other columns
unique(fb$PHAU)
fb$PHAU[fb$PHAU == "<1"] <- "0.5"
fb$PHAU[fb$PHAU == 1.00] <- 5.00
fb$PHAU[fb$PHAU == 10.00] <- 15.00
fb$PHAU <- as.numeric(fb$PHAU)

unique(fb$Cheno)
fb$Cheno[fb$Cheno == "<1"] <- "0.5"
fb$Cheno[fb$Cheno == 1.00] <- 5.00
fb$Cheno[fb$Cheno == 10.00] <- 15.00
fb$Cheno[fb$Cheno == 20.00] <- 25.00
fb$Cheno[fb$Cheno == 30.00] <- 35.00
fb$Cheno <- as.numeric(fb$Cheno)

unique(fb$Typha)
fb$Typha[fb$Typha == "<1"] <- "0.5"
fb$Typha[fb$Typha == 1.00] <- 5.00
fb$Typha[fb$Typha == 10.00] <- 15.00
fb$Typha <- as.numeric(fb$Typha)

unique(fb$BOMA)
fb$BOMA[fb$BOMA == "<1"] <- "0.5"
fb$BOMA[fb$BOMA == 1.00] <- 5.00
fb$BOMA[fb$BOMA == 10.00] <- 15.00
fb$BOMA[fb$BOMA == 20.00] <- 25.00
fb$BOMA <- as.numeric(fb$BOMA)

unique(fb$DISP)
fb$DISP[fb$DISP == "<1"] <- "0.5"
fb$DISP[fb$DISP == 1.00] <- 5.00
fb$DISP[fb$DISP == 10.00] <- 15.00
fb$DISP[fb$DISP == 20.00] <- 25.00
fb$DISP[fb$DISP == 30.00] <- 35.00
fb$DISP[fb$DISP == 40.00] <- 45.00
fb$DISP[fb$DISP == 50.00] <- 55.00
fb$DISP[fb$DISP == 60.00] <- 65.00
fb$DISP <- as.numeric(fb$DISP)

unique(fb$EUMA)
fb$EUMA[fb$EUMA == "<1"] <- "0.5"
fb$EUMA[fb$EUMA == 1.00] <- 5.00
fb$EUMA <- as.numeric(fb$EUMA)

unique(fb$SYCI)
fb$SYCI[fb$SYCI == "<1"] <- "0.5"
fb$SYCI[fb$SYCI == 1.00] <- 5.00
fb$SYCI[fb$SYCI == 10.00] <- 15.00
fb$SYCI <- as.numeric(fb$SYCI)

unique(fb$LEFA)
fb$LEFA[fb$LEFA == "<1"] <- "0.5"
fb$LEFA[fb$LEFA == 1.00] <- 5.00
fb$LEFA[fb$LEFA == 10.00] <- 15.00
fb$LEFA[fb$LEFA == 20.00] <- 25.00
fb$LEFA <- as.numeric(fb$LEFA)

unique(fb$SCAC)
fb$SCAC[fb$SCAC == "<1"] <- "0.5"
fb$SCAC[fb$SCAC == 1.00] <- 5.00
fb$SCAC <- as.numeric(fb$SCAC)

unique(fb$BICE)
fb$BICE[fb$BICE == "<1"] <- "0.5"
fb$BICE[fb$BICE == 1.00] <- 5.00
fb$BICE[fb$BICE == 10.00] <- 15.00
fb$BICE <- as.numeric(fb$BICE)

unique(fb$BIFR)
fb$BIFR[fb$BIFR == "<1"] <- "0.5"
fb$BIFR[fb$BIFR == 1.00] <- 5.00
fb$BIFR <- as.numeric(fb$BIFR)

unique(fb$EUOC)
fb$EUOC[fb$EUOC == 1.00] <- 5.00

unique(fb$MUAS)
fb$MUAS[fb$MUAS == 1.00] <- 5.00
fb$MUAS[fb$MUAS == 10.00] <- 15.00

unique(fb$SCAM)
fb$SCAM[fb$SCAM == 1.00] <- 5.00

unique(fb$RUMA)
fb$RUMA[fb$RUMA == "<1"] <- "0.5"
fb$RUMA[fb$RUMA == 1.00] <- 5.00
fb$RUMA <- as.numeric(fb$RUMA)

unique(fb$RUST)
fb$RUST[fb$RUST == "<1"] <- "0.5"
fb$RUST[fb$RUST == 1.00] <- 5.00
fb$RUST[fb$RUST == 10.00] <- 15.00
fb$RUST <- as.numeric(fb$RUST)

unique(fb$Unk_Forb)
fb$Unk_Forb[fb$Unk_Forb == "<1"] <- "0.5"
fb$Unk_Forb[fb$Unk_Forb == 1.00] <- 5.00
fb$Unk_Forb <- as.numeric(fb$Unk_Forb)

unique(fb$Unk_Grass)
fb$Unk_Grass[fb$Unk_Grass == "<1"] <- "0.5"
fb$Unk_Grass[fb$Unk_Grass == 1.00] <- 5.00
fb$Unk_Grass <- as.numeric(fb$Unk_Grass)

unique(fb$Unk_Rush)
fb$Unk_Rush[fb$Unk_Rush == "<1"] <- "0.5"
fb$Unk_Rush[fb$Unk_Rush == 1.00] <- 5.00
fb$Unk_Rush <- as.numeric(fb$Unk_Rush)
names(fb)[25] <- "Unk_Bulrush"

unique(fb$SARU)
fb$SARU[fb$SARU == "<1"] <- "0.5"
fb$SARU[fb$SARU == 1.00] <- 5.00
fb$SARU <- as.numeric(fb$SARU)

unique(fb$Tamarisk)
fb$Tamarisk[fb$Tamarisk == "<1"] <- "0.5"
fb$Tamarisk[fb$Tamarisk == 1.00] <- 5.00
fb$Tamarisk <- as.numeric(fb$Tamarisk)

glimpse(fb)

#check measurements to make sure they make sense
min(fb$Measurement.1)
max(fb$Measurement.1)

min(fb$Measurement.2)
max(fb$Measurement.2)

min(fb$Measurement.3)
max(fb$Measurement.3)

####fix UL####
glimpse(ul)

#make block and plot factors
ul$Block <- as.factor(ul$Block)
ul$Plot <- as.factor(ul$Plot)

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
ul$Total.Cover <- as.numeric(ul$Total.Cover)

names(ul)[7] <- "Unk_Forb" #fix the name
unique(ul$Unk_Forb)
ul$Unk_Forb[ul$Unk_Forb == "<1"] <- "0.5"
ul$Unk_Forb[ul$Unk_Forb == 1.00] <- 5.00
ul$Unk_Forb <- as.numeric(ul$Unk_Forb)

unique(ul$PHAU)
ul$PHAU[ul$PHAU == "<1"] <- "0.5"
ul$PHAU[ul$PHAU == 1.00] <- 5.00
ul$PHAU[ul$PHAU == 10.00] <- 15.00
ul$PHAU <- as.numeric(ul$PHAU)

unique(ul$Unk_Sedge)
ul$Unk_Sedge[ul$Unk_Sedge == "<1"] <- "0.5"
ul$Unk_Sedge[ul$Unk_Sedge == 1.00] <- 5.00
ul$Unk_Sedge[ul$Unk_Sedge == 10.00] <- 15.00
ul$Unk_Sedge[ul$Unk_Sedge == 20.00] <- 25.00
ul$Unk_Sedge[ul$Unk_Sedge == 30.00] <- 35.00
ul$Unk_Sedge[ul$Unk_Sedge == 40.00] <- 45.00
ul$Unk_Sedge <- as.numeric(ul$Unk_Sedge)

unique(ul$BOMA)
ul$BOMA[ul$BOMA == "<1"] <- "0.5"
ul$BOMA[ul$BOMA == 1.00] <- 5.00
ul$BOMA[ul$BOMA == 10.00] <- 15.00
ul$BOMA[ul$BOMA == 20.00] <- 25.00
ul$BOMA[ul$BOMA == 30.00] <- 35.00
ul$BOMA[ul$BOMA == 40.00] <- 45.00
ul$BOMA[ul$BOMA == 50.00] <- 55.00
ul$BOMA <- as.numeric(ul$BOMA)

unique(ul$BICE)
ul$BICE[ul$BICE == 1.00] <- 5.00
ul$BICE[ul$BICE == 10.00] <- 15.00
ul$BICE[ul$BICE == 20.00] <- 25.00
ul$BICE[ul$BICE == 30.00] <- 35.00

unique(ul$CYER)
ul$CYER[ul$CYER == 1.00] <- 5.00
ul$CYER[ul$CYER == 10.00] <- 15.00
ul$CYER[ul$CYER == 20.00] <- 25.00
ul$CYER[ul$CYER == 30.00] <- 35.00
ul$CYER[ul$CYER == 40.00] <- 45.00
ul$CYER[ul$CYER == 60.00] <- 65.00
ul$CYER[ul$CYER == 70.00] <- 75.00

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
ul$RUMA <- as.numeric(ul$RUMA)

unique(ul$BASC)
ul$BASC[ul$BASC == 1.00] <- 5.00
ul$BASC[ul$BASC == 10.00] <- 15.00

unique(ul$LASE)
ul$LASE[ul$LASE == 1.00] <- 5.00
ul$LASE[ul$LASE == 10.00] <- 15.00

unique(ul$Cheno)
ul$Cheno[ul$Cheno == "<1"] <- "0.5"
ul$Cheno[ul$Cheno == 1.00] <- 5.00
ul$Cheno[ul$Cheno == 10.00] <- 15.00
ul$Cheno[ul$Cheno == 20.00] <- 25.00
ul$Cheno[ul$Cheno == 30.00] <- 35.00
ul$Cheno[ul$Cheno == 40.00] <- 45.00
ul$Cheno[ul$Cheno == 50.00] <- 55.00
ul$Cheno <- as.numeric(ul$Cheno)

unique(ul$SCAC)
ul$SCAC[ul$SCAC == "<1"] <- "0.5"
ul$SCAC[ul$SCAC == 1.00] <- 5.00
ul$SCAC <- as.numeric(ul$SCAC)

unique(ul$SCPU)
ul$SCPU[ul$SCPU == "<1"] <- "0.5"
ul$SCPU[ul$SCPU == 1.00] <- 5.00
ul$SCPU <- as.numeric(ul$SCPU)

unique(ul$SCAM)
ul$SCAM[ul$SCAM == "<1"] <- "0.5"
ul$SCAM[ul$SCAM == 1.00] <- 5.00
ul$SCAM[ul$SCAM == 10.00] <- 15.00
ul$SCAM <- as.numeric(ul$SCAM)

unique(ul$DISP)
ul$DISP[ul$DISP == "<1"] <- "0.5"
ul$DISP[ul$DISP == 1.00] <- 5.00
ul$DISP[ul$DISP == 10.00] <- 15.00
ul$DISP <- as.numeric(ul$DISP)

unique(ul$RACY)
ul$RACY[ul$RACY == "<1"] <- "0.5"
ul$RACY[ul$RACY == 1.00] <- 5.00
ul$RACY <- as.numeric(ul$RACY)

unique(ul$ASIN)
ul$ASIN[ul$ASIN == "<1"] <- "0.5"
ul$ASIN[ul$ASIN == 1.00] <- 5.00
ul$ASIN <- as.numeric(ul$ASIN)

unique(ul$Unk_Grass)
ul$Unk_Grass[ul$Unk_Grass == "<1"] <- "0.5"
ul$Unk_Grass[ul$Unk_Grass == 1.00] <- 5.00
ul$Unk_Grass[ul$Unk_Grass == 10.00] <- 15.00
ul$Unk_Grass[ul$Unk_Grass == 20.00] <- 25.00
ul$Unk_Grass <- as.numeric(ul$Unk_Grass)

unique(ul$ALPR)
ul$ALPR[ul$ALPR == "<1"] <- "0.5"
ul$ALPR[ul$ALPR == 1.00] <- 5.00
ul$ALPR[ul$ALPR == 10.00] <- 15.00
ul$ALPR[ul$ALPR == 20.00] <- 25.00
ul$ALPR <- as.numeric(ul$ALPR)

unique(ul$CYDA)
ul$CYDA[ul$CYDA == 1.00] <- 5.00
ul$CYDA[ul$CYDA == 10.00] <- 15.00

unique(ul$POFR)
ul$POFR[ul$POFR == "<1"] <- "0.5"
ul$POFR[ul$POFR == 1.00] <- 5.00
ul$POFR <- as.numeric(ul$POFR)

unique(ul$SAAM)
ul$SAAM[ul$SAAM == "<1"] <- "0.5"
ul$SAAM[ul$SAAM == 1.00] <- 5.00
ul$SAAM <- as.numeric(ul$SAAM)

unique(ul$Unk_Bulrush)
ul$Unk_Bulrush[ul$Unk_Bulrush == "<1"] <- "0.5"
ul$Unk_Bulrush[ul$Unk_Bulrush == 1.00] <- 5.00
ul$Unk_Bulrush <- as.numeric(ul$Unk_Bulrush)

unique(ul$BY)
ul$BY[ul$BY == 1.00] <- 5.00

unique(ul$SYCI)
ul$SYCI[ul$SYCI == 1.00] <- 5.00
ul$SYCI[ul$SYCI == 10.00] <- 15.00

unique(ul$EUOC)
ul$EUOC[ul$EUOC == "<1"] <- "0.5"
ul$EUOC[ul$EUOC == 1.00] <- 5.00
ul$EUOC[ul$EUOC == 10.00] <- 15.00
ul$EUOC <- as.numeric(ul$EUOC)

unique(ul$TYPHA)
ul$TYPHA[ul$TYPHA == "<1"] <- "0.5"
ul$TYPHA[ul$TYPHA == 1.00] <- 5.00
ul$TYPHA <- as.numeric(ul$TYPHA)

unique(ul$Tamarisk)
ul$Tamarisk[ul$Tamarisk == "<1"] <- "0.5"
ul$Tamarisk[ul$Tamarisk == 1.00] <- 5.00
ul$Tamarisk[ul$Tamarisk == 10.00] <- 15.00
ul$Tamarisk <- as.numeric(ul$Tamarisk)

unique(ul$POPE)
ul$POPE[ul$POPE == "<1"] <- "0.5"
ul$POPE[ul$POPE == 1.00] <- 5.00
ul$POPE <- as.numeric(ul$POPE)

glimpse(ul)

#check the measurements
min(ul$Measurement.1)
max(ul$Measurement.1)

min(ul$Measurement.2)
max(ul$Measurement.2)

min(ul$Measurement.3, na.rm = TRUE)
max(ul$Measurement.3, na.rm = TRUE)

####Save files####
save(ul, fb, file = "clean_dfs.RData")

