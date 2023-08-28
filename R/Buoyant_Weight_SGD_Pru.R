rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggpmisc)

#First we input coral data
Buoyant_weight_Pru <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/PRU_Buoyant_Weight.csv")

#Now we input biogeochem data
BioGC.Info_raw <-read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Allbiogeochemdata_QC.csv")
BioGC.Info <- BioGC.Info_raw %>%
  filter(!Date == "2021-08-06") #filter out Varari high wave event
BioGC.Info <- BioGC.Info %>%
  filter(!Location == "Cabral") #filter out Cabral data
BioGC.Info <- BioGC.Info %>%
  filter(!Silicate_umolL == "NA") #filter out NA
BioGC.Info <- BioGC.Info %>%
  filter(!CowTagID == "Varari_Well") #filter out Varari_Well
BioGC.Info_no_Seep <- BioGC.Info %>%
  filter(!CowTagID == "VSEEP") #filter out Seep
BioGC.Info_no_Seep$Varari_Pin <-as.numeric(gsub("V","",BioGC.Info_no_Seep$CowTagID)) #adds column 'Varari_Pin' that removes the V in CowTagID so that pin placements are numbers not characters

#Insert the coral dictionary
Coral_Dict <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/PRU_Coral_Codes.csv")
Coral_Dict=Coral_Dict[-which(is.na(Coral_Dict$Pin_Number)),]

#And import flow data for clod cards
Clod <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/Clod_Cards.csv")

#Import data for NC in Turbinaria
NC_Turb <- read.csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Nutrients/Turb_NC.csv")
NC_Turb <- NC_Turb %>%
  filter(!CowTagID=="CSEEP") #take away the seep at cabral
NC_Turb <- NC_Turb %>%
  filter(!CowTagID=="VSEEP")#take away the seep at varari
NC_Turb <- NC_Turb %>%
  filter(!CowTagID %in% sprintf("C%d", 1:20)) #take away rest of cabral
NC_Turb$Varari_Pin <-as.numeric(gsub("V","",NC_Turb$CowTagID))


#Rename the files something easier - also makes the code a little easier to copy for PRU
bio=BioGC.Info_no_Seep
bw=Buoyant_weight_Pru
bw$pin=as.numeric(substr(bw$Placement_Code,6,7))
bw$ABC=substr(bw$Placement_Code,9,9)


pin=c(1:20)
lat=unlist(lapply(
  c(1:20),
  function(x){bio$lat[bio$Varari_Pin==x][1]}))
long=unlist(lapply(c(1:20),function(x){bio$lon[bio$Varari_Pin==x][1]}))
sal_day_high=unlist(lapply(c(1:20),function(x){bio$Salinity[bio$Varari_Pin==x & bio$Tide=="High" & bio$Day_Night=="Day"]}))
sal_night_high=unlist(lapply(c(1:20),function(x){bio$Salinity[bio$Varari_Pin==x & bio$Tide=="High" & bio$Day_Night=="Night"]}))
sal_day_low=unlist(lapply(c(1:20),function(x){bio$Salinity[bio$Varari_Pin==x & bio$Tide=="Low" & bio$Day_Night=="Day"]}))
sal_night_low=unlist(lapply(c(1:20),function(x){bio$Salinity[bio$Varari_Pin==x & bio$Tide=="Low" & bio$Day_Night=="Night"]}))
sal_mean=unlist(lapply(c(1:20),function(x){mean(bio$Salinity[bio$Varari_Pin==x & is.na(bio$Salinity)==F])})) #[] subset the data for where salinity is = x and not NA
sal_max=unlist(lapply(c(1:20),function(x){max(bio$Salinity[bio$Varari_Pin==x & is.na(bio$Salinity)==F])}))
sal_min=unlist(lapply(c(1:20),function(x){min(bio$Salinity[bio$Varari_Pin==x & is.na(bio$Salinity)==F])}))
Phos_mean=unlist(lapply(c(1:20),function(x){mean(bio$Phosphate_umolL[bio$Varari_Pin==x & is.na(bio$Phosphate_umolL)==F])}))
Phos_max=unlist(lapply(c(1:20),function(x){max(bio$Phosphate_umolL[bio$Varari_Pin==x & is.na(bio$Phosphate_umolL)==F])}))
Phos_min=unlist(lapply(c(1:20),function(x){min(bio$Phosphate_umolL[bio$Varari_Pin==x & is.na(bio$Phosphate_umolL)==F])}))
Silicate_mean=unlist(lapply(c(1:20),function(x){mean(bio$Silicate_umolL[bio$Varari_Pin==x & is.na(bio$Silicate_umolL)==F])}))
Silicate_max=unlist(lapply(c(1:20),function(x){max(bio$Silicate_umolL[bio$Varari_Pin==x & is.na(bio$Silicate_umolL)==F])}))
Silicate_min=unlist(lapply(c(1:20),function(x){min(bio$Silicate_umolL[bio$Varari_Pin==x & is.na(bio$Silicate_umolL)==F])}))
NN_mean=unlist(lapply(c(1:20),function(x){mean(bio$NN_umolL[bio$Varari_Pin==x & is.na(bio$NN_umolL)==F])}))
NN_max=unlist(lapply(c(1:20),function(x){max(bio$NN_umolL[bio$Varari_Pin==x & is.na(bio$NN_umolL)==F])}))
NN_min=unlist(lapply(c(1:20),function(x){min(bio$NN_umolL[bio$Varari_Pin==x & is.na(bio$NN_umolL)==F])}))
Flow=unlist(lapply(c(1:20),function(x){Clod$X._loss[Clod$PIN==x]}))
NC_T=unlist(lapply(c(1:20),function(x){NC_Turb$C_N[NC_Turb$Varari_Pin==x]}))
bwChangeA=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="A"]}))
bwChangeB=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="B"]})) 
bwChangeC=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="C"]})) 
Gtype=unlist(lapply(c(1:20),function(x){Coral_Dict$Genotype[Coral_Dict$Pin_Number==x][1]}))
bw_sa_ChangeA=unlist(lapply(c(1:20),function(x){bw$Change_Over_Area[bw$pin==x & bw$ABC=="A"]})) 
bw_sa_ChangeB=unlist(lapply(c(1:20),function(x){bw$Change_Over_Area[bw$pin==x & bw$ABC=="B"]})) 
bw_sa_ChangeC=unlist(lapply(c(1:20),function(x){bw$Change_Over_Area[bw$pin==x & bw$ABC=="C"]}))


df=data.frame(pin=pin,
              lat=lat,
              lon=long,
              sal_mean=sal_mean,
              sal_min=sal_min,
              sal_max=sal_max,
              Phos_mean=Phos_mean,
              Phos_max=Phos_max,
              Phos_min=Phos_min,
              Silicate_mean=Silicate_mean,
              Silicate_max=Silicate_max,
              Silicate_min=Silicate_min,
              NN_mean=NN_mean,
              NN_max=NN_max,
              NN_min=NN_min,
              Flow=Flow,
              NC_T=NC_T,
              bwChangeA=bwChangeA,
              bwChangeB=bwChangeB,
              bwChangeC=bwChangeC,
              bw_sa_ChangeA=bw_sa_ChangeA,
              bw_sa_ChangeB=bw_sa_ChangeB,
              bw_sa_ChangeC=bw_sa_ChangeC,
              Gtype=Gtype)

#Now time for plots

#Plots about salinity
plot(df$sal_mean,df$bw_sa_ChangeC, pch=20)
plot(df$sal_min,df$bw_sa_ChangeC, pch=20)
plot(df$sal_max,df$bw_sa_ChangeC, pch=20)

#Plots about Silicate
plot(df$Silicate_mean,df$bw_sa_ChangeC, pch=20)
plot(df$Silicate_min,df$bw_sa_ChangeC, pch=20)
plot(df$Silicate_max,df$bw_sa_ChangeC, pch=20)

#Plots about NN
plot(df$NN_mean,df$bw_sa_ChangeC, pch=20)
plot(df$NN_min,df$bw_sa_ChangeC, pch=20)
plot(df$NN_max,df$bw_sa_ChangeC, pch=20)

#Plot about Flow
plot(df$Flow,df$bw_sa_ChangeC, pch=20)
lm_Flow=lm(df$bw_sa_ChangeC~df$NC_T)
summary(lm_Flow)
abline(a=lm_Flow,col='red')

#Plot for NC_Turb
plot(df$NC_T,df$bw_sa_ChangeC, pch=20)
lm_Turb=lm(df$bw_sa_ChangeC~df$NC_T)
summary(lm_Turb)
abline(a=lm_Turb,col='red')
