library(tidyverse)
rm(list=ls())

#First we input coral data
Buoyant_weight_Pac <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/PAC_Buoyant_Weight.csv")
library(tidyverse)
#Now we clean it up
No_NA_Buoyant_weight_Pac <- Buoyant_weight_Pac%>%
  drop_na(T1_Weight) #Remove NA in PAC V01 A and B

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
Coral_Dict <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/PAC_Coral_Codes.csv")
Coral_Dict=Coral_Dict[-which(is.na(Coral_Dict$Pin_Number)),]

#And import flow data for clod cards
Clod <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/Clod_Cards.csv")

#Rename the files something easier - also makes the code a little easier to copy for PRU
bio=BioGC.Info_no_Seep
bw=No_NA_Buoyant_weight_Pac
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
bwChangeA=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="A"]})) #if you run this, you will only get 19 numbers because Pin 1 had the uncaged corals removed
bwChangeA=c(NA,bwChangeA) #Because it was just the first pin, we can do this. If we had losses elsewhere .... look at stacks overflow
bwChangeB=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="B"]})) 
bwChangeB=c(NA,bwChangeB) 
bwChangeC=unlist(lapply(c(1:20),function(x){bw$Percent_Change[bw$pin==x & bw$ABC=="C"]})) 
Gtype=unlist(lapply(c(1:20),function(x){Coral_Dict$Genotype[Coral_Dict$Pin_Number==x][1]}))
bw_sa_ChangeA=unlist(lapply(c(1:20),function(x){bw$Change_Over_Area[bw$pin==x & bw$ABC=="A"]})) #if you run this, you will only get 19 numbers because Pin 1 had the uncaged corals removed
bw_sa_ChangeA=c(NA,bw_sa_ChangeA) #Because it was just the first pin, we can do this. If we had losses elsewhere .... look at stacks overflow
bw_sa_ChangeB=unlist(lapply(c(1:20),function(x){bw$Change_Over_Area[bw$pin==x & bw$ABC=="B"]})) #if you run this, you will only get 19 numbers because Pin 1 had the uncaged corals removed
bw_sa_ChangeB=c(NA,bw_sa_ChangeB) #Because it was just the first pin, we can do this. If we had losses elsewhere .... look at stacks overflow
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
              bwChangeA=bwChangeA,
              bwChangeB=bwChangeB,
              bwChangeC=bwChangeC,
              bw_sa_ChangeA=bw_sa_ChangeA,
              bw_sa_ChangeB=bw_sa_ChangeB,
              bw_sa_ChangeC=bw_sa_ChangeC,
              Gtype=Gtype)

#makin some preliminary plotz about salinity
plot(df$sal_mean,df$bwChangeC)
plot(df$bwChangeC~df$sal_mean,pch=20,col=as.factor(df$Gtype))
plot(df$bw_sa_ChangeC~df$sal_mean,pch=20,col=as.factor(df$Gtype))
plot(df$bw_sa_ChangeB~df$sal_mean,pch=20,col=as.factor(df$Gtype))
plot(df$bw_sa_ChangeA~df$sal_mean,pch=20,col=as.factor(df$Gtype))

plot(df$sal_max,df$bwChangeC)
plot(df$bwChangeC~df$sal_max,pch=20,col=as.factor(df$Gtype))
plot(df$bw_sa_ChangeC~df$sal_max,pch=20,col=as.factor(df$Gtype))

plot(df$sal_min,df$bwChangeC)

#makin some preliminary plotz about Silicate
plot(df$bwChangeC~df$Silicate_mean,pch=20)
plot(df$bwChangeC~df$Silicate_max,pch=20)
plot(df$bwChangeC~df$Silicate_min,pch=20)

#makin some preliminary plotz about Phosphate
plot(df$bwChangeC~df$Phos_mean,pch=20)
plot(df$bwChangeC~df$Phos_max,pch=20)
plot(df$bwChangeC~df$Phos_min,pch=20)

#and checking for flow
plot(df$bw_sa_ChangeC,df$Flow, pch=20,col=as.factor(df$Gtype))
lm5=lm(df$bwChangeC~df$Flow)
summary(lm5)
abline(a=lm5,col='red')

#did you know r could make a map!
plot(df$lat~df$lon,asp=1)

hist(df$bwChangeC)

df2=df[,c(18,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

#I ran a quick GLM that would not be statistically robust to see what might be the biggest drivers to look into.
fit=glm(bwChangeC~.,data=df2,na.action=na.fail,family=gaussian())
library(MuMIn)
a=dredge(fit)


plot(df$bwChangeC~df$sal_mean,xlab='Salinity',pch=20)
lm1=lm(df$bwChangeC~df$sal_mean)
summary(lm1)
abline(a=lm1,col='red')



plot(df$bwChangeC~df$Phos_mean,pch=20)
lm2=lm(df$bwChangeC~df$Phos_mean)
summary(lm2)
abline(a=lm2,col='red')


plot(df$bwChangeC~df$NN_mean,pch=20)
lm3=lm(df$bwChangeC~df$NN_mean)
summary(lm3)
abline(a=lm3,col='red')


plot(df$bwChangeC~df$Silicate_mean,pch=20)
lm4=lm(df$bwChangeC~df$Silicate_mean)
summary(lm4)
abline(a=lm4,col='red')

