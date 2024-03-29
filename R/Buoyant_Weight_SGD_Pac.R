rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggpmisc)

#First we input coral data
Buoyant_weight_Pac <- read.csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Coral_Data/PAC_Buoyant_Weight.csv")

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
NC_T=unlist(lapply(c(1:20),function(x){NC_Turb$C_N[NC_Turb$Varari_Pin==x]}))
N_Percent_T=unlist(lapply(c(1:20),function(x){NC_Turb$N_percent[NC_Turb$Varari_Pin==x]}))
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
              NC_T=NC_T,
              N_Percent_T=N_Percent_T,
              bwChangeA=bwChangeA,
              bwChangeB=bwChangeB,
              bwChangeC=bwChangeC,
              bw_sa_ChangeA=bw_sa_ChangeA,
              bw_sa_ChangeB=bw_sa_ChangeB,
              bw_sa_ChangeC=bw_sa_ChangeC,
              Gtype=Gtype)


Salinity_Max_Plot <-ggplot(df, aes(
  y=bw_sa_ChangeC, 
  x=sal_max)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq(use_label(c("eq", "adj.R2")),label.x=0.9)+
  stat_poly_eq(use_label(c("p")),label.x=0.9,label.y = 0.88)+
  geom_point(aes(color=Gtype))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Maximum Salinity',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  theme(plot.title = element_text(hjust = 0.5))
Salinity_Max_Plot

Salinity_Max_Plot_All <- ggplot(df,aes(x=sal_max))+
  geom_point(aes(y=bw_sa_ChangeA, color='Uncaged'))+
  geom_point(aes(y=bw_sa_ChangeB, color='Partialy Caged'))+
  geom_point(aes(y=bw_sa_ChangeC, color='Caged'))+
  scale_color_manual(values=c('darkorchid4','cyan4','darkgoldenrod'))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Maximum Salinity',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeA), size=.5, colour="darkorchid4",se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeB), size=.5, colour='cyan4', se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeC), size=.5, colour='darkgoldenrod',se=FALSE, formula=y~x)+
  theme(plot.title = element_text(hjust = 0.5))
Salinity_Max_Plot_All

#BaseR Plots that I used to figure out what plots might work best.
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

#NC_Turb plot
plot(df$bw_sa_ChangeC~df$NC_T, pch=20)
lm6=lm(df$bw_sa_ChangeC~df$NC_T)
summary(lm6)
abline(a=lm6,col='red')

#N_Percent_Turb plot
plot(df$bw_sa_ChangeC~df$N_Percent_T, pch=20)
lm7=lm(df$bw_sa_ChangeC~df$N_Percent_T)
summary(lm7)
abline(a=lm7,col='red')
#inGGPLOT
N_Percent_Plot <-ggplot(df, aes(
  y=bw_sa_ChangeC, 
  x=N_Percent_T)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq(use_label(c("eq", "adj.R2")),label.x=0.9)+
  stat_poly_eq(use_label(c("p")),label.x=0.9,label.y = 0.88)+
  geom_point(aes(color=Gtype))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Percent N in Turbinaria',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  theme(plot.title = element_text(hjust = 0.5))
N_Percent_Plot

#N_Percent with all plotted
N_Percent_All_PAC <- ggplot(df,aes(x=N_Percent_T))+
  geom_point(aes(y=bw_sa_ChangeA, color='Uncaged'))+
  geom_point(aes(y=bw_sa_ChangeB, color='Partialy Caged'))+
  geom_point(aes(y=bw_sa_ChangeC, color='Caged'))+
  scale_color_manual(values=c('darkolivegreen4','darkorchid','darkgoldenrod1'))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Percent N in Turbinaria',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeA), size=.5, colour="darkolivegreen4",se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeB), size=.5, colour='darkorchid', se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeC), size=.5, colour='darkgoldenrod1',se=FALSE, formula=y~x)+
  theme(plot.title = element_text(hjust = 0.5))
N_Percent_All_PAC


#N_Percent with all plotted by Genotype
N_Percent_All_PAC <- ggplot(df,aes(x=N_Percent_T))+
  geom_point(aes(y=bw_sa_ChangeA, color='Uncaged'))+
  geom_point(aes(y=bw_sa_ChangeB, color='Partialy Caged'))+
  geom_point(aes(y=bw_sa_ChangeC, color='Caged'))+
  scale_color_manual(values=c('darkolivegreen4','darkorchid','darkgoldenrod1'))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Percent N in Turbinaria',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeA), size=.5, colour="darkolivegreen4",se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeB), size=.5, colour='darkorchid', se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeC), size=.5, colour='darkgoldenrod1',se=FALSE, formula=y~x)+
  theme(plot.title = element_text(hjust = 0.5))
N_Percent_All_PAC

#With Megan
N_Percent_AC_PAC <- ggplot(df,aes(x=N_Percent_T))+
  geom_point(aes(y=bw_sa_ChangeA, color='Uncaged'))+
  geom_point(aes(y=bw_sa_ChangeC, color='Caged'))+
  scale_color_manual(values=c('darkolivegreen4','darkgoldenrod1'))+
  labs(title='Coral Growth along Varari Submarine Groundwater Discharge Gradient',
       x='Percent N in Turbinaria',
       y='Change in Buoyant Weight over Surface Area',
       color='Genotype')+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeA), size=.5, colour="darkolivegreen4",se=FALSE, formula=y~x)+
  geom_smooth(method='lm', aes(y=bw_sa_ChangeC), size=.5, colour='darkgoldenrod1',se=FALSE, formula=y~x)+
  theme(plot.title = element_text(hjust = 0.5))
N_Percent_AC_PAC


#did you know r could make a map!
plot(df$lat~df$lon,asp=1)

hist(df$bwChangeC)

df2=df[,c(18,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

#I ran a quick GLM that would not be statistically robust to see what might be the biggest drivers to look into.
fit=glm(bwChangeC~.,data=df2,na.action=na.fail,family=gaussian())
library(MuMIn)
a=dredge(fit)


plot(df$bw_sa_ChangeC~df$sal_mean,xlab='Mean Salinity', ylab='Change in Buoyant Weight Divided by Surface Area',pch=20)
lm1=lm(df$bw_sa_ChangeC~df$sal_mean)
summary(lm1)
abline(a=lm1,col='red')


plot(df$bw_sa_ChangeC~df$sal_max,xlab='Mean Salinity', ylab='Change in Buoyant Weight Divided by Surface Area',pch=20)
lm1=lm(df$bw_sa_ChangeC~df$sal_max)
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

