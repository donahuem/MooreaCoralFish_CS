###### Data Exploration for Callie's Moorea Data
##################################
# LOAD LIBRARIES
###############################
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)
library(tidytext)
library(AICcmodavg)
library(kableExtra)
library(PNWColors)
library(lme4)
###############################
# READ IN DATA
###############################

setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")
meta <- read_csv("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/Data/Coral_Data/PRU_Coral_Codes.csv")
#meta <- read_csv(here("Data", "Coral_Data","PAC_Coral_Codes.csv"))
BW <- read_csv("https://raw.githubusercontent.com/donahuem/MooreaCoralFish_CS/main/Data/Coral_Data/Skeletal_Dry_Weight_Calc_Pru.csv") 
#BW <- read_csv(here("Data","Coral_Data","Skeletal_Dry_Weight_Calc_Pru"))
TLE_Pru <- read_csv("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/Data/Coral_Data/TLE_Pru.csv")

#meta <- meta %>% mutate(Cage_Uncaged = recode(Cage_Uncaged, "A" = "1", "B" = "2", "C" = "3"))
meta$Cage_Uncaged <- factor(meta$Cage_Uncaged)

TLE_sum <- TLE_Pru %>%
  mutate(
    Max_L_1 = T1_Feret_1 - T0_Feret_1,
    Max_L_2 = T1_Feret_2 - T0_Feret_2,
    Max_L_AVG = ((Max_L_1 + Max_L_2) /2),
    Max_L_Basal = T1_Feret_3 - T0_Feret_3,
    Area_1 = T1_Area_1 - T0_Area_1,
    Area_2 = T1_Area_2 - T0_Area_2,
    Area_AVG = ((Area_1 + Area_2) /2),
    Area_Basal = T1_Area_3 - T0_Area_3
  ) %>%
  select(Vial_No_Only, Placement_Code, Max_L_AVG, Max_L_Basal, Area_AVG, Area_Basal)
#### work through the area and TLE metrics as %∆ or log that which might help if need normality

TLE_sum <- merge(TLE_sum, meta[,c("Placement_Code","Cage_Uncaged")], by = "Placement_Code", all.x = TRUE)
BW <-merge(BW, meta[,c("Placement_Code","Cage_Uncaged")], by = "Placement_Code", all.x = TRUE)


###########
#Look for outliers
############
dotplot <- par(mfrow=c(2,2))
dotchart(BW$Change_Over_Area,main="∆BW", groups = BW$Cage_Uncaged)
plot(0,0,type="n",axes=F)
dotchart(TLE_sum$Area_AVG, main="Area",groups=TLE_sum$Cage_Uncaged)
dotchart(TLE_sum$Area_Basal, main="Basal Area",groups=TLE_sum$Cage_Uncaged)

######
#Remove Bad Data point
####
subset_BW <- BW[BW$Placement_Code != "PRU V13 A", ]
dotplot <- par(mfrow=c(2,2))
dotchart(subset_BW$Change_Over_Area,main="∆BW", groups = subset_BW$Cage_Uncaged)
plot(0,0,type="n",axes=F)
dotchart(TLE_sum$Area_AVG, main="Area",groups=TLE_sum$Cage_Uncaged)
dotchart(TLE_sum$Area_Basal, main="Basal Area",groups=TLE_sum$Cage_Uncaged)
#########
#Transform Data for Outliers
#########

#Is this appropriate?
