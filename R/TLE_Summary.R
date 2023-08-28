###########
# Script for converting raw TLE data into usable files
# Callie Stephenson
# Created: 8/16/23
# Last Modified: 8/16/23

library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(here)

######## Add in Data ########
setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")
meta <- read_csv("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/Data/Coral_Data/PRU_Coral_Codes.csv")
TLE_Pru <- read_csv("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/Data/Coral_Data/TLE_Pru.csv")
#TLE_Pac <- read_csv("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/Data/Coral_Data/TLE_Pac.csv")
#This sheet still needs to be created from the Google Drive
#The raw data is done, CS just needs to move it over


TLE_Sum_Pru <- TLE_Pru %>%
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

#### Write as a .csv
write_csv(TLE_Sum_Pru, here("Data", "Coral_Data", "TLE_summary_Pru.csv"))
