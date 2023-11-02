###########
# Script for converting raw TLE data into usable files
# Callie Stephenson
# Created: 8/28/23
# Last Modified: 8/29/23

library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(here)


######## Add in Data ########
meta <- read.csv("data/PRU_Coral_Codes.csv")
TLE_Pru <- read.csv("data/TLE_Pru.csv")
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
  select(Vial_No_Only, Placement_Code, Max_L_AVG, Max_L_Basal, Area_AVG, Area_Basal) %>% 
  mutate(Species="PRU")
#### work through the area and TLE metrics as %∆ or log that which might help if need normality

T0_Pac <- read.csv("data/TLE_Pac_T0_raw.csv") %>% 
  select(!X) %>% 
  select(!X.1)
T1_Pac <- read.csv("data/TLE_Pac_T1_raw.csv") %>% 
  select(!X)
TLE_Pac1 <- left_join(T0_Pac,T1_Pac) %>% 
  drop_na()


TLE_Sum_Pac <- TLE_Pac %>% 
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
  select(Vial_No_Only, Placement_Code, Max_L_AVG, Max_L_Basal, Area_AVG, Area_Basal) %>% 
  mutate(Species="PAC")


TLE_all <- rbind(TLE_Sum_Pru,TLE_Sum_Pac)

#### Write as a .csv
file_path <- "data/TLE_summary.csv"
if (!file.exists(file_path)) {
  write.csv(TLE_all, file = file_path)
} else {
  # Do nothing
}
