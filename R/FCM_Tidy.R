###########Code to merge FCM values with their Sample names and export to Git
###### CS
######## 08/28/23
#Woohoo

library(dplyr)
library(tidyr)
library(here)
library(readr)
setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")
FCM <- read.csv(here("Data","CS_FCM_Plate_3_1.csv"))
meta <-read.csv(here("Data","FCM_Meta.csv"))
#FCM <- read.csv("/Users/calliestephenson/Downloads/CS_FCM_Plate_3_1.csv")
#meta <- read.csv("/Users/calliestephenson/Downloads/FCM_Meta.csv")


meta <- meta %>%
  drop_na(SLURRY.LABEL) %>%
  mutate(Tube.Name.=sprintf("%02d-Well-%s%d", PLATE, ROW, COLUMN))

merged_FCM <- meta %>%
  left_join(FCM, by = "Tube.Name.") %>%
  filter(GOOD.SAMPLE)

FCM_data <- merged_FCM[,c(1,2,6,7,13,17,21)]

PAC <- read.csv("data/PAC_Coral_Codes.csv")
PRU <- read.csv("data/PRU_Coral_Codes.csv")

PAC <- PAC[,c(2,3,4,8,9)] %>% 
  rename("PLACEMENT" = "Placement_Code")
PRU <- PRU[,c(2,3,4,7,8)] %>% 
  rename("PLACEMENT" = "Placement_Code")

codes <- rbind(PAC,PRU)

FCM <- left_join(FCM_data, codes)
FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)
  
write_csv(FCM, here("data", "FCM_data.csv"))

