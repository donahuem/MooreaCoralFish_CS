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

FCM_data_raw <- merged_FCM

write_csv(merged_FCM, here("output", "FCM_data.csv"))

