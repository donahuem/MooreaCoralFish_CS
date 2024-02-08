###########Code to merge FCM values with their Sample names and export to Git
###### CS
########Created: 08/28/23
#######Edited: 2/8/24
#Woohoo

library(dplyr)
library(tidyr)
library(here)
library(readr)
#setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")
FCM <- read.csv(here("Data","CS_FCM_Plate_3_1.csv"))
meta <-read.csv(here("Data","FCM_Meta.csv"))
sa <- read.csv(here("Data","Surface_Area.csv"))
#FCM <- read.csv("/Users/calliestephenson/Downloads/CS_FCM_Plate_3_1.csv")
#meta <- read.csv("/Users/calliestephenson/Downloads/FCM_Meta.csv")


meta <- meta %>%
  drop_na(SLURRY.LABEL) %>%
  mutate(Tube.Name.=sprintf("%02d-Well-%s%d", PLATE, ROW, COLUMN))

merged_FCM <- meta %>%
  left_join(FCM, by = "Tube.Name.") %>%
  filter(GOOD.SAMPLE)

FCM_data <- merged_FCM[,c(1,2,6,7,13,17,21)]


PAC <- read.csv("data/PAC_Coral_Codes.csv")%>% 
  select(!X) %>% 
  select(!X.1) %>% 
  rename("PLACEMENT" = "Placement_Code")
PRU <- read.csv("data/PRU_Coral_Codes.csv")%>% 
  rename("PLACEMENT" = "Placement_Code") %>% 
  select(!X)

codes <- rbind(PAC[,c(2,3,4,7,8)],PRU[,c(2,3,4,7,8)])

FCM <- left_join(FCM_data, codes)
FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)

sa$PLACEMENT <- sa$Placement_Code
FCM <- merge(FCM, sa[, c('PLACEMENT', 'Surface_Area')], by = 'PLACEMENT') #make a joining variable


#GREAT! Now we have the number of cells in each sample
  
#upscale it to the full sample
#From the slurry I took 1 ml for FCM
#From that ml I took 28 ul from the original slurry volume
#This was then diluted to make a 150 uL sample that was run fast (60 uL/min) on the flow cytometer
#From this, we only used the last 120 ul (cut out 30 seconds at the beginning which is 30 uL)
#Assuming the 8 second mix appropriately homogenized everything, this means we used 120/150 of the sample (aka 80% of the sample)
#THUS, we officially only used (0.8)*(28) ul in the sample, which is 22.4 uL
FCM$sym_FSC.Events <- as.numeric(FCM$sym_FSC.Events)
#These events are per 22.4 uL, so if we divide by 22.4 we will get the actual number of events per ul
FCM$FSC.Events.per.ul <- FCM$sym_FSC.Events / 22.4
#Need to multiply by 50,000 (the number of ul in a 50 ml sample) to get the number of symbionts per slurry
FCM$FSC.Events.per.slurry <- FCM$FSC.Events.per.ul * FCM$ORIGINAL.SLURRY.VOLUME * 1000
#Now, normalizing that to the amount of live surface area that I airbrushed per coral:
FCM$FSC.Events_normalized <- FCM$FSC.Events.per.slurry / FCM$Surface_Area 

write_csv(FCM, here("Data","FCM_Tidy.csv"))



#FCM_Join <- FCM[, c("Placement_Code", "sym_FSC.Events")]
#FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)
