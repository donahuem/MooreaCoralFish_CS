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
dry_wt <- read.csv(here("Data", "Wet_Dry_Weight.csv"))
#FCM <- read.csv("/Users/calliestephenson/Downloads/CS_FCM_Plate_3_1.csv")
#meta <- read.csv("/Users/calliestephenson/Downloads/FCM_Meta.csv")


meta <- meta %>%
  drop_na(SLURRY.LABEL) %>%
  mutate(Tube.Name.=sprintf("%02d-Well-%s%d", PLATE, ROW, COLUMN))

merged_FCM <- meta %>%
  left_join(FCM, by = "Tube.Name.")

merged_FCM[merged_FCM$PLACEMENT == "PRU V03 C" & merged_FCM$PLATE == 3, "GOOD.SAMPLE"] <- FALSE #May 9, 2024 CS found an error in this code where I had a sample I had re-run that was still listed as a 'good sample'

merged_FCM <- merged_FCM %>%
  filter(GOOD.SAMPLE)

FCM_data <- merged_FCM[,c(1,2,6,7,13,17,21)]


PAC <- read.csv("data/PAC_Coral_Codes.csv")%>% 
  dplyr::select(!X) %>% 
  dplyr::select(!X.1) %>% 
  rename("PLACEMENT" = "Placement_Code")
PRU <- read.csv("data/PRU_Coral_Codes.csv")%>% 
  rename("PLACEMENT" = "Placement_Code") %>% 
  dplyr::select(!X)

codes <- rbind(PAC[,c(2,3,4,7,8)],PRU[,c(2,3,4,7,8)])

FCM <- left_join(FCM_data, codes)
FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)

sa$PLACEMENT <- sa$Placement_Code
FCM <- merge(FCM, sa[, c('PLACEMENT', 'Surface_Area')], by = 'PLACEMENT') #make a joining variable
dry_wt$PLACEMENT <- dry_wt$Placement
FCM <- merge(FCM, dry_wt[c('PLACEMENT','Approximate.Dry.Weight.of.Whole.Sample')])

FCM$sym_FSC.Events <- as.numeric(FCM$sym_FSC.Events)
#GREAT! Now we have the number of cells in each sample
  
#upscale it to the full sample
#From the slurry I took 1 ml for FCM
#From that ml I took 28 ul from the original slurry volume
#This was then diluted to make a 150 uL sample that was run fast (60 uL/min) on the flow cytometer
#From this, we only used the last 120 ul (cut out 30 seconds at the beginning which is 30 uL)
#Assuming the 8 second mix appropriately homogenized everything, this means we used 120/150 of the sample (aka 80% of the sample)
#5/21/24 I THINK I FOUND THE ERROR
#I did a 10x dilution - I put 28 ul of sample and then added water to 280 ul, BUT, only 15ul was run. So it *was* a 10x dilution that I need to put in here.
#So, if we accept homogenization, it would actually be taking 15ul of the slurry volume, meaning what I thought was in 28 ul was actually in 15.
FCM$FSC.Events.per.ul <- FCM$sym_FSC.Events / (15*(120/150)) #15 ul of slurry, 120 seconds/ul count of the 150 seconds/ul total
FCM$FSC.Events.per.ul <- FCM$FSC.Events.per.ul * (1-0.0159) #
#Need to multiply by 50,000 (the number of ul in a 50 ml sample) to get the number of symbionts per slurry
FCM$FSC.Events.per.slurry <- FCM$FSC.Events.per.ul * FCM$ORIGINAL.SLURRY.VOLUME * 1000 #og slurry volume is in ml, convert to ul to ml with *1000


#Now, normalizing that to the amount of live surface area that I airbrushed per coral:
FCM$FSC.Events_per_cm_2 <- FCM$FSC.Events.per.slurry / FCM$Surface_Area 

#Now, normalizing to the dry weight of the coral sample
FCM$FSC.Events_per_g_dry_weight <- FCM$FSC.Events.per.slurry / FCM$Approximate.Dry.Weight.of.Whole.Sample

write_csv(FCM, here("Data","FCM_Tidy.csv"))

#FCM_Join <- FCM[, c("Placement_Code", "sym_FSC.Events")]
#FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)
