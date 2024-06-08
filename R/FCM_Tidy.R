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

####adding a new row for the one good sample on 6/7/24
# Assuming 'FCM' is your DataFrame and the columns are already named as shown
new_row <- data.frame(
  PLACEMENT = "PAC 056 2",
  SLURRY.LABEL = "Second_Run_PAC_056",                           # Adjust the values as necessary
  ORIGINAL.SLURRY.VOLUME = 25,
  AMOUNT.OF.SAMPLE.USED..μL. = 30,
  sym_FSC.Events = 15155,
  sym_FSC.Mean.Chla.A = NA,
  sym_FSC.Events.μL.V. = NA,
  Pin_Number = NA,
  Cage_Uncaged = NA,
  Genotype = NA,
  Species = "Pocillopora_acuta",
  Surface_Area = 79.96546,
  Approximate.Dry.Weight.of.Whole.Sample = 0.356500
)

# Add the new row to the DataFrame
FCM <- rbind(FCM, new_row)

  
#upscale it to the full sample
#From the slurry I took 0.984 ml for FCM, and added 0.016 ml of paraformaldehyde
dilution_factor <- 1 / 0.984
#From that ml I took 28 ul into a 280 ul sample to make a 10x dilution
#From this,  only the last 120 ul were used to calculate the number of events
#Assuming the 8 second mix appropriately homogenized everything, this means we used 12 ul of sample
FCM$FSC.Events.per.ul <- (FCM$sym_FSC.Events / (12) ) * dilution_factor #15 ul of slurry, 120 seconds/ul count of the 150 seconds/ul total
#Need to multiply by the amount of slurry adn convert the units (the number of ul in a 25/50 ml sample) to get the number of symbionts per slurry
FCM$FSC.Events.per.slurry <- FCM$FSC.Events.per.ul * FCM$ORIGINAL.SLURRY.VOLUME * 1000 #og slurry volume is in ml, convert to ul to ml with *1000


#Now, normalizing that to the amount of live surface area that I airbrushed per coral:
FCM$FSC.Events_per_cm_2 <- FCM$FSC.Events.per.slurry / FCM$Surface_Area 

#Now, normalizing to the dry weight of the coral sample
FCM$FSC.Events_per_g_dry_weight <- FCM$FSC.Events.per.slurry / FCM$Approximate.Dry.Weight.of.Whole.Sample

write_csv(FCM, here("Data","FCM_Tidy.csv"))

#FCM_Join <- FCM[, c("Placement_Code", "sym_FSC.Events")]
#FCM$Cage_Uncaged <- as.factor(FCM$Cage_Uncaged)
