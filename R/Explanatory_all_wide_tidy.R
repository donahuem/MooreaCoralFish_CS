# BUILDING EXPLANATORY
#### Callie Stephenson
########Created: ?
#######Edited: 6/29/24
#Woohoo

library(dplyr)

setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")

#metadata
meta <- read.csv("data/coral_metadata.csv")

#explanatory data
all_nut <- read.csv("data/All_Nutrients_Processed.csv")
#nut <- read.csv("data/March_nutrients_processed.csv")
#nut_wide <- read.csv("data/Nutrient_data_wide.csv")
explanatory_seasonal <- read.csv("data/Explanatory_variables.csv")
turb <- read_csv("https://raw.githubusercontent.com/dbarnas/Community_Functional_Diversity/main/Data/Biogeochem/August2021/Turb_NC.csv")

#clean turb
turb1 <- turb %>% 
  filter(startsWith(CowTagID, "V")& CowTagID != "VSEEP")

turb1 <- turb1 %>%
  dplyr::select(c(CowTagID,
                  N_percent,
                  C_N, 
                  del15N))
#use C:N 
#C:N goes down with SGD
#can also use N percent
#%N goes up with SGD

#Mean Low Tide Value PCA 1 Axis
PCA_Pulse_variables_wide <- read.csv("data/PCA_Pulse_variables_wide.csv")
#explan <- read.csv("data/Explanatory_variables_wide.csv")

all_nut_long <- pivot_longer(
  data = all_nut,
  cols = c(Minimum_Salinity:Low_Tide_Mean_Ammonia_umolL)) 
names(all_nut_long)[names(all_nut_long) == "name"] <- "Parameter"

join <- explanatory_seasonal[,c(2:7)]
join <- unique(join)

explanatory_all <- left_join(join,all_nut_long, by = join_by(CowTagID))
explanatory_all_wide <- left_join(join, all_nut,by = join_by(CowTagID))
explanatory_all_wide <- left_join(explanatory_all_wide, turb1,by = join_by(CowTagID))
explanatory_all_wide <- left_join(explanatory_all_wide, PCA_Pulse_variables_wide[,c("CowTagID", "pulse_pc1")],by = join_by(CowTagID))


write.csv(explanatory_all_wide,"data/explanatory_all_wide.csv", row.names = FALSE)
