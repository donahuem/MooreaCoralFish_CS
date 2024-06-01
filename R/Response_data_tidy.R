library(dplyr)

#Response data
sa <- read.csv("data/Surface_Area.csv")
BW <- read.csv("data/BW_tidy.csv") 
BW <- subset(BW, select=-c(1,4,5)) #remove some erroneous columns
TLE <- read.csv("data/TLE_summary.csv")
FCM <- read.csv("data/FCM_tidy.csv") %>% 
  rename(Placement_Code = PLACEMENT)
SI <- read.csv("data/SI_Tidy.csv")

SI_join <- SI[,c("HS","δ15N", "δ13C", "Placement_Code","Pin_Number", "Cage_Uncaged", "Genotype", "Species", "CowTagID", "Δ15N", "Δ13C", "C_N_ratio","δ15N_T1_T0", "δ13C_T1_T0", "Δ15N_T1T0","Δ13C_T1T0")] %>%
  pivot_wider(names_from = HS, values_from = c("δ15N", "δ13C", "C_N_ratio","δ15N_T1_T0", "δ13C_T1_T0", "Δ15N_T1T0","Δ13C_T1T0"), names_sep = "_")

#metadata
meta <- read.csv("data/coral_metadata.csv")

#make a master response variable data sheet
response_data <- left_join(meta[,c(2:4,7,8)], BW[,c(2,4,5,6)], by = join_by(Placement_Code)) %>% 
  #select(!X) %>% #check that this doesn't need to be here
  left_join(FCM[,c(1,16,17)], by = join_by(Placement_Code)) %>% 
  left_join(TLE[,c(3:7)], by = join_by(Placement_Code)) %>% 
  left_join(SI_join[,c(1,4,6:18)], by = join_by(Placement_Code, Genotype))
response_data$CowTagID <- paste0("V",response_data$Pin_Number)

PRU_response_data <- response_data %>% 
  filter(Species == "Porites rus")

PRU_response_data_caged <- PRU_response_data %>% 
  filter(Cage_Uncaged == "C")

PAC_response_data <- response_data %>% 
  filter(Species == "Pocillopora acuta")

PAC_response_data_caged <- PAC_response_data %>% 
  filter(Cage_Uncaged == "C")

write.csv(response_data,"data/response_data.csv", row.names = FALSE)

