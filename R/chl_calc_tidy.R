### I wanna make a sheet
library(dplyr)
library(here)
#Response data

#Let's remake a meta sheet:
meta <-read.csv(here("Data","FCM_Meta.csv")) %>% 
  rename(Placement_Code = PLACEMENT) %>% 
  rename(Slurry_ID = SLURRY.LABEL) %>% 
  rename(Slurry_Volume.ml = ORIGINAL.SLURRY.VOLUME) %>% 
  filter(GOOD.SAMPLE != "FALSE") %>% 
  distinct()


#others
response_data <- read.csv("data/response_data.csv")
Buoyant_weight_Pac_raw <- read.csv("data/PAC_Buoyant_Weight.csv") 
chl <- read.csv("data/Chl_concentrations.csv") 
meta <-read.csv(here("Data","FCM_Meta.csv")) %>% 
  rename(Placement_Code = PLACEMENT) %>% 
  rename(Slurry_ID = SLURRY.LABEL) %>% 
  rename(Slurry_Volume.ml = ORIGINAL.SLURRY.VOLUME) %>% 
  filter(GOOD.SAMPLE != "FALSE")

Buoyant_weight_Pac_raw <- Buoyant_weight_Pac_raw%>% 
  rename("Surface_Area" = "Surface.Area")

Buoyant_weight_Pru_raw <- read.csv("data/PRU_Buoyant_Weight.csv")

surface_area <- rbind(Buoyant_weight_Pac_raw[,c("Placement_Code", "Surface_Area")], Buoyant_weight_Pru_raw[,c("Placement_Code", "Surface_Area")])
response_data_export <- left_join(response_data, surface_area)
response_data_export <- left_join(response_data_export, meta[,c("Placement_Code","Slurry_Volume.ml", "Slurry_ID")])

#make the dataframe
CHL_calc <- left_join(response_data_export[,c("Placement_Code", "Slurry_ID", "Surface_Area", "Slurry_Volume.ml")], chl[,c("Slurry_ID", "Total_Chl_ug.mL")], by = join_by(Slurry_ID))

#calculate
CHL_calc <- CHL_calc %>% 
  mutate(Chl_ug.cm.2 = (Total_Chl_ug.mL * Slurry_Volume.ml) / Surface_Area)%>%
  filter(Placement_Code != "T0") %>% 
  distinct()

#put it back where it belongs
response_data_export <- left_join(CHL_calc[,c("Placement_Code", "Chl_ug.cm.2")], response_data_export) %>% 
  distinct()

write.csv(response_data_export, "output/T1_response_data.csv")

response_data_export
