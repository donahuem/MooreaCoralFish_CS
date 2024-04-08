#This is the version of the Full_Nutrient_Processing code that gives you a wider version of the dataframe
## Load Libraries
library(tidyverse)
library(lubridate)
library(here)
library(curl) # pull data from url
library(dplyr)

## Read in data
AugChemData<-read_csv(curl('https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Allbiogeochemdata_QC.csv')) %>% mutate(Season = "Dry")
#turb1<-read_csv(here("Data","Biogeochem","August2021","Turb_NC.csv"))
MarchChemData <- read_csv(curl('https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data//March2022/CarbonateChemistry/pHProbe_Data_calculated_POcorrect.csv')) %>% mutate(Season = "Wet")
#turb2<-read_csv(here("Data","Biogeochem","March2022","Turb_NC.csv"))
#depth <- read_csv(here("Data","Adj_Sandwich_Depth.csv"))
turb <- read_csv(curl("https://raw.githubusercontent.com/dbarnas/Community_Functional_Diversity/main/Data/Biogeochem/August2021/Turb_NC.csv"))


###DANIELLE REMOVED THESE:
# Join august and march data


## There seems to be a contaminated nutrient sample for V2 Low tide on the 8/8/2021.
# Remove outliers / irrelevant data points
removeSite1 <- AugChemData %>%
  filter(Season == "Dry",
         CowTagID == "V2",
         Tide == " Low",
         Day_Night == "Day",
         Date == ymd("2021-08-08"))

removeSite2 <- AugChemData %>%
  filter(Season == "Dry",
         CowTagID == "C4",
         Tide =="Low",
         Day_Night == "Night",
         Date == ymd("2021-08-09"))

## Filter out redundant low tide day sample, first 'low tide' was super high
removeSite3 <- AugChemData %>%
  filter(Season == "Dry",
         Tide == "Low",
         Day_Night == "Day",
         Date == ymd("2021-08-06"))

removeSite5 <- AugChemData %>%
  filter(CowTagID %in% c("VSPRING", "Varari_Well", "CSPRING"))

## Remove outliers (samples run on different day from rest) from March
removeSite4 <- MarchChemData %>%
  filter(Season == "Wet",
         CowTagID %in% c("VSPRING",  # remove unnecessary sites from my analysis
                         #"V17",
                         "VRC",
                         "CRC",
                         "CPIT",
                         "CPIT_Bottom",
                         "CSPRING_ROAD",
                         "CSPRING_BEACH",
                         "CSPRING_BEACH2"))

## Pull out Location, lat, lon of CowTagIDs
gps <- AugChemData[,c(1:4)] %>% distinct()

## Remove unnecessary/redundant data
AugChem <- AugChemData %>%
  dplyr::select(-c(Date,
                   Time,
                   DateTime,
                   Plate_Seep,
                   Top_Plate_ID,
                   Bottom_Plate_ID,
                   Jamie_Plate_ID,
                   #Temperature,
                   # Craig recommends to remove "because we don't hypothesize them to be
                   # orthogonal to any of the other fDOM we're using"
                   MarineHumic_Like, 
                   Lignin_Like)) %>%
  anti_join(removeSite1) %>% # remove outlier/irrelevant data
  anti_join(removeSite2) %>%
  anti_join(removeSite3) %>%
  anti_join(removeSite5) %>%
  #left_join(turb, by = c('CowTagID','Season')) %>% # join with T. ornata nutrient loading data
  left_join(gps, by = c('Location','CowTagID','lat','lon'))

MarchChem <- MarchChemData %>%
  rename(Temperature = TempInSitu) %>%
  dplyr::select(-c(Date,
                   SeepCode,
                   SamplingTime,
                   #Temperature,
                   Notes)) %>%
  anti_join(removeSite4) %>%
  #left_join(turb, by = c('CowTagID','Season')) %>%
  left_join(gps, by = c('CowTagID'))


ReducedChemData <- full_join(AugChem, MarchChem) %>%
  relocate(Season, .after = Day_Night) %>%
  # remove clearly incorrect value of Ammonia_umolL at V17
  filter(Ammonia_umolL < 16) %>% 
  filter(Location == "Varari")

#remove things that were only taken in dry season:
ReducedChemData <- ReducedChemData[,c(1:15)]
ReducedChemData$NNP_umolL <- ReducedChemData$NN_umolL / ReducedChemData$Phosphate_umolL
ReducedChemData <- ReducedChemData[,c(1:14, 16, 15)]

# here we summarize:
max_data <- ReducedChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = max, na.rm = T) %>% 
  rename_with(~ paste0("Maximum_", .), Salinity:Ammonia_umolL)

min_data <- ReducedChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = min, na.rm = T) %>% 
  rename_with(~ paste0("Minimum_", .), Salinity:Ammonia_umolL)

mean_data <- ReducedChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = mean, na.rm = T) %>% 
  rename_with(~ paste0("Mean_", .), Salinity:Ammonia_umolL)

mean_low_data <- ReducedChemData %>%
  filter(Tide == "Low") %>% 
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = mean, na.rm = T) %>% 
  rename_with(~ paste0("Low_Tide_Mean_", .), Salinity:Ammonia_umolL)

sd_data <- ReducedChemData %>%
  group_by(Location, CowTagID) %>% 
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = sd, na.rm = T) %>% 
  rename_with(~ paste0("sd_", .), Salinity:Ammonia_umolL)

cv_chem_data <- left_join(mean_data, sd_data, by = join_by(Location, CowTagID))

cv_chem_data <- cv_chem_data %>%
  mutate(CV_Salinity = sd_Salinity / Mean_Salinity,
         CV_Temperature = sd_Temperature / Mean_Temperature,
         CV_TA = sd_TA / Mean_TA,
         CV_pH = sd_pH / Mean_pH,
         CV_Phosphate_umolL = sd_Phosphate_umolL / Mean_Phosphate_umolL,
         CV_Silicate_umolL = sd_Silicate_umolL / Mean_Silicate_umolL,
         CV_NN_umolL = sd_NN_umolL / Mean_NN_umolL,
         CV_Ammonia_umolL = sd_Ammonia_umolL / Mean_Ammonia_umolL) %>% 
  ungroup() %>% 
  dplyr::select(-starts_with("sd"))

min_max_data <- left_join(min_data, max_data, by = join_by(Location, CowTagID))

Summarized_chem_data <- left_join(min_max_data, cv_chem_data, by = join_by(Location, CowTagID))
Summarized_chem_data <- left_join(Summarized_chem_data, mean_low_data, by = join_by(Location, CowTagID))
Summarized_chem_data<- Summarized_chem_data[,c(2:46)]

Summarized_chem_data$Time <- "All"

#Do the same thing but only for wet season data:
WetChemData <-ReducedChemData %>% 
  filter(Season == "Wet")

max_data_wet <- WetChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = max, na.rm = T) %>% 
  rename_with(~ paste0("Maximum_", .), Salinity:Ammonia_umolL)

min_data_wet <- WetChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = min, na.rm = T) %>% 
  rename_with(~ paste0("Minimum_", .), Salinity:Ammonia_umolL)

mean_low_data_wet <- ReducedChemData %>%
  filter(Tide == "Low") %>% 
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = mean, na.rm = T) %>% 
  rename_with(~ paste0("Low_Tide_Mean_", .), Salinity:Ammonia_umolL)

mean_data_wet <- WetChemData %>%
  group_by(Location, CowTagID) %>%
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = mean, na.rm = T) %>% 
  rename_with(~ paste0("Mean_", .), Salinity:Ammonia_umolL)

sd_data_wet <- WetChemData %>%
  group_by(Location, CowTagID) %>% 
  summarise_at(vars(Salinity:Ammonia_umolL), .funs = sd, na.rm = T) %>% 
  rename_with(~ paste0("sd_", .), Salinity:Ammonia_umolL)

cv_chem_data_wet <- left_join(mean_data_wet, sd_data_wet, by = join_by(Location, CowTagID))

cv_chem_data_wet <- cv_chem_data_wet %>%
  mutate(CV_Salinity = sd_Salinity / Mean_Salinity,
         CV_Temperature = sd_Temperature / Mean_Temperature,
         CV_TA = sd_TA / Mean_TA,
         CV_pH = sd_pH / Mean_pH,
         CV_Phosphate_umolL = sd_Phosphate_umolL / Mean_Phosphate_umolL,
         CV_Silicate_umolL = sd_Silicate_umolL / Mean_Silicate_umolL,
         CV_NN_umolL = sd_NN_umolL / Mean_NN_umolL,
         CV_Ammonia_umolL = sd_Ammonia_umolL / Mean_Ammonia_umolL) %>% 
  ungroup() %>% 
  dplyr::select(-starts_with("sd"))

min_max_data_wet <- left_join(min_data_wet, max_data_wet, by = join_by(Location, CowTagID))

Summarized_chem_data_wet <- left_join(min_max_data_wet, cv_chem_data_wet, by = join_by(Location, CowTagID))
Summarized_chem_data_wet <- left_join(Summarized_chem_data_wet, mean_low_data_wet, by = join_by(Location, CowTagID))
Summarized_chem_data_wet <- Summarized_chem_data_wet[,c(2:46)]

Summarized_chem_data_wet$Time <- "Wet"

#write_csv(Summarized_chem_data, here("data","All_Nutrients_Processed.csv"))
#write_csv(Summarized_chem_data_wet, here("data","Wet_Nutrients_Processed.csv"))

nutrients2 <- left_join(mean_low_data, cv_chem_data, by = join_by(Location, CowTagID)) %>% 
  dplyr::select(-starts_with("Mean_"))
nutrients2$Time <- "All"

nutrients2_wet <- left_join(mean_low_data_wet, cv_chem_data_wet, by = join_by(Location, CowTagID)) %>% 
  dplyr::select(-starts_with("Mean_"))
nutrients2_wet$Time <- "Wet"

#write_csv(Summarized_chem_data, here("data","All_Nutrients_Processed_CV_Low.csv"))
#write_csv(Summarized_chem_data_wet, here("data","Wet_Nutrients_Processed_CV_Low.csv"))
