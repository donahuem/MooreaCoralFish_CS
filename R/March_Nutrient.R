#### Process March nutrient data and save as csv
#### Created by Callie Stephenson
#### Created on 10/26/23

## Load Libraries
library(tidyverse)
library(lubridate)
library(here)
library(curl)

## Read in data
AugChemData<-read_csv(curl('https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Allbiogeochemdata_QC.csv')) %>% mutate(Season = "Dry")
MarchChemData <- read_csv(curl('https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data//March2022/CarbonateChemistry/pHProbe_Data_calculated_POcorrect.csv')) %>% mutate(Season = "Wet")
turb <- read_csv(curl("https://raw.githubusercontent.com/dbarnas/Community_Functional_Diversity/main/Data/Biogeochem/March2022/Turb_NC.csv"))

## Pull out Location, lat, lon of CowTagIDs from Aug data (not in March data)
gps <- AugChemData %>% 
  filter(substr(CowTagID, 1, 1) == "V") %>% #subset for only Varari data
  select(CowTagID, lat, lon) %>% 
  distinct()

#Clean up MarchChem and join it with gps and turb data
MarchChem <- MarchChemData %>%
  filter(substr(CowTagID, 1, 1) == "V") %>%
  rename(Temperature = TempInSitu) %>%
  filter(!(CowTagID %in% c("VSPRING", "VRC"))) %>%#DB removed these "unnecessary" sites. I am too.
  left_join(gps, by = c('CowTagID')) %>% 
  left_join(turb, by = c('CowTagID')) %>% 
  select(-c(Date, SeepCode, SamplingTime, Notes, Season)) %>% 
  filter(Ammonia_umolL < 16) #remove clearly incorrect value of Ammonia_umolL at V17  

#Summarize it!
#make the max
max_data <- MarchChem %>%
  group_by(CowTagID) %>%
  summarise_at(vars(Salinity:Phosphate_umolL), .funs = max, na.rm = T) %>%  # select for max values
  pivot_longer(cols = Salinity:Phosphate_umolL, names_to = "Parameters", values_to = "Maximum")

#make the min
min_data <- MarchChem %>%
  group_by(CowTagID) %>%
  summarise_at(vars(Salinity:Phosphate_umolL), .funs = min, na.rm = T) %>%  # select for max values
  pivot_longer(cols = Salinity:Phosphate_umolL, names_to = "Parameters", values_to = "Minimum")

#make the mean
mean_dataS <- MarchChem %>%
  group_by(CowTagID) %>% 
  summarise_at(vars(Salinity:Phosphate_umolL), .funs = mean, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = c(Salinity:Phosphate_umolL), names_to = "Parameters", values_to = "MeanSeasonal")

#make the standard deviation
sd_dataS <- MarchChem %>%
  group_by(CowTagID) %>%
  # Calculate mean values and pivot longer to join for CV calculation
  summarise_at(vars(Salinity:Phosphate_umolL), .funs = sd, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = c(Salinity:Phosphate_umolL), names_to = "Parameters", values_to = "sdS")

# join max and min and mean and cv and gps data
full_data <- full_join(max_data, min_data) %>%
  mutate(Range = Maximum - Minimum)
full_data <- left_join(full_data, mean_dataS) #R makes me break this out of the pipe to run properly
full_data <- left_join(full_data, sd_dataS) %>% 
  mutate(CVSeasonal = sdS / MeanSeasonal) %>%
  select(-c(sdS))
full_data <- left_join(full_data, gps)

#YOU DID IT! <3
#MAKE IT A CSV:
write_csv(full_data,"data/March_nutrients_processed.csv")
