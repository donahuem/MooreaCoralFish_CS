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
  select(Location, CowTagID, lat, lon) %>% 
  distinct()

#Clean up MarchChem and join it with gps
MarchChem <- MarchChemData %>%
  filter(substr(CowTagID, 1, 1) == "V") %>%
  rename(Temperature = TempInSitu) %>%
  filter(!(CowTagID %in% c("VSPRING", "VRC"))) %>% #DB removed these "unnecessary" sites. I am too.
  select(-c(Date,
            SeepCode,
            SamplingTime,
            #Temperature,
            Notes)) %>%
  left_join(gps, by = c('CowTagID'))%>%
  filter(Ammonia_umolL < 16) %>%#remove clearly incorrect value of Ammonia_umolL at V17  
  left_join(turb, by = c('CowTagID'))

