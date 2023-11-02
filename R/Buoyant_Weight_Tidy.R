## Density of aragonite = 2.93 g/cm-3 (Jokiel et al. 1978)
## SA: wax dipping technique (Stimson and Kinzie 1991, Veal et al 2010)
## avg sea water density = 1.023 g cm-3
#############################
### Load up some libaries
#############################
library(dplyr)
library(tidyr)
library(here)
library(readr)
library(seacarb)
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(seacarb)
library(here)
#############################
### First we input coral data
#############################
#setwd("/Users/calliestephenson/Documents/GitHub/MooreaCoralFish_CS/")
Buoyant_weight_Pac_raw <- read.csv("data/PAC_Buoyant_Weight.csv")

#############################
### CALCULATE DRY WEIGHT
#############################
#### Using function rho from seacarb package
## rho(S = 35, T = 25, P = 0)
BwPac <- Buoyant_weight_Pac_raw %>%
  mutate(T0_sw_dens = rho(S = T0_Sal, T = T0_Temp, P = 0), # calculate density of seawater
         T0_sw_dens = T0_sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  mutate(T1_sw_dens = rho(S = T1_Sal, T = T1_Temp, P = 0), # calculate density of seawater
         T1_sw_dens = T1_sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  mutate(T0_dry_weight.g = T0_Weight / (1 - (T0_sw_dens/2.93))) %>% # dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)
  mutate(T1_dry_weight.g = T1_Weight / (1 - (T1_sw_dens/2.93))) %>% # dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)
  mutate(Change = T1_dry_weight.g - T0_dry_weight.g) %>%
  mutate(Percent_Change = Change / T0_dry_weight.g) %>%
  mutate(Percent_Change_100 = Percent_Change * 100) %>%
  mutate(Change_Over_Area= Change / Surface.Area) %>%
  select(Vial_No_Only, Placement_Code, T0_dry_weight.g, T1_dry_weight.g, Change, Change_Over_Area, Percent_Change) %>% 
  mutate(Species="PAC")

#write_csv(BwPac, here("output", "Skeletal_Dry_Weight_Calc_Pac.csv"))
#############################
### SAME FOR PRU
#############################
Buoyant_weight_Pru_raw <- read.csv("data/PRU_Buoyant_Weight.csv")

BwPru <- Buoyant_weight_Pru_raw %>%
  mutate(T0_sw_dens = rho(S = T0_Sal, T = T0_Temp, P = 0), # calculate density of seawater
         T0_sw_dens = T0_sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  mutate(T1_sw_dens = rho(S = T1_Sal, T = T1_Temp, P = 0), # calculate density of seawater
         T1_sw_dens = T1_sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  mutate(T0_dry_weight.g = T0_Weight / (1 - (T0_sw_dens/2.93))) %>% # dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)
  mutate(T1_dry_weight.g = T1_Weight / (1 - (T1_sw_dens/2.93))) %>% # dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)
  mutate(Change = T1_dry_weight.g - T0_dry_weight.g) %>%
  mutate(Percent_Change = Change / T0_dry_weight.g) %>%
  mutate(Percent_Change_100 = Percent_Change * 100) %>%
  mutate(Change_Over_Area= Change / Surface_Area) %>%
  select(Vial_No_Only, Placement_Code, T0_dry_weight.g, T1_dry_weight.g, Change, Change_Over_Area, Percent_Change) %>% 
  mutate(Species="PRU")

#write_csv(BwPru, here("output", "Skeletal_Dry_Weight_Calc_Pru.csv"))

BW_all <- rbind(BwPac, BwPru) %>% 
  drop_na()

#this if / else checks whether that file exists to prevent overwriting the file unnecesarily
file_path <- "data/BW_tidy.csv"
if (!file.exists(file_path)) {
  write.csv(BW_all, file = file_path)
} else {
  # Do nothing
}