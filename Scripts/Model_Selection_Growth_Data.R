#### Model selection on linear and nonlinear regressions of Growth versus SGD parameters
### Created by Callie Stephenson
### Created on July 20th, 2023
###

##################################
# LOAD LIBRARIES
###############################
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)
library(tidytext)
library(AICcmodavg)
library(kableExtra)
library(PNWColors)

###############################
# READ IN DATA
###############################
meta <- read_csv(here("Data", "Coral_Data","PRU_Coral_Codes.csv"))
#meta <- read_csv(here("Data", "Coral_Data","PAC_Coral_Codes.csv"))
BW <- read_csv(here("Data","Coral_Data","Skeletal_Dry_Weight_Calc_Pru.csv")) 
#BW <- read_csv(here("Data","Coral_Data","PAC_Buoyant_Weight.csv"))
TLE_all <- read_csv(here("Data","Coral_Data","TLE_Pru.csv"))
#TLE_all <- read_csv(here("Data","Coral_Data","TLE_Pac.csv"))
chem <- read_csv(here("Data","Biogeochem", "Nutrients_Processed_All.csv")) %>%
  filter(Season == "Dry") %>%
  filter(Location == "Varari", CowTagID != "V13") %>%
  filter(CowTagID != "VSEEP") %>%
  select(CowTagID, Parameters, CVSeasonal) %>%
  pivot_wider(names_from = Parameters, values_from = CVSeasonal)
alphatag <- read_csv("https://raw.githubusercontent.com/dbarnas/Community_Functional_Diversity/main/Data/CowTag_to_AlphaTag.csv")


##################
##Prepare BW Data
##################
BW_sum <- BW %>% 
  select(Vial_No_Only, Placement_Code, Change, Percent_Change, Change_Over_Area)

###################
##Prepare TLE Data
###################
TLE_sum <- TLE_all %>%
  mutate(
    Max_L_1 = T1_Feret_1 - T0_Feret_1,
    Max_L_2 = T1_Feret_2 - T0_Feret_2,
    Max_L_Basal = T1_Feret_3 - T0_Feret_3,
    Area_1 = T1_Area_1 - T0_Area_1,
    Area_2 = T1_Area_2 - T0_Area_2,
    Area_Basal = T1_Area_3 - T0_Area_3
  ) %>%
  select(Vial_No_Only, Placement_Code, Max_L_1, Max_L_2, Max_L_Basal, Area_1, Area_2, Area_Basal)

  
#########
##Prepare the data
##########

for_model <- meta %>%
  select(-c(1,5,6,8)) %>%
  mutate(CowTagID = paste0("V", Pin_Number))%>%
  filter(CowTagID != "V13")%>%
  as_tibble() %>%
  left_join(BW_sum) %>%
  left_join(TLE_sum) %>%
  left_join(chem)%>%
  drop_na(Percent_Change)

# for no interactive effect
modTable <- tibble(Y = as.character(),
                   Parameter = as.character(),
                   Reg_Type = as.character(), # linear or nonlinear
                   AICc = as.numeric(),
                   R2 = as.numeric(),
                   pVal.P = as.numeric(),
                   #pVal.Rug = as.numeric()
)

myDep <- colnames(for_model  %>% select(c(8:15))) #May need to change the numbers depending on the model's table!
mydata <-for_model %>% select(c(8:15),CowTagID, Cage_Uncaged, Genotype, Salinity, Temperature, pH, Phosphate_umolL, Silicate_umolL, NN_umolL)

for(i in myDep){
  Y <- as.character(i)
  k <- which(myDep == i) # use as multiplier for list
  
  for(j in 12:ncol(mydata)){
    Parameter <- colnames(mydata[j])
    
    # with rugosity as covariate
    model1R <- lm(paste0(Y, "~", Parameter, ""), data = mydata)
    subdata1R <- as_tibble(cbind(Y,Parameter)) %>%
      mutate(Parameter = paste0(Parameter,"")) %>%
      mutate(Reg_Type = "Linear",
             AICc = AICc(model1R),
             R2 = summary(model1R)$r.squared,
             pVal.P = summary(model1R)$coefficients[8],
             #pVal.Rug = summary(model1R)$coefficients[8]
      )
    
    # with rugosity as covariate
    model2R <- lm(paste0(Y, "~ poly(", Parameter, ",2)"), data = mydata)
    subdata2R <- as_tibble(cbind(Y,Parameter)) %>%
      mutate(Parameter = paste0(Parameter,"")) %>%
      mutate(Reg_Type = "Polynomial",
             AICc = AICc(model2R),
             R2 = summary(model2R)$r.squared,
             pVal.P = summary(model2R)$coefficients[12],
             #pVal.Rug = summary(model2R)$coefficients[16]
      )
    
    
    modTable <- modTable %>%
      rbind(subdata1R) %>%
      rbind(subdata2R)
    
  }
}


########### filter
modelTable <- modTable %>%
  #filter(Parameter != "meanRugosity + Rugosity") %>%  # do not include alone in model test
  #drop_na(pVal) %>%  # remove rugosity:rugosity
  group_by(Y) %>%
  mutate(minAIC = min(AICc)) %>%
  mutate(delAICc = AICc - minAIC) %>%
  select(-c(minAIC)) %>%
  mutate(Parameter = if_else(Parameter == "NN_umolL", "Nitrate+Nitrite",
                             if_else(Parameter == "Phosphate_umolL", "Phosphate",
                                     if_else(Parameter == "Silicate_umolL", "Silicate", Parameter))))

write_csv(modelTable, here("Data","Output", "Model_Selection_Growth_Data.csv"))
