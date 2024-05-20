library(tidyr)
library(dplyr)
library(here)

SI <- read.csv("data/STABLE_ISOTOPE_RAW.csv")
meta <- read.csv("data/coral_metadata.csv")

SI <- SI[,c(1:6)] #remove random extra columns
# Create a new column 'HS' containing the last 4 characters of 'Sample.ID'
SI$HS <- substr(SI$Sample.ID, nchar(SI$Sample.ID) - 3, nchar(SI$Sample.ID))

# Create a new column 'Sample.ID' containing all characters except the last 4 characters
SI$Sample.ID <- substr(SI$Sample.ID, 1, nchar(SI$Sample.ID) - 5)
SI$δ15N <- SI$δ15N...........vs..AIR.
SI$δ13C <- SI$δ13C............vs..VPDB

# Now your_data contains 'Sample.ID' split into two columns 'Sample.ID' and 'HS'
SI_cleaner <- SI[SI$Sample.ID != "", ]

SI_cleaner <- SI_cleaner %>%
  rename(`Micro_Vial` = Sample.ID)

SI_meta <- left_join(SI_cleaner, meta, by = join_by(Micro_Vial))
SI_meta$Genotype[is.na(SI_meta$Genotype)] <- SI_meta$Micro_Vial[is.na(SI_meta$Genotype)]

#Change TO Pin_Number Placement Code
SI_meta$Pin_Number <- ifelse(is.na(SI_meta$Pin_Number), "T0", SI_meta$Pin_Number)
SI_meta$Placement_Code <- ifelse(is.na(SI_meta$Placement_Code), "T0", SI_meta$Placement_Code)

#Make CowTagID
SI_meta$CowTagID <- ifelse(SI_meta$Pin_Number != "T0", paste0("V", SI_meta$Pin_Number), SI_meta$Pin_Number)

# Changing the Species column for T0
SI_meta$Species <- ifelse(is.na(SI_meta$Species) & substr(SI_meta$Genotype, 2, 2) == "A", "Pocillopora acuta",
                     ifelse(is.na(SI_meta$Species) & substr(SI_meta$Genotype, 2, 2) == "R", "Porites rus",
                            SI_meta$Species))

#Calculate "Δ15N"
SI_meta_Δ_calc <- SI_meta[,c("Micro_Vial", "HS",  "δ15N","δ13C" )] %>%
  pivot_wider(names_from = HS, values_from = c("δ15N", "δ13C"), names_sep = "_")

SI_meta_Δ_calc$Δ15N <- SI_meta_Δ_calc$δ15N_host - SI_meta_Δ_calc$δ15N_symb
SI_meta_Δ_calc$Δ13C <- SI_meta_Δ_calc$δ13C_host - SI_meta_Δ_calc$δ13C_symb

SI_meta <- left_join(SI_meta, SI_meta_Δ_calc[,c("Micro_Vial", "Δ15N","Δ13C")])

SI_meta$C_N_ratio <- SI_meta$μg.C / SI_meta$μg.N
SI_meta$Percent_N <- ((SI_meta$μg.N* 0.001) /(SI_meta$Weight..mg.)) * 100
SI_meta$Percent_C <- ((SI_meta$μg.C* 0.001) /(SI_meta$Weight..mg.)) * 100

#Make T0 dataframe so we can make T1-T0
# Assuming SI_meta is your initial dataframe
host_df_T0 <- SI_meta %>%
  filter(Pin_Number == "T0") %>% 
  filter(HS == "host") %>%
  dplyr::select(Genotype, δ15N_T0 = δ15N, δ13C_T0 = δ13C)

symb_df_T0 <- SI_meta %>%
  filter(Pin_Number == "T0") %>% 
  filter(HS == "symb")%>%
  dplyr::select(Genotype, δ15N_T0 = δ15N, δ13C_T0 = δ13C)

host_df<- SI_meta %>%
  filter(HS == "host") %>% 
  left_join(host_df_T0, by = "Genotype") %>% 
  mutate(δ15N_T1_T0 = δ15N - δ15N_T0) %>% 
  mutate(δ13C_T1_T0 = δ13C - δ13C_T0)

symb_df <- SI_meta %>%
  filter(HS == "symb") %>% 
  left_join(symb_df_T0, by = "Genotype")%>% 
  mutate(δ15N_T1_T0 = δ15N - δ15N_T0) %>% 
  mutate(δ13C_T1_T0 = δ13C - δ13C_T0)

SI_meta <- rbind(host_df, symb_df)

#now re-doing the ∆ calculations with this in mind
#this is then how much it shifted on the gradient from the colony 'baseline' amount of heterotrophy / autotrophy
SI_meta_Δ_calc <- SI_meta[,c("Micro_Vial", "HS",  "δ15N_T1_T0","δ13C_T1_T0" )] %>%
  pivot_wider(names_from = HS, values_from = c("δ15N_T1_T0", "δ13C_T1_T0"), names_sep = "_")

SI_meta_Δ_calc$Δ15N_T1T0 <- SI_meta_Δ_calc$δ15N_T1_T0_host - SI_meta_Δ_calc$δ15N_T1_T0_symb
SI_meta_Δ_calc$Δ13C_T1T0 <- SI_meta_Δ_calc$δ13C_T1_T0_host - SI_meta_Δ_calc$δ13C_T1_T0_symb

SI_meta <- left_join(SI_meta, SI_meta_Δ_calc[,c("Micro_Vial", "Δ15N_T1T0","Δ13C_T1T0")])

#Percent N just because
SI_meta$Percent_N15 <- SI_meta$δ15N...........vs..AIR./SI_meta$μg.N * 100
SI_meta$Percent_C13 <- SI_meta$δ13C............vs..VPDB./SI_meta$μg.C * 100

#write_csv(SI_meta, here("Data","SI_Tidy.csv"))

SI_meta %>% 
  filter(!Placement_Code == "T0") %>% 
  ggplot(aes(x = Δ15N_T1T0, y = Δ13C_T1T0)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm") +
  #  geom_text(aes(label = Pin_Number)) +
  facet_wrap(~Species, scales = "free_x")

SI_meta %>% 
  filter(Species == "Pocillopora acuta") %>% 
  hist(Δ15N_T1T0)
