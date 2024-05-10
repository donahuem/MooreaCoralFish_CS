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

write_csv(SI_meta, here("Data","SI_Tidy.csv"))