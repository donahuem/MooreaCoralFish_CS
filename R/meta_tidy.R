##METADATA
PAC_meta <- read.csv("data/PAC_Coral_Codes.csv") 
PRU_meta <- read.csv("data/PRU_Coral_Codes.csv") 

PAC_meta <- PAC_meta[,c(3:10)]
PAC_meta$Species <- "Pocillopora acuta"
PAC_meta$Genotype <- gsub('PR21', 'PA21', PAC_meta$Genotype) #fix a typo in the genotypes
PRU_meta <- PRU_meta[,c(2:9)]
PRU_meta$Species <- "Porites rus"

meta <- rbind(PAC_meta, PRU_meta)
meta$Cage_Uncaged <- ifelse(meta$Cage_Uncaged == "T0", meta$Cage_Uncaged, 
                            substr(meta$Placement_Code, nchar(meta$Placement_Code),
                                   nchar(meta$Placement_Code)))
meta$Cage_Uncaged <- as.factor(meta$Cage_Uncaged)

write_csv(meta, here("Data","coral_metadata.csv"))
