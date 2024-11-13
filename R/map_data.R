#Making the dataframe for the QGIS map:
db <- read.csv("https://raw.githubusercontent.com/dbarnas/Community_Functional_Diversity/main/Data/Full_Metadata.csv") #Rugosity data pulled from Danielle Barnas' measurements
all_nut <- read.csv("data/All_Nutrients_Processed.csv")
pulse_columns <- names(all_nut)[grepl("Low_Tide_Mean_", names(all_nut)) & 
                                  !grepl("Temperature|pH|Ammonia_umol|TA",names(all_nut))]
nut_no_seep <- all_nut[all_nut$CowTagID != 'VSEEP',]
nut_no_seep_scaled <- nut_no_seep %>%
  mutate(across(all_of(pulse_columns), scale))

pca.data <- na.omit(nut_no_seep_scaled[, c(pulse_columns)]) 
pca = princomp(pca.data, cor=TRUE)
biplot(pca)

#NOT TRANSFORMED SO HIGH VALUES = LOW SGD
nut_no_seep_scaled$pc1 = 1*(pca$scores[,1]) #what's this data's score on pc1 axis
nut_no_seep_scaled$pc2 = 1*(pca$scores[,2]) #what's this data's score on pc1 axis

# Add back in the seep
data_means <- colMeans(pca.data)
data_sds <- apply(pca.data, 2, sd)
seep <- all_nut[all_nut$CowTagID == 'VSEEP',]
pca.data_seep_not_scaled <- seep[, c(pulse_columns)]
seep_scaled <- as.matrix((pca.data_seep_not_scaled - data_means) / data_sds)
# Calculate the PCA scores for the outlier
loadings <- pca$loadings
outlier_scores <- as.vector(seep_scaled %*% loadings)


## FOR MAKING QGIS MAPS
map_data <- db[c("CowTagID", "lat", "lon","Location", "dist_to_seep_m")] %>%
  filter(Location == "Varari") %>%
  filter(grepl("^V", CowTagID)) %>%
  # filter(!(CowTagID == "V13")) %>% #comment this out for coral
  # filter(!(CowTagID == "V2")) %>%  #comment this out for coral
  mutate(dist_to_seep_m_temp = ifelse(dist_to_seep_m < 0, 10000, dist_to_seep_m)) %>%
  mutate(dist_to_seep_m_temp = ifelse(dist_to_seep_m_temp == 0, Inf, dist_to_seep_m_temp)) %>%
  arrange(dist_to_seep_m) %>%
  mutate(alphabetical = case_when(
    CowTagID == "VSEEP" ~ "SEEP",
    TRUE ~ toupper(letters[rank(dist_to_seep_m_temp)])
  )) %>%
  dplyr::select(-dist_to_seep_m_temp)
#

map_data_pc1 <- map_data %>%
  left_join(nut_no_seep_scaled[,c("CowTagID","pc1")])

map_data_pc1$pc1[map_data_pc1$CowTagID == "VSEEP"] <- outlier_scores[1]

map_data_pc1$log_pc <- log(map_data_pc1$pc1+110)

write_csv(map_data_pc1, here("Data", "map_data_pc1.csv"))

map_data_pc1_rm_seep <- map_data_pc1[map_data_pc1$CowTagID != 'VSEEP',]
write_csv(map_data_pc1_rm_seep, here("Data", "map_data_pc1_no_seep.csv"))
