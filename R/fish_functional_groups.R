# Fish Functional Groups
mcr_fish <- read.csv("data/MCR_LTER_Annual_Fish_Survey_20230615.csv") %>% #Find this file in google drive, too big for git
  filter(!Year == "(100840 rows)")

functional_groups <- mcr_fish %>%
  distinct(Taxonomy, Family, Fine_Trophic) %>% 
  mutate(Species = Taxonomy) %>% 
  select(-Taxonomy)

#rm(mcr_fish) #didn't know you could do that!

maxn1 <-left_join(maxn, functional_groups, by = join_by(Species))

na_functional <- maxn1 %>% 
  filter(is.na(Fine_Trophic)) %>% 
  distinct(Species)

na_family <- maxn1 %>% 
  filter(is.na(Family)) %>% 
  distinct(Species)

identical(na_functional, na_family) #should be true

Fine_Trophic <- c("Herbivore/Detritivore", #looked these up
                  "Benthic Invertebrate Consumer",
                  "Benthic Invertebrate Consumer", #Fishbase feeds on fishes, crustaceans, and molluscs
                  "Primary Consumer",
                  "Scraper",
                  "Benthic Invertebrate Consumer",
                  "Benthic Invertebrate Consumer",
                  "Benthic Invertebrate Consumer",
                  "Piscivore"
                  
)

Family <- c("Pomacentridae",
            "Labridae",
            "Dasyatidae",
            "Acanthuridae",
            "Scaridae",
            "Mugilidae",
            "Labridae",
            "Mugilidae",
            "Bothidae"
)

na_fix_fish_functional <- cbind(na_functional,Fine_Trophic,Family)
functional_groups <- rbind(na_fix_fish_functional, functional_groups)

write_csv(functional_groups, here("Data", "fish_functional_groups.csv"))
