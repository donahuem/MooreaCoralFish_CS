Coral Growth and Symbionts Analysis
================
Callie Stephenson
2023-08-29

## Introduction

This is a R Markdown file in which I hope to write out my analyses. I
will use this document to process all the R scripts found in the R
folder and complete my project.

### NS reccomendation

- Grab both dataframes and make the same version of nut
- make graphs that have the min/max (like you’ve put into the PCA) and
  then the PC axes and then the bw
- use BW just caged and do model selection like Kyle says

#### Loading the data

``` r
turb1 <- turb %>% 
  filter(startsWith(CowTagID, "V")& CowTagID != "VSEEP")

turb1 <- turb1 %>%
  dplyr::select(c(CowTagID,
                   N_percent,
                   C_N))
#use C:N 
#can also use N percent
#plot(turb$C_N, turb$N_percent)
#hist(turb$C_N)
#hist(turb$N_percent)
#N_percent looks better. Will use N_percent for models
```

``` r
all_nut_long <- pivot_longer(
  data = all_nut,
  cols = c(Minimum_Salinity:CV_Ammonia_umolL)) 
names(all_nut_long)[names(all_nut_long) == "name"] <- "Parameter"

join <- explanatory_seasonal[,c(2:7)]
join <- unique(join)

explanatory_all <- left_join(join,all_nut_long, by = join_by(CowTagID))
explanatory_all_wide <- left_join(join, all_nut,by = join_by(CowTagID))
explanatory_all_wide <- left_join(explanatory_all_wide, turb1,by = join_by(CowTagID))
```

``` r
#now make an all data that has both explanatory and response variables:
all_data_seasonal <- left_join(response_data, explanatory_seasonal, by = join_by(CowTagID))
#suppress warnings because there will be a many-to-many relationship between x and y
all_data_seasonal$Genotype <- as.factor(all_data_seasonal$Genotype)
all_PAC_data_seasonal <- all_data_seasonal %>% 
  filter(Species == "Pocillopora acuta")
```

``` r
#now make an all data that has both explanatory and response variables:
all_data <- left_join(response_data, explanatory_all, by = join_by(CowTagID))
#suppress warnings because there will be a many-to-many relationship between x and y
all_data$Genotype <- as.factor(all_data$Genotype)
all_PAC_data <- all_data %>% 
  filter(Species == "Pocillopora acuta")

all_data_wide <- left_join(response_data, explanatory_all_wide, by = join_by(CowTagID))
```

## Data Exploration:

Data exploration focuses on the following points: 1. Outliers 2.
Collinearity 3. Independence (Relationships between response and
explanatory variables)

![](analysis_files/figure-gfm/response%20data%20hist-1.png)<!-- -->

Ok so FSC.Events are both pretty right-skewed. CN says log10 transform
these.

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/wet%20season%20raw%20growth%20data%20plots-1.png)<!-- -->

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/wet%20season%20raw%20growth%20data%20plots-2.png)<!-- -->
Now, doing this again but for both seasons combined:

``` r
species <- unique(all_data$Species)
for (sp in species) {
  # Filter data for the current species and only caged
  species_data <- all_data %>%
    filter(Species == sp, Cage_Uncaged == "C")
  
  # Create plot for the current species
  plot <- species_data %>%
    ggplot(aes(x = value, y = Percent_Change)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~ Parameter, scales = "free") +
    ggtitle(paste(sp)) +  #Add species name to the plot title
    theme_bw()
  # Print and save the plot
  print(plot)
  #ggsave(filename = paste("output/raw_growth_data_response_plot_", sp, ".png", sep = ""),
  #       plot = plot, 
  #       width = 15, height = 15)
}
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/both%20season%20raw%20growth%20data%20plots-1.png)<!-- -->

    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/both%20season%20raw%20growth%20data%20plots-2.png)<!-- -->

Same but let’s do for FCM data

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/wet%20season%20raw%20symbiont%20count%20plots-1.png)<!-- -->

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/wet%20season%20raw%20symbiont%20count%20plots-2.png)<!-- -->

And again, now for both seasons:

    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/both%20seasons%20raw%20symbiont%20count%20plots-1.png)<!-- -->

    ## `geom_smooth()` using formula = 'y ~ x'

![](analysis_files/figure-gfm/both%20seasons%20raw%20symbiont%20count%20plots-2.png)<!-- -->

#### Look for Outliers:

Buoyant Weight It looks like that 13 A is really off. We know it broke,
so I am removing it and replotted next to it:

Now making a plot to look at outliers:

![](analysis_files/figure-gfm/BW%20Outlier%20Plot-1.png)<!-- --> I
removed PRU V13 A placement because this was broken in the field,
creating a large negative change in buoyant weight.

TLE ![](analysis_files/figure-gfm/TLE%20Plot-1.png)<!-- -->

### Symbionts

![](analysis_files/figure-gfm/Sym%20plot-1.png)<!-- --> Jess will rerun
PRU 49 (PRU V17 C) with her samples - I likely added twice as much as
needed to the sample, but for now I will omit in my analyses (using
dataframe FCM_no for “no outliers”)

``` r
ggplot(FCM, aes(x = Cage_Uncaged, y = log10(FSC.Events_per_cm_2), fill = Species)) +
  geom_violin() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +  # Adding jitter
  scale_fill_fish_d(option = "Acanthurus_olivaceus", alpha = 0.4, begin = 0.3, end = 1)+
  facet_wrap(~Species)+
  labs(title = expression(paste("Symbionts per ", cm^2, " by caging treatment")))+
  theme(plot.title = element_text(hjust = 0.5))
```

![](analysis_files/figure-gfm/FCM%20Violin%20Plot-1.png)<!-- --> same
but other normalization:

``` r
ggplot(FCM, aes(x = Cage_Uncaged, y = log10(FSC.Events_per_g_dry_weight), fill = Species)) +
  geom_violin() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +  # Adding jitter
  scale_fill_fish_d(option = "Acanthurus_olivaceus", alpha = 0.4, begin = 0.3, end = 1) +
  facet_wrap(~Species)+
  labs(title = "Symbionts per g dry weight by caging treatment")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](analysis_files/figure-gfm/FCM%20Violin%20Plot%20Dry%20Weight-1.png)<!-- -->

### Collinearity

need to make a dataframe that’s long that has all the nutrient metrics
and independent variables Here’s the thing: we KNOW the nutrients are
collinear For example, here’s a pairs plot for the maximum of all the
nutrient values:

``` r
nut_wide_no_seep<- nut_wide[nut_wide$CowTagID != 'VSEEP', ]
maximum_columns <- names(nut_wide)[grepl("Maximum_", names(nut_wide)) & !grepl("Salinity|Temperature|pH|Ammonia_umol", names(nut_wide))]

ggpairs(nut_wide_no_seep[, c(maximum_columns)],progress = FALSE)
```

![](analysis_files/figure-gfm/pairs%20plot%20collinearity-1.png)<!-- -->

### Independence

### Data Analysis

FCM Data Analysis:

    ##              Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Cage_Uncaged  2 2.802e+08 140086728   0.596  0.555
    ## Residuals    56 1.317e+10 235216369

    ##              Df    Sum Sq  Mean Sq F value Pr(>F)
    ## Cage_Uncaged  2  16430837  8215419   0.659  0.521
    ## Residuals    56 697625032 12457590

### Growth Models

``` r
#minimum_columns <- names(all_data_wide)[grepl("Minimum_", names(all_data_wide)) & 
#                                     grepl("Salinity|Temperature|pH", names(all_data_wide))]
#maximum_columns <- names(all_data_wide)[grepl("Maximum_", names(all_data_wide)) & 
#                                  !grepl("Salinity|Temperature|pH|Ammonia_umol",names(all_data_wide))]
#mean_columns <- names(all_data_wide)[grepl("Mean_", names(all_data_wide)) & 
#                                  !grepl("Salinity|Temperature|pH|Ammonia_umol",names(all_data_wide))]
CV_columns <- names(all_data_wide)[grepl("CV_", names(all_data_wide), names(all_data_wide))]

mean_low_columns <- names(all_data_wide)[grepl("Low_Tide_Mean_", names(all_data_wide), names(all_data_wide))]

#dredge.data <- na.omit(all_data_wide[, c(maximum_columns, 
#                                    minimum_columns,
#                                    mean_columns, #removed bc not enough data for the amount of variables
#                                    CV_columns)])

PAC_model_data <- na.omit(all_data_wide %>% 
        filter(Species == "Pocillopora acuta"))

# Extract column names for filtering
selected_columns <- c("FSC.Events_per_cm_2", "Percent_Change", "Genotype", "Cage_Uncaged", 
                      CV_columns, mean_low_columns)

# Filter the data and select the relevant columns
PAC_model_data <- PAC_model_data[, selected_columns]

PAC_model_data$CV_Salinity_scaled <- scale(PAC_model_data$CV_Salinity, TRUE, TRUE)
PAC_model_data$CV_Temperature_scaled  <-  scale(PAC_model_data$CV_Temperature, TRUE, TRUE)
PAC_model_data$CV_TA_scaled  <-  scale(PAC_model_data$CV_TA, TRUE, TRUE)
PAC_model_data$CV_pH_scaled  <-  scale(PAC_model_data$CV_pH, TRUE, TRUE)
PAC_model_data$Low_Tide_Mean_Phosphate_umolL_scaled<-
  scale(PAC_model_data$Low_Tide_Mean_Phosphate_umolL, TRUE, TRUE)
PAC_model_data$Low_Tide_Mean_NN_umolL_scaled  <-  
  scale(PAC_model_data$Low_Tide_Mean_NN_umolL, TRUE, TRUE)
PAC_model_data$Low_Tide_Mean_Ammonia_umolL_scaled  <-
  scale(PAC_model_data$Low_Tide_Mean_Ammonia_umolL, TRUE, TRUE)
PAC_model_data$Low_Tide_Mean_NNP_umolL_scaled <- 
  scale(PAC_model_data$Low_Tide_Mean_Ammonia_umolL, TRUE, TRUE)
```

``` r
#PAC_cv_model <- glm(Percent_Change ~ ., data = PAC_cv_model_data, na.action = na.pass)
#the above line works if you only include response and explanatory in the dataframe, but dredge can't handle it with random effects

# Build the GLM model
#PAC_cv_model <- glm(Percent_Change ~ ., data = PAC_cv_model_data, na.action = na.pass)
PAC_cv_low_growth_model <- lmer(Percent_Change ~ 
                       CV_Salinity_scaled + 
                       CV_Temperature_scaled + 
                       CV_TA_scaled + 
                       CV_pH_scaled +
                       Low_Tide_Mean_Phosphate_umolL_scaled + 
                       Low_Tide_Mean_NN_umolL_scaled + 
                       Low_Tide_Mean_Ammonia_umolL_scaled +
                       Low_Tide_Mean_NNP_umolL_scaled +
                       (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_model_data, 
                                 na.action = na.pass)

PAC_cv_low_growth_model_dredge <- dredge(PAC_cv_low_growth_model, trace = 2)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

``` r
model.sel(PAC_cv_low_growth_model_dredge)
```

    ## Global model call: lmer(formula = Percent_Change ~ CV_Salinity_scaled + CV_Temperature_scaled + 
    ##     CV_TA_scaled + CV_pH_scaled + Low_Tide_Mean_Phosphate_umolL_scaled + 
    ##     Low_Tide_Mean_NN_umolL_scaled + Low_Tide_Mean_Ammonia_umolL_scaled + 
    ##     Low_Tide_Mean_NNP_umolL_scaled + (1 | Genotype) + (1 | Cage_Uncaged), 
    ##     data = PAC_model_data, na.action = na.pass)
    ## ---
    ## Model selection table 
    ##      (Int) CV_pH_scl CV_Sln_scl CV_TA_scl CV_Tmp_scl Low_Tid_Men_Amm_umL_scl
    ## 1   0.1048                                                                  
    ## 2   0.1041   0.02380                                                        
    ## 5   0.1067                       0.022640                                   
    ## 6   0.1061   0.02352             0.022220                                   
    ## 10  0.1044   0.02991                        0.018330                        
    ## 18  0.1040   0.02708                                               -0.017540
    ## 66  0.1040   0.02708                                                        
    ## 82  0.1040   0.02708                                               -0.017540
    ## 17  0.1050                                                         -0.013130
    ## 65  0.1050                                                                  
    ## 81  0.1050                                                         -0.013130
    ## 129 0.1047                                                                  
    ## 9   0.1051                                  0.010040                        
    ## 3   0.1046           -0.0093360                                             
    ## 14  0.1063   0.02971             0.022290   0.018020                        
    ## 22  0.1062   0.02692             0.023290                          -0.018390
    ## 70  0.1062   0.02692             0.023290                                   
    ## 86  0.1062   0.02692             0.023290                          -0.018390
    ## 34  0.1037   0.02925                                                        
    ## 33  0.1048                                                                  
    ## 145 0.1050                                                         -0.024260
    ## 193 0.1050                                                                  
    ## 209 0.1050                                                         -0.024260
    ## 4   0.1039   0.02327 -0.0067710                                             
    ## 130 0.1042   0.02214                                                        
    ## 21  0.1069                       0.023060                          -0.014130
    ## 69  0.1069                       0.023060                                   
    ## 85  0.1069                       0.023060                          -0.014130
    ## 38  0.1059   0.03000             0.023970                                   
    ## 13  0.1069                       0.022460   0.009567                        
    ## 146 0.1043   0.02248                                               -0.024090
    ## 194 0.1043   0.02248                                                        
    ## 210 0.1043   0.02248                                               -0.024090
    ## 7   0.1065           -0.0093220  0.022450                                   
    ## 133 0.1065                       0.020350                                   
    ## 26  0.1043   0.02998                        0.012940               -0.010300
    ## 74  0.1043   0.02998                        0.012940                        
    ## 90  0.1043   0.02998                        0.012940               -0.010300
    ## 138 0.1045   0.02723                        0.020110                        
    ## 12  0.1042   0.02949 -0.0088290             0.019040                        
    ## 134 0.1065   0.02650             0.027650                                   
    ## 37  0.1067                       0.022650                                   
    ## 137 0.1050                                  0.014660                        
    ## 131 0.1044           -0.0162600                                             
    ## 8   0.1060   0.02296 -0.0063270  0.022110                                   
    ## 42  0.1045   0.02926                        0.019730                        
    ## 50  0.1038   0.02961                                               -0.015800
    ## 114 0.1038   0.02961                                               -0.015800
    ## 98  0.1038   0.02961                                                        
    ## 20  0.1039   0.02683 -0.0035300                                    -0.017060
    ## 68  0.1039   0.02683 -0.0035300                                             
    ## 84  0.1039   0.02683 -0.0035300                                    -0.017060
    ## 41  0.1052                                  0.018510                        
    ## 161 0.1046                                                                  
    ## 19  0.1047           -0.0068530                                    -0.011900
    ## 67  0.1047           -0.0068530                                             
    ## 83  0.1047           -0.0068530                                    -0.011900
    ## 49  0.1050                                                         -0.015630
    ## 113 0.1050                                                         -0.015630
    ## 97  0.1050                                                                  
    ## 11  0.1049           -0.0109600             0.011210                        
    ## 25  0.1051                                  0.004739               -0.010390
    ## 73  0.1051                                  0.004739                        
    ## 89  0.1051                                  0.004739               -0.010390
    ## 162 0.1036   0.02787                                                        
    ## 30  0.1063   0.02976             0.022820   0.011780               -0.011750
    ## 78  0.1063   0.02976             0.022820   0.011780                        
    ## 94  0.1063   0.02976             0.022820   0.011780               -0.011750
    ## 147 0.1046           -0.0157800                                    -0.023770
    ## 195 0.1046           -0.0157800                                             
    ## 211 0.1046           -0.0157800                                    -0.023770
    ## 16  0.1061   0.02925 -0.0081420  0.021820   0.018640                        
    ## 35  0.1045           -0.0105800                                             
    ## 149 0.1063                       0.014200                          -0.021820
    ## 197 0.1063                       0.014200                                   
    ## 213 0.1063                       0.014200                          -0.021820
    ## 54  0.1061   0.03026             0.024150                          -0.016210
    ## 118 0.1061   0.03026             0.024150                          -0.016210
    ## 102 0.1061   0.03026             0.024150                                   
    ## 150 0.1060   0.02561             0.020660                          -0.020070
    ## 198 0.1060   0.02561             0.020660                                   
    ## 214 0.1060   0.02561             0.020660                          -0.020070
    ## 46  0.1063   0.02989             0.022450   0.017730                        
    ## 142 0.1065   0.03048             0.024070   0.017550                        
    ## 24  0.1061   0.02670 -0.0024960  0.023270                          -0.018010
    ## 72  0.1061   0.02670 -0.0024960  0.023270                                   
    ## 88  0.1061   0.02670 -0.0024960  0.023270                          -0.018010
    ## 139 0.1047           -0.0221500             0.019110                        
    ## 36  0.1037   0.02866 -0.0025900                                             
    ## 132 0.1039   0.01981 -0.0105700                                             
    ## 177 0.1050                                                         -0.023380
    ## 241 0.1050                                                         -0.023380
    ## 225 0.1050                                                                  
    ## 226 0.1038   0.02738                                                        
    ## 178 0.1038   0.02738                                               -0.021790
    ## 242 0.1038   0.02738                                               -0.021790
    ## 153 0.1051                                  0.005535               -0.021010
    ## 201 0.1051                                  0.005535                        
    ## 217 0.1051                                  0.005535               -0.021010
    ## 23  0.1067           -0.0065260  0.023070                          -0.012970
    ## 71  0.1067           -0.0065260  0.023070                                   
    ## 87  0.1067           -0.0065260  0.023070                          -0.012970
    ## 154 0.1045   0.02552                        0.012400               -0.017000
    ## 202 0.1045   0.02552                        0.012400                        
    ## 218 0.1045   0.02552                        0.012400               -0.017000
    ## 53  0.1069                       0.022720                          -0.016440
    ## 101 0.1069                       0.022720                                   
    ## 117 0.1069                       0.022720                          -0.016440
    ## 140 0.1042   0.02438 -0.0162200             0.022760                        
    ## 29  0.1069                       0.022890   0.003106               -0.012270
    ## 77  0.1069                       0.022890   0.003106                        
    ## 93  0.1069                       0.022890   0.003106               -0.012270
    ## 43  0.1048           -0.0235600             0.029670                        
    ## 45  0.1070                       0.021630   0.016880                        
    ## 15  0.1067           -0.0108900  0.021990   0.010640                        
    ## 148 0.1039   0.02046 -0.0104200                                    -0.023950
    ## 196 0.1039   0.02046 -0.0104200                                             
    ## 212 0.1039   0.02046 -0.0104200                                    -0.023950
    ## 141 0.1065                       0.016860   0.012280                        
    ## 135 0.1060           -0.0131600  0.017070                                   
    ## 166 0.1059   0.02997             0.023610                                   
    ## 44  0.1044   0.02599 -0.0145200             0.026330                        
    ## 40  0.1059   0.03001 -0.0002385  0.024010                                   
    ## 39  0.1065           -0.0102200  0.022280                                   
    ## 165 0.1063                       0.018690                                   
    ## 170 0.1043   0.02808                        0.017340                        
    ## 28  0.1041   0.02971 -0.0066730             0.014430               -0.008413
    ## 76  0.1041   0.02971 -0.0066730             0.014430                        
    ## 92  0.1041   0.02971 -0.0066730             0.014430               -0.008413
    ## 169 0.1050                                  0.015930                        
    ## 163 0.1044           -0.0152700                                             
    ## 106 0.1044   0.02928                        0.014410                        
    ## 58  0.1044   0.02928                        0.014410               -0.010310
    ## 122 0.1044   0.02928                        0.014410               -0.010310
    ## 136 0.1063   0.02554 -0.0033410  0.026310                                   
    ## 105 0.1052                                  0.013050                        
    ## 57  0.1052                                  0.013050               -0.010600
    ## 121 0.1052                                  0.013050               -0.010600
    ## 99  0.1046           -0.0093500                                             
    ## 51  0.1046           -0.0093500                                    -0.014790
    ## 115 0.1046           -0.0093500                                    -0.014790
    ## 52  0.1037   0.02928 -0.0018850                                    -0.015730
    ## 116 0.1037   0.02928 -0.0018850                                    -0.015730
    ## 100 0.1037   0.02928 -0.0018850                                             
    ## 27  0.1049           -0.0088200             0.007155               -0.007423
    ## 75  0.1049           -0.0088200             0.007155                        
    ## 91  0.1049           -0.0088200             0.007155               -0.007423
    ## 155 0.1048           -0.0193400             0.011090               -0.017460
    ## 203 0.1048           -0.0193400             0.011090                        
    ## 219 0.1048           -0.0193400             0.011090               -0.017460
    ## 151 0.1057           -0.0135500  0.011140                          -0.021890
    ## 199 0.1057           -0.0135500  0.011140                                   
    ## 215 0.1057           -0.0135500  0.011140                          -0.021890
    ## 164 0.1035   0.02603 -0.0063820                                             
    ## 158 0.1061   0.02857             0.020340   0.011730               -0.013320
    ## 206 0.1061   0.02857             0.020340   0.011730                        
    ## 222 0.1061   0.02857             0.020340   0.011730               -0.013320
    ## 32  0.1062   0.02949 -0.0055250  0.022560   0.013050               -0.010190
    ## 80  0.1062   0.02949 -0.0055250  0.022560   0.013050                        
    ## 96  0.1062   0.02949 -0.0055250  0.022560   0.013050               -0.010190
    ## 47  0.1066           -0.0211500  0.019880   0.026710                        
    ## 110 0.1063   0.02997             0.022990   0.011500                        
    ## 62  0.1063   0.02997             0.022990   0.011500               -0.011720
    ## 126 0.1063   0.02997             0.022990   0.011500               -0.011720
    ## 227 0.1046           -0.0152800                                             
    ## 179 0.1046           -0.0152800                                    -0.023210
    ## 243 0.1046           -0.0152800                                    -0.023210
    ## 171 0.1047           -0.0265400             0.027610                        
    ## 230 0.1054   0.02881             0.017370                                   
    ## 182 0.1054   0.02881             0.017370                          -0.019070
    ## 246 0.1054   0.02881             0.017370                          -0.019070
    ## 48  0.1062   0.02679 -0.0121300  0.020840   0.023550                        
    ## 144 0.1059   0.02796 -0.0101200  0.019160   0.019620                        
    ## 143 0.1057           -0.0196100  0.010510   0.017070                        
    ## 229 0.1062                       0.013300                                   
    ## 181 0.1062                       0.013300                          -0.021580
    ## 245 0.1062                       0.013300                          -0.021580
    ## 157 0.1062                       0.013600   0.004190               -0.019400
    ## 205 0.1062                       0.013600   0.004190                        
    ## 221 0.1062                       0.013600   0.004190               -0.019400
    ## 156 0.1042   0.02331 -0.0143100             0.015600               -0.015000
    ## 204 0.1042   0.02331 -0.0143100             0.015600                        
    ## 220 0.1042   0.02331 -0.0143100             0.015600               -0.015000
    ## 152 0.1057   0.02425 -0.0052240  0.018820                          -0.020380
    ## 200 0.1057   0.02425 -0.0052240  0.018820                                   
    ## 216 0.1057   0.02425 -0.0052240  0.018820                          -0.020380
    ## 174 0.1065   0.03036             0.024470   0.018120                        
    ## 56  0.1061   0.03039  0.0001276  0.024230                          -0.016190
    ## 104 0.1061   0.03039  0.0001276  0.024230                                   
    ## 120 0.1061   0.03039  0.0001276  0.024230                          -0.016190
    ## 233 0.1050                                  0.003002                        
    ## 185 0.1050                                  0.003002               -0.021830
    ## 249 0.1050                                  0.003002               -0.021830
    ## 180 0.1036   0.02541 -0.0075240                                    -0.022090
    ## 228 0.1036   0.02541 -0.0075240                                             
    ## 244 0.1036   0.02541 -0.0075240                                    -0.022090
    ## 234 0.1040   0.02738                        0.005406                        
    ## 186 0.1040   0.02738                        0.005406               -0.019230
    ## 250 0.1040   0.02738                        0.005406               -0.019230
    ## 61  0.1070                       0.021950   0.010140               -0.012320
    ## 109 0.1070                       0.021950   0.010140                        
    ## 125 0.1070                       0.021950   0.010140               -0.012320
    ## 55  0.1067           -0.0085340  0.022730                          -0.015540
    ## 119 0.1067           -0.0085340  0.022730                          -0.015540
    ## 103 0.1067           -0.0085340  0.022730                                   
    ## 31  0.1068           -0.0081040  0.022650   0.005382               -0.009565
    ## 79  0.1068           -0.0081040  0.022650   0.005382                        
    ## 95  0.1068           -0.0081040  0.022650   0.005382               -0.009565
    ## 172 0.1043   0.02347 -0.0178200             0.025190                        
    ## 173 0.1068                       0.019240   0.016400                        
    ## 59  0.1048           -0.0222100             0.027090               -0.003641
    ## 123 0.1048           -0.0222100             0.027090               -0.003641
    ## 107 0.1048           -0.0222100             0.027090                        
    ## 167 0.1059           -0.0129500  0.015730                                   
    ## 60  0.1043   0.02666 -0.0119100             0.021490               -0.006869
    ## 108 0.1043   0.02666 -0.0119100             0.021490                        
    ## 124 0.1043   0.02666 -0.0119100             0.021490               -0.006869
    ## 168 0.1059   0.02990 -0.0004359  0.023500                                   
    ## 235 0.1047           -0.0220300             0.016450                        
    ## 187 0.1047           -0.0220300             0.016450               -0.015160
    ## 251 0.1047           -0.0220300             0.016450               -0.015160
    ## 159 0.1056           -0.0172700  0.008811   0.009772               -0.016710
    ## 207 0.1056           -0.0172700  0.008811   0.009772                        
    ## 223 0.1056           -0.0172700  0.008811   0.009772               -0.016710
    ## 175 0.1060           -0.0238000  0.013220   0.026500                        
    ## 160 0.1057   0.02642 -0.0094250  0.016470   0.013930               -0.012810
    ## 208 0.1057   0.02642 -0.0094250  0.016470   0.013930                        
    ## 224 0.1057   0.02642 -0.0094250  0.016470   0.013930               -0.012810
    ## 231 0.1056           -0.0135600  0.010270                                   
    ## 183 0.1056           -0.0135600  0.010270                          -0.021760
    ## 247 0.1056           -0.0135600  0.010270                          -0.021760
    ## 238 0.1059   0.02920             0.019280   0.009188                        
    ## 190 0.1059   0.02920             0.019280   0.009188               -0.014320
    ## 254 0.1059   0.02920             0.019280   0.009188               -0.014320
    ## 64  0.1062   0.02782 -0.0083750  0.021870   0.016800               -0.009332
    ## 112 0.1062   0.02782 -0.0083750  0.021870   0.016800                        
    ## 128 0.1062   0.02782 -0.0083750  0.021870   0.016800               -0.009332
    ## 111 0.1067           -0.0187900  0.020470   0.022280                        
    ## 63  0.1067           -0.0187900  0.020470   0.022280               -0.006119
    ## 127 0.1067           -0.0187900  0.020470   0.022280               -0.006119
    ## 176 0.1061   0.02652 -0.0125500  0.019960   0.023530                        
    ## 184 0.1052   0.02776 -0.0035650  0.016030                          -0.019410
    ## 232 0.1052   0.02776 -0.0035650  0.016030                                   
    ## 248 0.1052   0.02776 -0.0035650  0.016030                          -0.019410
    ## 189 0.1062                       0.013840   0.004849               -0.018910
    ## 237 0.1062                       0.013840   0.004849                        
    ## 253 0.1062                       0.013840   0.004849               -0.018910
    ## 236 0.1040   0.02404 -0.0131800             0.013360                        
    ## 188 0.1040   0.02404 -0.0131800             0.013360               -0.015830
    ## 252 0.1040   0.02404 -0.0131800             0.013360               -0.015830
    ## 191 0.1058           -0.0203900  0.010520   0.016900               -0.013340
    ## 239 0.1058           -0.0203900  0.010520   0.016900                        
    ## 255 0.1058           -0.0203900  0.010520   0.016900               -0.013340
    ## 192 0.1057   0.02638 -0.0097540  0.016770   0.014610               -0.012460
    ## 256 0.1057   0.02638 -0.0097540  0.016770   0.014610               -0.012460
    ## 240 0.1057   0.02638 -0.0097540  0.016770   0.014610                        
    ##     Low_Tid_Men_NN_umL_scl Low_Tid_Men_NNP_umL_scl Low_Tid_Men_Phs_umL_scl df
    ## 1                                                                           4
    ## 2                                                                           5
    ## 5                                                                           5
    ## 6                                                                           6
    ## 10                                                                          6
    ## 18                                                                          6
    ## 66                                                                          6
    ## 82                                               +                          6
    ## 17                                                                          5
    ## 65                                                                          5
    ## 81                                               +                          5
    ## 129                                                              0.0122300  5
    ## 9                                                                           5
    ## 3                                                                           5
    ## 14                                                                          7
    ## 22                                                                          7
    ## 70                                                                          7
    ## 86                                               +                          7
    ## 34              -0.0115600                                                  6
    ## 33               0.0008557                                                  5
    ## 145                                                              0.0232900  6
    ## 193                                                              0.0232900  6
    ## 209                                              +               0.0232900  6
    ## 4                                                                           6
    ## 130                                                              0.0046230  6
    ## 21                                                                          6
    ## 69                                                                          6
    ## 85                                               +                          6
    ## 38              -0.0134400                                                  7
    ## 13                                                                          6
    ## 146                                                              0.0156900  7
    ## 194                                                              0.0156900  7
    ## 210                                              +               0.0156900  7
    ## 7                                                                           6
    ## 133                                                              0.0038770  6
    ## 26                                                                          7
    ## 74                                                                          7
    ## 90                                               +                          7
    ## 138                                                              0.0092230  7
    ## 12                                                                          7
    ## 134                                                             -0.0081840  7
    ## 37              -0.0001824                                                  6
    ## 137                                                              0.0169700  6
    ## 131                                                              0.0171800  6
    ## 8                                                                           7
    ## 42               0.0025070                                                  7
    ## 50              -0.0058140                                                  7
    ## 114             -0.0058140                       +                          7
    ## 98              -0.0058140                                                  7
    ## 20                                                                          7
    ## 68                                                                          7
    ## 84                                               +                          7
    ## 41               0.0135500                                                  6
    ## 161             -0.0101800                                       0.0185300  6
    ## 19                                                                          6
    ## 67                                                                          6
    ## 83                                               +                          6
    ## 49               0.0065560                                                  6
    ## 113              0.0065560                       +                          6
    ## 97               0.0065560                                                  6
    ## 11                                                                          6
    ## 25                                                                          6
    ## 73                                                                          6
    ## 89                                               +                          6
    ## 162             -0.0202500                                       0.0150900  7
    ## 30                                                                          8
    ## 78                                                                          8
    ## 94                                               +                          8
    ## 147                                                              0.0281200  7
    ## 195                                                              0.0281200  7
    ## 211                                              +               0.0281200  7
    ## 16                                                                          8
    ## 35               0.0036000                                                  6
    ## 149                                                              0.0163800  7
    ## 197                                                              0.0163800  7
    ## 213                                              +               0.0163800  7
    ## 54              -0.0073760                                                  8
    ## 118             -0.0073760                       +                          8
    ## 102             -0.0073760                                                  8
    ## 150                                                              0.0043090  8
    ## 198                                                              0.0043090  8
    ## 214                                              +               0.0043090  8
    ## 46              -0.0005734                                                  8
    ## 142                                                             -0.0024810  8
    ## 24                                                                          8
    ## 72                                                                          8
    ## 88                                               +                          8
    ## 139                                                              0.0248800  7
    ## 36              -0.0106700                                                  7
    ## 132                                                              0.0086420  7
    ## 177             -0.0064130                                       0.0267800  7
    ## 241             -0.0064130                       +               0.0267800  7
    ## 225             -0.0064130                                       0.0267800  7
    ## 226             -0.0163100                                       0.0227300  8
    ## 178             -0.0163100                                       0.0227300  8
    ## 242             -0.0163100                       +               0.0227300  8
    ## 153                                                              0.0234500  7
    ## 201                                                              0.0234500  7
    ## 217                                              +               0.0234500  7
    ## 23                                                                          7
    ## 71                                                                          7
    ## 87                                               +                          7
    ## 154                                                              0.0151800  8
    ## 202                                                              0.0151800  8
    ## 218                                              +               0.0151800  8
    ## 53               0.0059320                                                  7
    ## 101              0.0059320                                                  7
    ## 117              0.0059320                       +                          7
    ## 140                                                              0.0159200  8
    ## 29                                                                          7
    ## 77                                                                          7
    ## 93                                               +                          7
    ## 43               0.0277100                                                  7
    ## 45               0.0114700                                                  7
    ## 15                                                                          7
    ## 148                                                              0.0193500  8
    ## 196                                                              0.0193500  8
    ## 212                                              +               0.0193500  8
    ## 141                                                              0.0092590  7
    ## 135                                                              0.0093100  7
    ## 166             -0.0138700                                       0.0006819  8
    ## 44               0.0121600                                                  8
    ## 40              -0.0134400                                                  8
    ## 39               0.0025510                                                  7
    ## 165             -0.0043370                                       0.0072350  7
    ## 170             -0.0057690                                       0.0116700  8
    ## 28                                                                          8
    ## 76                                                                          8
    ## 92                                               +                          8
    ## 169              0.0024940                                       0.0158700  7
    ## 163             -0.0081220                                       0.0218100  7
    ## 106              0.0026300                                                  8
    ## 58               0.0026300                                                  8
    ## 122              0.0026300                       +                          8
    ## 136                                                             -0.0062990  8
    ## 105              0.0135500                                                  7
    ## 57               0.0135500                                                  7
    ## 121              0.0135500                       +                          7
    ## 99               0.0086930                                                  7
    ## 51               0.0086930                                                  7
    ## 115              0.0086930                       +                          7
    ## 52              -0.0052860                                                  8
    ## 116             -0.0052860                       +                          8
    ## 100             -0.0052860                                                  8
    ## 27                                                                          7
    ## 75                                                                          7
    ## 91                                               +                          7
    ## 155                                                              0.0296700  8
    ## 203                                                              0.0296700  8
    ## 219                                              +               0.0296700  8
    ## 151                                                              0.0221900  8
    ## 199                                                              0.0221900  8
    ## 215                                              +               0.0221900  8
    ## 164             -0.0185300                                       0.0164900  8
    ## 158                                                              0.0039210  9
    ## 206                                                              0.0039210  9
    ## 222                                              +               0.0039210  9
    ## 32                                                                          9
    ## 80                                                                          9
    ## 96                                               +                          9
    ## 47               0.0237800                                                  8
    ## 110             -0.0005767                                                  9
    ## 62              -0.0005767                                                  9
    ## 126             -0.0005767                       +                          9
    ## 227             -0.0047880                                       0.0306000  8
    ## 179             -0.0047880                                       0.0306000  8
    ## 243             -0.0047880                       +               0.0306000  8
    ## 171              0.0152000                                       0.0196900  8
    ## 230             -0.0119100                                       0.0111500  9
    ## 182             -0.0119100                                       0.0111500  9
    ## 246             -0.0119100                       +               0.0111500  9
    ## 48               0.0082440                                                  9
    ## 144                                                              0.0040240  9
    ## 143                                                              0.0191600  8
    ## 229             -0.0025130                                       0.0181500  8
    ## 181             -0.0025130                                       0.0181500  8
    ## 245             -0.0025130                       +               0.0181500  8
    ## 157                                                              0.0167300  8
    ## 205                                                              0.0167300  8
    ## 221                                              +               0.0167300  8
    ## 156                                                              0.0203600  9
    ## 204                                                              0.0203600  9
    ## 220                                              +               0.0203600  9
    ## 152                                                              0.0072300  9
    ## 200                                                              0.0072300  9
    ## 216                                              +               0.0072300  9
    ## 174              0.0012370                                      -0.0031770  9
    ## 56              -0.0075530                                                  9
    ## 104             -0.0075530                                                  9
    ## 120             -0.0075530                       +                          9
    ## 233             -0.0043260                                       0.0257200  8
    ## 185             -0.0043260                                       0.0257200  8
    ## 249             -0.0043260                       +               0.0257200  8
    ## 180             -0.0145300                                       0.0246100  9
    ## 228             -0.0145300                                       0.0246100  9
    ## 244             -0.0145300                       +               0.0246100  9
    ## 234             -0.0121900                                       0.0208000  9
    ## 186             -0.0121900                                       0.0208000  9
    ## 250             -0.0121900                       +               0.0208000  9
    ## 61               0.0112500                                                  8
    ## 109              0.0112500                                                  8
    ## 125              0.0112500                       +                          8
    ## 55               0.0077870                                                  8
    ## 119              0.0077870                       +                          8
    ## 103              0.0077870                                                  8
    ## 31                                                                          8
    ## 79                                                                          8
    ## 95                                               +                          8
    ## 172              0.0045480                                       0.0146900  9
    ## 173              0.0086970                                       0.0043470  8
    ## 59               0.0268600                                                  8
    ## 123              0.0268600                       +                          8
    ## 107              0.0268600                                                  8
    ## 167             -0.0035200                                       0.0119100  8
    ## 60               0.0103800                                                  9
    ## 108              0.0103800                                                  9
    ## 124              0.0103800                       +                          9
    ## 168             -0.0138800                                       0.0008739  9
    ## 235              0.0078160                                       0.0263200  9
    ## 187              0.0078160                                       0.0263200  9
    ## 251              0.0078160                       +               0.0263200  9
    ## 159                                                              0.0247400  9
    ## 207                                                              0.0247400  9
    ## 223                                              +               0.0247400  9
    ## 175              0.0177800                                       0.0115500  9
    ## 160                                                              0.0095600 10
    ## 208                                                              0.0095600 10
    ## 224                                              +               0.0095600 10
    ## 231             -0.0021240                                       0.0237200  9
    ## 183             -0.0021240                                       0.0237200  9
    ## 247             -0.0021240                       +               0.0237200  9
    ## 238             -0.0046730                                       0.0066120 10
    ## 190             -0.0046730                                       0.0066120 10
    ## 254             -0.0046730                       +               0.0066120 10
    ## 64               0.0054910                                                 10
    ## 112              0.0054910                                                 10
    ## 128              0.0054910                       +                         10
    ## 111              0.0222700                                                  9
    ## 63               0.0222700                                                  9
    ## 127              0.0222700                       +                          9
    ## 176              0.0075600                                       0.0015360 10
    ## 184             -0.0114300                                       0.0129500 10
    ## 232             -0.0114300                                       0.0129500 10
    ## 248             -0.0114300                       +               0.0129500 10
    ## 189              0.0009409                                       0.0160800  9
    ## 237              0.0009409                                       0.0160800  9
    ## 253              0.0009409                       +               0.0160800  9
    ## 236             -0.0033220                                       0.0214100 10
    ## 188             -0.0033220                                       0.0214100 10
    ## 252             -0.0033220                       +               0.0214100 10
    ## 191              0.0107200                                       0.0191000 10
    ## 239              0.0107200                                       0.0191000 10
    ## 255              0.0107200                       +               0.0191000 10
    ## 192              0.0010400                                       0.0089280 11
    ## 256              0.0010400                       +               0.0089280 11
    ## 240              0.0010400                                       0.0089280 11
    ##     logLik   AICc delta weight
    ## 1   67.909 -126.9  0.00  0.491
    ## 2   68.420 -125.4  1.48  0.235
    ## 5   66.916 -122.4  4.48  0.052
    ## 6   67.757 -121.5  5.42  0.033
    ## 10  67.690 -121.3  5.56  0.031
    ## 18  67.226 -120.4  6.48  0.019
    ## 66  67.226 -120.4  6.48  0.019
    ## 82  67.226 -120.4  6.48  0.019
    ## 17  65.296 -119.2  7.72  0.010
    ## 65  65.296 -119.2  7.72  0.010
    ## 81  65.296 -119.2  7.72  0.010
    ## 129 65.139 -118.9  8.04  0.009
    ## 9   64.809 -118.2  8.70  0.006
    ## 3   64.648 -117.9  9.02  0.005
    ## 14  67.265 -117.7  9.16  0.005
    ## 22  67.215 -117.6  9.26  0.005
    ## 70  67.215 -117.6  9.26  0.005
    ## 86  67.215 -117.6  9.26  0.005
    ## 34  65.475 -116.9  9.99  0.003
    ## 33  64.018 -116.6 10.28  0.003
    ## 145 64.979 -115.9 10.98  0.002
    ## 193 64.979 -115.9 10.98  0.002
    ## 209 64.979 -115.9 10.98  0.002
    ## 4   64.884 -115.7 11.17  0.002
    ## 130 64.675 -115.3 11.59  0.001
    ## 21  64.606 -115.2 11.72  0.001
    ## 69  64.606 -115.2 11.72  0.001
    ## 85  64.606 -115.2 11.72  0.001
    ## 38  65.191 -113.6 13.31  0.001
    ## 13  63.768 -113.5 13.40  0.001
    ## 146 65.132 -113.5 13.42  0.001
    ## 194 65.132 -113.5 13.42  0.001
    ## 210 65.132 -113.5 13.42  0.001
    ## 7   63.642 -113.2 13.65  0.001
    ## 133 63.215 -112.4 14.51  0.000
    ## 26  64.577 -112.4 14.53  0.000
    ## 74  64.577 -112.4 14.53  0.000
    ## 90  64.577 -112.4 14.53  0.000
    ## 138 64.460 -112.1 14.77  0.000
    ## 12  64.379 -112.0 14.93  0.000
    ## 134 64.352 -111.9 14.98  0.000
    ## 37  62.973 -111.9 14.99  0.000
    ## 137 62.958 -111.9 15.02  0.000
    ## 131 62.808 -111.6 15.32  0.000
    ## 8   64.158 -111.5 15.37  0.000
    ## 42  64.006 -111.2 15.68  0.000
    ## 50  63.574 -110.3 16.54  0.000
    ## 114 63.574 -110.3 16.54  0.000
    ## 98  63.574 -110.3 16.54  0.000
    ## 20  63.438 -110.1 16.81  0.000
    ## 68  63.438 -110.1 16.81  0.000
    ## 84  63.438 -110.1 16.81  0.000
    ## 41  61.990 -109.9 16.96  0.000
    ## 161 61.964 -109.9 17.01  0.000
    ## 19  61.795 -109.5 17.35  0.000
    ## 67  61.795 -109.5 17.35  0.000
    ## 83  61.795 -109.5 17.35  0.000
    ## 49  61.747 -109.4 17.44  0.000
    ## 113 61.747 -109.4 17.44  0.000
    ## 97  61.747 -109.4 17.44  0.000
    ## 11  61.741 -109.4 17.45  0.000
    ## 25  61.656 -109.3 17.63  0.000
    ## 73  61.656 -109.3 17.63  0.000
    ## 89  61.656 -109.3 17.63  0.000
    ## 162 63.030 -109.3 17.63  0.000
    ## 30  64.441 -109.2 17.70  0.000
    ## 78  64.441 -109.2 17.70  0.000
    ## 94  64.441 -109.2 17.70  0.000
    ## 147 62.703 -108.6 18.28  0.000
    ## 195 62.703 -108.6 18.28  0.000
    ## 211 62.703 -108.6 18.28  0.000
    ## 16  63.869 -108.0 18.84  0.000
    ## 35  60.890 -107.7 19.16  0.000
    ## 149 62.263 -107.7 19.16  0.000
    ## 197 62.263 -107.7 19.16  0.000
    ## 213 62.263 -107.7 19.16  0.000
    ## 54  63.663 -107.6 19.25  0.000
    ## 118 63.663 -107.6 19.25  0.000
    ## 102 63.663 -107.6 19.25  0.000
    ## 150 63.572 -107.5 19.44  0.000
    ## 198 63.572 -107.5 19.44  0.000
    ## 214 63.572 -107.5 19.44  0.000
    ## 46  63.501 -107.3 19.58  0.000
    ## 142 63.478 -107.3 19.63  0.000
    ## 24  63.317 -106.9 19.95  0.000
    ## 72  63.317 -106.9 19.95  0.000
    ## 88  63.317 -106.9 19.95  0.000
    ## 139 61.825 -106.8 20.04  0.000
    ## 36  61.756 -106.7 20.18  0.000
    ## 132 61.550 -106.3 20.59  0.000
    ## 177 61.457 -106.1 20.77  0.000
    ## 241 61.457 -106.1 20.77  0.000
    ## 225 61.457 -106.1 20.77  0.000
    ## 226 62.896 -106.1 20.79  0.000
    ## 178 62.896 -106.1 20.79  0.000
    ## 242 62.896 -106.1 20.79  0.000
    ## 153 61.343 -105.9 21.00  0.000
    ## 201 61.343 -105.9 21.00  0.000
    ## 217 61.343 -105.9 21.00  0.000
    ## 23  61.055 -105.3 21.58  0.000
    ## 71  61.055 -105.3 21.58  0.000
    ## 87  61.055 -105.3 21.58  0.000
    ## 154 62.424 -105.2 21.73  0.000
    ## 202 62.424 -105.2 21.73  0.000
    ## 218 62.424 -105.2 21.73  0.000
    ## 53  60.973 -105.1 21.74  0.000
    ## 101 60.973 -105.1 21.74  0.000
    ## 117 60.973 -105.1 21.74  0.000
    ## 140 62.325 -105.0 21.93  0.000
    ## 29  60.841 -104.9 22.01  0.000
    ## 77  60.841 -104.9 22.01  0.000
    ## 93  60.841 -104.9 22.01  0.000
    ## 43  60.740 -104.7 22.21  0.000
    ## 45  60.726 -104.7 22.24  0.000
    ## 15  60.685 -104.6 22.32  0.000
    ## 148 61.996 -104.3 22.59  0.000
    ## 196 61.996 -104.3 22.59  0.000
    ## 212 61.996 -104.3 22.59  0.000
    ## 141 60.497 -104.2 22.69  0.000
    ## 135 60.386 -104.0 22.92  0.000
    ## 166 61.606 -103.5 23.37  0.000
    ## 44  61.387 -103.1 23.81  0.000
    ## 40  61.378 -103.1 23.82  0.000
    ## 39  59.795 -102.8 24.10  0.000
    ## 165 59.651 -102.5 24.39  0.000
    ## 170 61.022 -102.4 24.54  0.000
    ## 28  61.020 -102.3 24.54  0.000
    ## 76  61.020 -102.3 24.54  0.000
    ## 92  61.020 -102.3 24.54  0.000
    ## 169 59.519 -102.2 24.65  0.000
    ## 163 59.446 -102.1 24.80  0.000
    ## 106 60.890 -102.1 24.80  0.000
    ## 58  60.890 -102.1 24.80  0.000
    ## 122 60.890 -102.1 24.80  0.000
    ## 136 60.666 -101.6 25.25  0.000
    ## 105 58.850 -100.9 25.99  0.000
    ## 57  58.850 -100.9 25.99  0.000
    ## 121 58.850 -100.9 25.99  0.000
    ## 99  58.467 -100.1 26.75  0.000
    ## 51  58.467 -100.1 26.75  0.000
    ## 115 58.467 -100.1 26.75  0.000
    ## 52  59.802  -99.9 26.98  0.000
    ## 116 59.802  -99.9 26.98  0.000
    ## 100 59.802  -99.9 26.98  0.000
    ## 27  58.342  -99.9 27.00  0.000
    ## 75  58.342  -99.9 27.00  0.000
    ## 91  58.342  -99.9 27.00  0.000
    ## 155 59.684  -99.7 27.21  0.000
    ## 203 59.684  -99.7 27.21  0.000
    ## 219 59.684  -99.7 27.21  0.000
    ## 151 59.590  -99.5 27.40  0.000
    ## 199 59.590  -99.5 27.40  0.000
    ## 215 59.590  -99.5 27.40  0.000
    ## 164 59.509  -99.3 27.56  0.000
    ## 158 60.773  -98.8 28.08  0.000
    ## 206 60.773  -98.8 28.08  0.000
    ## 222 60.773  -98.8 28.08  0.000
    ## 32  60.753  -98.8 28.12  0.000
    ## 80  60.753  -98.8 28.12  0.000
    ## 96  60.753  -98.8 28.12  0.000
    ## 47  59.210  -98.7 28.16  0.000
    ## 110 60.658  -98.6 28.31  0.000
    ## 62  60.658  -98.6 28.31  0.000
    ## 126 60.658  -98.6 28.31  0.000
    ## 227 59.070  -98.4 28.44  0.000
    ## 179 59.070  -98.4 28.44  0.000
    ## 243 59.070  -98.4 28.44  0.000
    ## 171 59.064  -98.4 28.45  0.000
    ## 230 60.585  -98.4 28.46  0.000
    ## 182 60.585  -98.4 28.46  0.000
    ## 246 60.585  -98.4 28.46  0.000
    ## 48  60.556  -98.4 28.51  0.000
    ## 144 60.288  -97.8 29.05  0.000
    ## 143 58.681  -97.7 29.22  0.000
    ## 229 58.582  -97.5 29.42  0.000
    ## 181 58.582  -97.5 29.42  0.000
    ## 245 58.582  -97.5 29.42  0.000
    ## 157 58.533  -97.4 29.51  0.000
    ## 205 58.533  -97.4 29.51  0.000
    ## 221 58.533  -97.4 29.51  0.000
    ## 156 59.936  -97.1 29.75  0.000
    ## 204 59.936  -97.1 29.75  0.000
    ## 220 59.936  -97.1 29.75  0.000
    ## 152 59.929  -97.1 29.77  0.000
    ## 200 59.929  -97.1 29.77  0.000
    ## 216 59.929  -97.1 29.77  0.000
    ## 174 59.927  -97.1 29.77  0.000
    ## 56  59.798  -96.9 30.03  0.000
    ## 104 59.798  -96.9 30.03  0.000
    ## 120 59.798  -96.9 30.03  0.000
    ## 233 57.932  -96.2 30.72  0.000
    ## 185 57.932  -96.2 30.72  0.000
    ## 249 57.932  -96.2 30.72  0.000
    ## 180 59.441  -96.1 30.74  0.000
    ## 228 59.441  -96.1 30.74  0.000
    ## 244 59.441  -96.1 30.74  0.000
    ## 234 59.368  -96.0 30.89  0.000
    ## 186 59.368  -96.0 30.89  0.000
    ## 250 59.368  -96.0 30.89  0.000
    ## 61  57.785  -95.9 31.01  0.000
    ## 109 57.785  -95.9 31.01  0.000
    ## 125 57.785  -95.9 31.01  0.000
    ## 55  57.623  -95.6 31.33  0.000
    ## 119 57.623  -95.6 31.33  0.000
    ## 103 57.623  -95.6 31.33  0.000
    ## 31  57.449  -95.2 31.68  0.000
    ## 79  57.449  -95.2 31.68  0.000
    ## 95  57.449  -95.2 31.68  0.000
    ## 172 58.916  -95.1 31.79  0.000
    ## 173 57.293  -94.9 32.00  0.000
    ## 59  57.115  -94.5 32.35  0.000
    ## 123 57.115  -94.5 32.35  0.000
    ## 107 57.115  -94.5 32.35  0.000
    ## 167 56.784  -93.9 33.01  0.000
    ## 60  57.885  -93.0 33.85  0.000
    ## 108 57.885  -93.0 33.85  0.000
    ## 124 57.885  -93.0 33.85  0.000
    ## 168 57.878  -93.0 33.87  0.000
    ## 235 56.446  -90.2 36.73  0.000
    ## 187 56.446  -90.2 36.73  0.000
    ## 251 56.446  -90.2 36.73  0.000
    ## 159 56.386  -90.0 36.85  0.000
    ## 207 56.386  -90.0 36.85  0.000
    ## 223 56.386  -90.0 36.85  0.000
    ## 175 56.168  -89.6 37.29  0.000
    ## 160 57.523  -89.1 37.79  0.000
    ## 208 57.523  -89.1 37.79  0.000
    ## 224 57.523  -89.1 37.79  0.000
    ## 231 55.892  -89.0 37.84  0.000
    ## 183 55.892  -89.0 37.84  0.000
    ## 247 55.892  -89.0 37.84  0.000
    ## 238 57.322  -88.7 38.19  0.000
    ## 190 57.322  -88.7 38.19  0.000
    ## 254 57.322  -88.7 38.19  0.000
    ## 64  57.307  -88.7 38.22  0.000
    ## 112 57.307  -88.7 38.22  0.000
    ## 128 57.307  -88.7 38.22  0.000
    ## 111 55.682  -88.6 38.26  0.000
    ## 63  55.682  -88.6 38.26  0.000
    ## 127 55.682  -88.6 38.26  0.000
    ## 176 56.988  -88.0 38.86  0.000
    ## 184 56.871  -87.8 39.09  0.000
    ## 232 56.871  -87.8 39.09  0.000
    ## 248 56.871  -87.8 39.09  0.000
    ## 189 55.108  -87.5 39.41  0.000
    ## 237 55.108  -87.5 39.41  0.000
    ## 253 55.108  -87.5 39.41  0.000
    ## 236 56.535  -87.1 39.76  0.000
    ## 188 56.535  -87.1 39.76  0.000
    ## 252 56.535  -87.1 39.76  0.000
    ## 191 53.306  -80.7 46.22  0.000
    ## 239 53.306  -80.7 46.22  0.000
    ## 255 53.306  -80.7 46.22  0.000
    ## 192 54.097  -78.9 48.03  0.000
    ## 256 54.097  -78.9 48.03  0.000
    ## 240 54.097  -78.9 48.03  0.000
    ## Models ranked by AICc(x) 
    ## Random terms (all models): 
    ##   1 | Genotype, 1 | Cage_Uncaged

``` r
PAC.avg_cv_low_growth_model = model.avg(PAC_cv_low_growth_model_dredge, delta < 5)
summary(PAC.avg_cv_low_growth_model)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_cv_low_growth_model_dredge, subset = delta < 
    ##     5)
    ## 
    ## Component model call: 
    ## lmer(formula = Percent_Change ~ <3 unique rhs>, data = PAC_model_data, 
    ##      na.action = na.pass)
    ## 
    ## Component models: 
    ##        df logLik    AICc delta weight
    ## (Null)  4  67.91 -126.89  0.00   0.63
    ## 1       5  68.42 -125.41  1.48   0.30
    ## 2       5  66.92 -122.40  4.48   0.07
    ## 
    ## Term codes: 
    ## CV_pH_scaled CV_TA_scaled 
    ##            1            2 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##              Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)  0.104707   0.013387    0.013768   7.605   <2e-16 ***
    ## CV_pH_scaled 0.007179   0.011691    0.011735   0.612    0.541    
    ## CV_TA_scaled 0.001518   0.006171    0.006199   0.245    0.807    
    ##  
    ## (conditional average) 
    ##              Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)  0.104707   0.013387    0.013768   7.605  < 2e-16 ***
    ## CV_pH_scaled 0.023802   0.007585    0.007805   3.050  0.00229 ** 
    ## CV_TA_scaled 0.022640   0.009468    0.009742   2.324  0.02012 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Make the effects plots for the model with the most support:

``` r
PAC_growth_cv_plus_lowtide_model <- lmer(Percent_Change ~ 
#                       CV_Salinity_scaled + 
#                       CV_Temperature_scaled + 
                       CV_TA_scaled + 
                       CV_pH_scaled +
                       (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_model_data, 
                                 na.action = na.pass)
plot(allEffects(PAC_growth_cv_plus_lowtide_model))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictors CV_TA_scaled, CV_pH_scaled are one-column matrices that were
    ## converted to vectors

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictors CV_TA_scaled, CV_pH_scaled are one-column matrices that were
    ## converted to vectors

![](analysis_files/figure-gfm/Growth%20CV%20+%20Low%20tide%20best%20model%20effects%20plots-1.png)<!-- -->

### Symbiont Count Models

``` r
# Build the GLM model
#PAC_cv_model <- glm(Percent_Change ~ ., data = PAC_cv_model_data, na.action = na.pass)
PAC_cv_low_symb_model <- lmer(log10(FSC.Events_per_cm_2) ~
                       Low_Tide_Mean_Phosphate_umolL_scaled + 
                       Low_Tide_Mean_NN_umolL_scaled + 
                       Low_Tide_Mean_Ammonia_umolL_scaled +
                       Low_Tide_Mean_NNP_umolL_scaled +
                       (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_model_data, 
                                 na.action = na.pass)

PAC_cv_low_symb_model_dredge <- dredge(PAC_cv_low_symb_model, trace = 2)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   6%  |                                                                              |=========                                                             |  12%  |                                                                              |=============                                                         |  19%  |                                                                              |==================                                                    |  25%  |                                                                              |======================                                                |  31%  |                                                                              |==========================                                            |  38%  |                                                                              |===============================                                       |  44%  |                                                                              |===================================                                   |  50%  |                                                                              |=======================================                               |  56%  |                                                                              |============================================                          |  62%  |                                                                              |================================================                      |  69%  |                                                                              |====================================================                  |  75%  |                                                                              |=========================================================             |  81%  |                                                                              |=============================================================         |  88%  |                                                                              |==================================================================    |  94%

``` r
model.sel(PAC_cv_low_symb_model_dredge)
```

    ## Global model call: lmer(formula = log10(FSC.Events_per_cm_2) ~ Low_Tide_Mean_Phosphate_umolL_scaled + 
    ##     Low_Tide_Mean_NN_umolL_scaled + Low_Tide_Mean_Ammonia_umolL_scaled + 
    ##     Low_Tide_Mean_NNP_umolL_scaled + (1 | Genotype) + (1 | Cage_Uncaged), 
    ##     data = PAC_model_data, na.action = na.pass)
    ## ---
    ## Model selection table 
    ##    (Int) Low_Tid_Men_Amm_umL_scl Low_Tid_Men_NN_umL_scl Low_Tid_Men_NNP_umL_scl
    ## 1  3.743                                                                       
    ## 9  3.743                                                                       
    ## 3  3.742                                       -0.01078                        
    ## 2  3.743              -0.0005502                                               
    ## 5  3.743                                                                       
    ## 6  3.743              -0.0005502                                              +
    ## 11 3.742                                       -0.03811                        
    ## 10 3.743              -0.0205600                                               
    ## 13 3.743                                                                       
    ## 14 3.743              -0.0205600                                              +
    ## 4  3.742               0.0036630               -0.01141                        
    ## 8  3.742               0.0036630               -0.01141                       +
    ## 7  3.742                                       -0.01141                        
    ## 12 3.742              -0.0174700               -0.03707                        
    ## 16 3.742              -0.0174700               -0.03707                       +
    ## 15 3.742                                       -0.03707                        
    ##    Low_Tid_Men_Phs_umL_scl df logLik AICc delta weight
    ## 1                           4  0.378  8.2  0.00  0.878
    ## 9                  0.02735  5 -1.768 15.0  6.79  0.029
    ## 3                           5 -2.047 15.5  7.35  0.022
    ## 2                           5 -2.084 15.6  7.42  0.021
    ## 5                           5 -2.084 15.6  7.42  0.021
    ## 6                           5 -2.084 15.6  7.42  0.021
    ## 11                 0.04784  6 -3.642 21.3 13.16  0.001
    ## 10                 0.03813  6 -3.936 21.9 13.75  0.001
    ## 13                 0.03813  6 -3.936 21.9 13.75  0.001
    ## 14                 0.03813  6 -3.936 21.9 13.75  0.001
    ## 4                           6 -4.436 22.9 14.75  0.001
    ## 8                           6 -4.436 22.9 14.75  0.001
    ## 7                           6 -4.436 22.9 14.75  0.001
    ## 12                 0.05654  7 -5.844 28.5 20.31  0.000
    ## 16                 0.05654  7 -5.844 28.5 20.31  0.000
    ## 15                 0.05654  7 -5.844 28.5 20.31  0.000
    ## Models ranked by AICc(x) 
    ## Random terms (all models): 
    ##   1 | Genotype, 1 | Cage_Uncaged

``` r
PAC_cv_low_symb_model_dredge_model.avg = model.avg(PAC_cv_low_symb_model_dredge, delta < 8)
summary(PAC_cv_low_symb_model_dredge_model.avg)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_cv_low_symb_model_dredge, subset = delta < 
    ##     8)
    ## 
    ## Component model call: 
    ## lmer(formula = log10(FSC.Events_per_cm_2) ~ <6 unique rhs>, data = 
    ##      PAC_model_data, na.action = na.pass)
    ## 
    ## Component models: 
    ##    df logLik  AICc delta weight
    ## a   4   0.38  8.17  0.00   0.88
    ## 4   5  -1.77 14.97  6.79   0.03
    ## 2   5  -2.05 15.52  7.35   0.02
    ## 1   5  -2.08 15.60  7.42   0.02
    ## b   5  -2.08 15.60  7.42   0.02
    ## 13  5  -2.08 15.60  7.42   0.02
    ## 
    ## Term codes: 
    ##   Low_Tide_Mean_Ammonia_umolL_scaled        Low_Tide_Mean_NN_umolL_scaled 
    ##                                    1                                    2 
    ##       Low_Tide_Mean_NNP_umolL_scaled Low_Tide_Mean_Phosphate_umolL_scaled 
    ##                                    3                                    4 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                                        Estimate Std. Error Adjusted SE z value
    ## (Intercept)                           3.743e+00  3.463e-02   3.562e-02 105.083
    ## Low_Tide_Mean_Phosphate_umolL_scaled  8.095e-04  7.467e-03   7.600e-03   0.107
    ## Low_Tide_Mean_NN_umolL_scaled        -2.416e-04  5.326e-03   5.467e-03   0.044
    ## Low_Tide_Mean_Ammonia_umolL_scaled   -2.376e-05  7.113e-03   7.319e-03   0.003
    ## Low_Tide_Mean_NNP_umolL_scaled       -1.188e-05  5.029e-03   5.175e-03   0.002
    ##                                      Pr(>|z|)    
    ## (Intercept)                            <2e-16 ***
    ## Low_Tide_Mean_Phosphate_umolL_scaled    0.915    
    ## Low_Tide_Mean_NN_umolL_scaled           0.965    
    ## Low_Tide_Mean_Ammonia_umolL_scaled      0.997    
    ## Low_Tide_Mean_NNP_umolL_scaled          0.998    
    ##  
    ## (conditional average) 
    ##                                        Estimate Std. Error Adjusted SE z value
    ## (Intercept)                           3.7427178  0.0346350   0.0356169 105.083
    ## Low_Tide_Mean_Phosphate_umolL_scaled  0.0273456  0.0340265   0.0350114   0.781
    ## Low_Tide_Mean_NN_umolL_scaled        -0.0107793  0.0339403   0.0349226   0.309
    ## Low_Tide_Mean_Ammonia_umolL_scaled   -0.0005502  0.0342242   0.0352148   0.016
    ## Low_Tide_Mean_NNP_umolL_scaled       -0.0005502  0.0342242   0.0352148   0.016
    ##                                      Pr(>|z|)    
    ## (Intercept)                            <2e-16 ***
    ## Low_Tide_Mean_Phosphate_umolL_scaled    0.435    
    ## Low_Tide_Mean_NN_umolL_scaled           0.758    
    ## Low_Tide_Mean_Ammonia_umolL_scaled      0.988    
    ## Low_Tide_Mean_NNP_umolL_scaled          0.988    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
PAC_symb_cv_plus_lowtide_model <- lmer(Percent_Change ~ 
                       CV_Salinity + 
                       CV_Temperature + 
                       CV_TA + 
                       CV_pH +
                       (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_model_data, 
                                 na.action = na.pass)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
plot(allEffects(PAC_symb_cv_plus_lowtide_model))
```

![](analysis_files/figure-gfm/Symbiont%20cv%20and%20low%20tide%20best%20model%20effects%20plots-1.png)<!-- -->
