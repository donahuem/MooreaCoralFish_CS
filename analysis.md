Coral
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
all_nut_long <- pivot_longer(
  data = all_nut,
  cols = c(Minimum_Salinity:CV_Ammonia_umolL)) 
names(all_nut_long)[names(all_nut_long) == "name"] <- "Parameter"

join <- explanatory_seasonal[,c(2:7)]
join <- unique(join)

explanatory_all <- left_join(join,all_nut_long, by = join_by(CowTagID))
explanatory_all_wide <- left_join(join, all_nut,by = join_by(CowTagID))
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

### Models

#### Need to add a caged effect

#### Random effect of genotype

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
```

``` r
PAC_model_data <- na.omit(all_data_wide %>% 
        filter(Species == "Pocillopora acuta"))

# Extract column names for filtering
selected_columns <- c("Percent_Change", "Genotype", "Cage_Uncaged", CV_columns)

# Filter the data and select the relevant columns
PAC_cv_model_data <- PAC_model_data[, selected_columns]

# Build the GLM model
#PAC_cv_model <- glm(Percent_Change ~ ., data = PAC_cv_model_data, na.action = na.pass)
PAC_cv_model <- lmer(Percent_Change ~ CV_Salinity + CV_Temperature + CV_TA + CV_pH + CV_Phosphate_umolL + CV_NN_umolL + CV_Ammonia_umolL + (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_cv_model_data, 
                                 na.action = na.pass)

PAC_cv_model_dredge <- dredge(PAC_cv_model)
#model.sel(PAC_cv_model_dredge)
PAC.avg_cv_model = model.avg(PAC_cv_model_dredge, delta < 4)
summary(PAC.avg_cv_model)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_cv_model_dredge, subset = delta < 4)
    ## 
    ## Component model call: 
    ## lmer(formula = Percent_Change ~ <4 unique rhs>, data = 
    ##      PAC_cv_model_data, na.action = na.pass)
    ## 
    ## Component models: 
    ##       df logLik    AICc delta weight
    ## 2456   8  90.06 -160.44  0.00   0.65
    ## 12456  9  89.97 -157.20  3.24   0.13
    ## 256    7  86.95 -157.10  3.34   0.12
    ## 23456  9  89.72 -156.70  3.74   0.10
    ## 
    ## Term codes: 
    ##        CV_NN_umolL              CV_pH CV_Phosphate_umolL        CV_Salinity 
    ##                  1                  2                  3                  4 
    ##              CV_TA     CV_Temperature 
    ##                  5                  6 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                     Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)        -0.476140   0.147737    0.151563   3.142 0.001681 ** 
    ## CV_pH              48.371252  13.981900   14.348241   3.371 0.000748 ***
    ## CV_Salinity        -4.672252   5.118706    5.259982   0.888 0.374399    
    ## CV_TA              12.299425   4.555935    4.698008   2.618 0.008844 ** 
    ## CV_Temperature      6.937413   2.402563    2.476509   2.801 0.005090 ** 
    ## CV_NN_umolL        -0.023003   0.073397    0.074190   0.310 0.756518    
    ## CV_Phosphate_umolL  0.005927   0.089286    0.092032   0.064 0.948648    
    ##  
    ## (conditional average) 
    ##                    Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)        -0.47614    0.14774     0.15156   3.142 0.001681 ** 
    ## CV_pH              48.37125   13.98190    14.34824   3.371 0.000748 ***
    ## CV_Salinity        -5.32223    5.13681     5.29689   1.005 0.315001    
    ## CV_TA              12.29943    4.55594     4.69801   2.618 0.008844 ** 
    ## CV_Temperature      6.93741    2.40256     2.47651   2.801 0.005090 ** 
    ## CV_NN_umolL        -0.17892    0.11834     0.12213   1.465 0.142907    
    ## CV_Phosphate_umolL  0.05928    0.27672     0.28558   0.208 0.835543    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

OK so that’s for a coefficient of variance hypothesis. It shows that the
CV for pH, TA, and Temperature matter, but not the nutrients! Our best
model includes pH, Salinity, TA, and Temperature. Here’s those effects
plots:

``` r
CV_model_effects_plots <- lmer(Percent_Change ~ CV_Salinity + 
                                 CV_Temperature + 
                                 CV_TA + 
                                 CV_pH + 
                                 (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_cv_model_data, 
                                 na.action = na.pass)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
plot(allEffects(CV_model_effects_plots))
```

![](analysis_files/figure-gfm/effect%20plots-1.png)<!-- -->

What about mean low!

``` r
PAC_model_data <- na.omit(all_data_wide %>% 
        filter(Species == "Pocillopora acuta"))

# Extract column names for filtering
selected_columns <- c("Percent_Change","Genotype", "Cage_Uncaged", mean_low_columns)

# Filter the data and select the relevant columns
PAC_low_tide_model_data <- PAC_model_data[, selected_columns]

# Build the big model
#PAC_mean_low_tide_model <- lmer(Percent_Change ~ . + (1 | Genotype) + (1 | Cage_Uncaged), 
#                                 data = PAC_low_tide_model_data, 
#                                 na.action = na.pass)

PAC_mean_low_tide_model <- lmer(Percent_Change ~ Low_Tide_Mean_Salinity + Low_Tide_Mean_Temperature + Low_Tide_Mean_TA + Low_Tide_Mean_pH + Low_Tide_Mean_Phosphate_umolL + Low_Tide_Mean_NN_umolL + Low_Tide_Mean_Ammonia_umolL + (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_low_tide_model_data, 
                                 na.action = na.pass)

PAC_low_tide_model_dredge <- dredge(PAC_mean_low_tide_model)
#model.sel(PAC_low_tide_model_dredge)
PAC.avg_low_tide = model.avg(PAC_low_tide_model_dredge, delta < 5)
summary(PAC.avg_low_tide)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_low_tide_model_dredge, subset = delta < 
    ##     5)
    ## 
    ## Component model call: 
    ## lmer(formula = Percent_Change ~ <18 unique rhs>, data = 
    ##      PAC_low_tide_model_data, na.action = na.pass)
    ## 
    ## Component models: 
    ##        df logLik    AICc delta weight
    ## 4       5  69.69 -127.94  0.00   0.15
    ## 14      6  70.85 -127.66  0.29   0.13
    ## 46      6  70.68 -127.30  0.64   0.11
    ## 34      6  70.48 -126.90  1.04   0.09
    ## (Null)  4  67.91 -126.89  1.05   0.09
    ## 134     7  71.52 -126.25  1.69   0.06
    ## 346     7  71.44 -126.07  1.87   0.06
    ## 145     7  71.32 -125.84  2.10   0.05
    ## 3       5  68.62 -125.80  2.14   0.05
    ## 6       5  68.42 -125.42  2.53   0.04
    ## 36      6  69.63 -125.20  2.74   0.04
    ## 1345    8  72.42 -125.16  2.79   0.04
    ## 2346    8  71.66 -123.64  4.31   0.02
    ## 15      6  68.81 -123.58  4.37   0.02
    ## 146     7  70.10 -123.40  4.54   0.02
    ## 45      6  68.71 -123.37  4.57   0.02
    ## 24      6  68.61 -123.17  4.77   0.01
    ## 234     7  69.97 -123.15  4.79   0.01
    ## 
    ## Term codes: 
    ##   Low_Tide_Mean_Ammonia_umolL        Low_Tide_Mean_NN_umolL 
    ##                             1                             2 
    ##              Low_Tide_Mean_pH Low_Tide_Mean_Phosphate_umolL 
    ##                             3                             4 
    ##        Low_Tide_Mean_Salinity     Low_Tide_Mean_Temperature 
    ##                             5                             6 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                                Estimate Std. Error Adjusted SE z value Pr(>|z|)
    ## (Intercept)                    3.588703   7.567568    7.670536   0.468    0.640
    ## Low_Tide_Mean_Phosphate_umolL  1.222503   1.053053    1.067856   1.145    0.252
    ## Low_Tide_Mean_Ammonia_umolL   -0.032773   0.053086    0.053294   0.615    0.539
    ## Low_Tide_Mean_Temperature     -0.078025   0.147207    0.148313   0.526    0.599
    ## Low_Tide_Mean_pH              -0.049035   0.629608    0.642467   0.076    0.939
    ## Low_Tide_Mean_Salinity        -0.030236   0.103381    0.104022   0.291    0.771
    ## Low_Tide_Mean_NN_umolL        -0.006775   0.040851    0.041275   0.164    0.870
    ##  
    ## (conditional average) 
    ##                               Estimate Std. Error Adjusted SE z value Pr(>|z|)
    ## (Intercept)                    3.58870    7.56757     7.67054   0.468  0.63989
    ## Low_Tide_Mean_Phosphate_umolL  1.60098    0.91994     0.94202   1.700  0.08922
    ## Low_Tide_Mean_Ammonia_umolL   -0.10397    0.03921     0.04010   2.593  0.00952
    ## Low_Tide_Mean_Temperature     -0.27861    0.14661     0.15054   1.851  0.06421
    ## Low_Tide_Mean_pH              -0.13296    1.03137     1.05265   0.126  0.89949
    ## Low_Tide_Mean_Salinity        -0.24907    0.18312     0.18609   1.338  0.18075
    ## Low_Tide_Mean_NN_umolL        -0.15159    0.12404     0.12714   1.192  0.23315
    ##                                 
    ## (Intercept)                     
    ## Low_Tide_Mean_Phosphate_umolL . 
    ## Low_Tide_Mean_Ammonia_umolL   **
    ## Low_Tide_Mean_Temperature     . 
    ## Low_Tide_Mean_pH                
    ## Low_Tide_Mean_Salinity          
    ## Low_Tide_Mean_NN_umolL          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Note that there are many models with a similar Akaike weight. Note that
the low tide (high SGD) values of Ammonia, Nitrate/Nitrite, NNP ratio,
Phosphate, and salinity are all in most of the best models, and so they
all have support from the data. Use the best model to report
coefficients / effect size plots / etc.

``` r
Low_tide_PAC_growth_model_effects_plots <- lmer(Percent_Change ~ Low_Tide_Mean_Phosphate_umolL + 
                                 (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_low_tide_model_data, 
                                 na.action = na.pass)

plot(allEffects(Low_tide_PAC_growth_model_effects_plots))
```

![](analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
PAC_model_data <- na.omit(all_data_wide %>% 
        filter(Species == "Pocillopora acuta"))

# Extract column names for filtering
selected_columns <- c("FSC.Events_per_cm_2", "Genotype", "Cage_Uncaged", CV_columns)

# Filter the data and select the relevant columns
PAC_cv_model_data <- PAC_model_data[, selected_columns]

# Build the GLM model
#PAC_cv_model <- glm(Percent_Change ~ ., data = PAC_cv_model_data, na.action = na.pass)
PAC_cv_model <- lmer(log(FSC.Events_per_cm_2) ~ CV_Salinity + CV_Temperature + CV_TA + CV_pH + CV_Phosphate_umolL + CV_NN_umolL + CV_Ammonia_umolL + (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_cv_model_data, 
                                 na.action = na.pass)

PAC_cv_model_dredge <- dredge(PAC_cv_model)
#model.sel(PAC_cv_model_dredge)
PAC.avg_cv_model = model.avg(PAC_cv_model_dredge, delta < 2)
#I only have a model that includes everything! You take out one, the model gets significantly worse. I don't fully know how to interpret this right now, espeically since only phosphate is coming out significant here...
summary(PAC.avg_cv_model)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_cv_model_dredge, subset = delta < 2)
    ## 
    ## Component model call: 
    ## lmer(formula = log(FSC.Events_per_cm_2) ~ <2 unique rhs>, data = 
    ##      PAC_cv_model_data, na.action = na.pass)
    ## 
    ## Component models: 
    ##        df logLik  AICc delta weight
    ## 123456 10 -10.30 46.54  0.00   0.71
    ## 23456   9 -12.79 48.32  1.78   0.29
    ## 
    ## Term codes: 
    ##        CV_NN_umolL              CV_pH CV_Phosphate_umolL        CV_Salinity 
    ##                  1                  2                  3                  4 
    ##              CV_TA     CV_Temperature 
    ##                  5                  6 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                    Estimate Std. Error Adjusted SE z value Pr(>|z|)   
    ## (Intercept)           6.188      2.173       2.233   2.771  0.00558 **
    ## CV_NN_umolL           1.575      1.556       1.586   0.993  0.32056   
    ## CV_pH               237.301    200.699     206.094   1.151  0.24956   
    ## CV_Phosphate_umolL   -7.246      3.330       3.434   2.110  0.03486 * 
    ## CV_Salinity          71.656     57.555      59.429   1.206  0.22792   
    ## CV_TA                -6.251     48.951      50.545   0.124  0.90157   
    ## CV_Temperature       45.128     29.047      29.974   1.506  0.13217   
    ##  
    ## (conditional average) 
    ##                    Estimate Std. Error Adjusted SE z value Pr(>|z|)   
    ## (Intercept)           6.188      2.173       2.233   2.771  0.00558 **
    ## CV_NN_umolL           2.222      1.406       1.452   1.530  0.12608   
    ## CV_pH               237.301    200.699     206.094   1.151  0.24956   
    ## CV_Phosphate_umolL   -7.246      3.330       3.434   2.110  0.03486 * 
    ## CV_Salinity          71.656     57.555      59.429   1.206  0.22792   
    ## CV_TA                -6.251     48.951      50.545   0.124  0.90157   
    ## CV_Temperature       45.128     29.047      29.974   1.506  0.13217   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Megan WHAT is going on!!!! Only the most complicated model has support,
but even in that, only Phosphate has significant effects?

``` r
CV_PAC_symb_model_effects_plots <- lmer(log(FSC.Events_per_cm_2) ~ CV_Phosphate_umolL + 
                                 (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_cv_model_data, 
                                 na.action = na.pass)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(allEffects(CV_PAC_symb_model_effects_plots))
```

![](analysis_files/figure-gfm/CV%20Symbiont%20Model%20Effects-1.png)<!-- -->

``` r
PAC_model_data <- na.omit(all_data_wide %>% 
        filter(Species == "Pocillopora acuta"))

# Extract column names for filtering
selected_columns <- c("FSC.Events_per_cm_2","Genotype", "Cage_Uncaged", mean_low_columns)

# Filter the data and select the relevant columns
PAC_low_tide_model_data <- PAC_model_data[, selected_columns]

# Build the big model
#PAC_mean_low_tide_model <- lmer(Percent_Change ~ . + (1 | Genotype) + (1 | Cage_Uncaged), 
#                                 data = PAC_low_tide_model_data, 
#                                 na.action = na.pass)

PAC_mean_low_tide_model <- lmer(log(FSC.Events_per_cm_2) ~ Low_Tide_Mean_Salinity + Low_Tide_Mean_Temperature + Low_Tide_Mean_TA + Low_Tide_Mean_pH + Low_Tide_Mean_Phosphate_umolL + Low_Tide_Mean_NN_umolL + Low_Tide_Mean_Ammonia_umolL + (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_low_tide_model_data, 
                                 na.action = na.pass)

PAC_low_tide_model_dredge <- dredge(PAC_mean_low_tide_model)
#model.sel(PAC_low_tide_model_dredge)
PAC.avg_low_tide = model.avg(PAC_low_tide_model_dredge, delta < 1) #I had to make it all the way to 10 to include more than the most complex model. I don't really know how to interpret that!
summary(PAC.avg_low_tide)
```

    ## 
    ## Call:
    ## model.avg(object = PAC_low_tide_model_dredge, subset = delta < 
    ##     1)
    ## 
    ## Component model call: 
    ## lmer(formula = log(FSC.Events_per_cm_2) ~ <7 unique rhs>, data = 
    ##      PAC_low_tide_model_data, na.action = na.pass)
    ## 
    ## Component models: 
    ##       df logLik  AICc delta weight
    ## 123    7 -30.77 78.34  0.00   0.18
    ## 1235   8 -29.44 78.56  0.23   0.17
    ## 1234   8 -29.53 78.75  0.41   0.15
    ## 12345  9 -28.13 79.00  0.67   0.13
    ## 23     6 -32.53 79.11  0.77   0.13
    ## 235    7 -31.15 79.11  0.77   0.13
    ## 234    7 -31.24 79.28  0.94   0.12
    ## 
    ## Term codes: 
    ##        Low_Tide_Mean_NN_umolL              Low_Tide_Mean_pH 
    ##                             1                             2 
    ## Low_Tide_Mean_Phosphate_umolL        Low_Tide_Mean_Salinity 
    ##                             3                             4 
    ##     Low_Tide_Mean_Temperature 
    ##                             5 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                                Estimate Std. Error Adjusted SE z value Pr(>|z|)
    ## (Intercept)                    65.62194  117.00147   119.98413   0.547    0.584
    ## Low_Tide_Mean_NN_umolL         -0.86216    1.17170     1.19684   0.720    0.471
    ## Low_Tide_Mean_pH               -6.59161   12.91066    13.24616   0.498    0.619
    ## Low_Tide_Mean_Phosphate_umolL   8.80492    9.27394     9.55028   0.922    0.357
    ## Low_Tide_Mean_Temperature      -0.31473    0.98842     1.01505   0.310    0.757
    ## Low_Tide_Mean_Salinity          0.09341    0.90637     0.93298   0.100    0.920
    ##  
    ## (conditional average) 
    ##                               Estimate Std. Error Adjusted SE z value Pr(>|z|)
    ## (Intercept)                    65.6219   117.0015    119.9841   0.547    0.584
    ## Low_Tide_Mean_NN_umolL         -1.3623     1.2198      1.2578   1.083    0.279
    ## Low_Tide_Mean_pH               -6.5916    12.9107     13.2462   0.498    0.619
    ## Low_Tide_Mean_Phosphate_umolL   8.8049     9.2739      9.5503   0.922    0.357
    ## Low_Tide_Mean_Temperature      -0.7437     1.4105      1.4545   0.511    0.609
    ## Low_Tide_Mean_Salinity          0.2344     1.4243      1.4668   0.160    0.873

Now, for mean low tide data, nothing is significant!!!! But only the
most complicated model has support. I don’t know how this means I should
interpret the findings.

``` r
Low_tide_PAC_symb_model_effects_plots <- lmer(log(FSC.Events_per_cm_2) ~ Low_Tide_Mean_NN_umolL + 
                                                Low_Tide_Mean_pH + 
                                                Low_Tide_Mean_Phosphate_umolL +
                                 (1 | Genotype) + (1 | Cage_Uncaged), 
                                 data = PAC_low_tide_model_data, 
                                 na.action = na.pass)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(allEffects(Low_tide_PAC_symb_model_effects_plots))
```

![](analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
