02-Analysis
================
(11 June, 2021)

This file contains all the models (OLS, Spatial Lag and Spatial Error)
prepared as part of this analysis.

## Data Wrangling

### Load Data

``` r
# `feeders_year` contains statistical and recorded values of electricity consumption. It does not contain any geometry.

feeders_year <- read.csv(here::here("analysis/data/raw_data/feeders_year.csv")) %>% 
  select(-X)
```

``` r
# `sf_feeders_info` contains geometry for each feeder.

sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))

sf_feeders_info$GRIDID <- as.integer(sf_feeders_info$GRIDID)
```

``` r
# `sf_feeder_area_elec` is `feeders_year` plus the area of each feeder with its electricity density.

sf_feeder_area_elec <- readRDS(here::here("analysis/data/derived_data/sf_feeder_area_elec.rds"))

feeder_area_elec <- sf_feeder_area_elec %>% 
  st_set_geometry(NULL)

feeder_area_elec <- as.data.frame(feeder_area_elec)

feeder_area_elec <- feeder_area_elec %>% 
  select(GRIDID, FEEDER_NAM, KE_IBC, KE_REGION, elec_norm_area)
```

``` r
# `sf_feeder_population` contains the total population for each feeder as obtained from WorldPop. NAs have been replaced by 0.

sf_feeder_population <- readRDS(here::here("analysis/data/derived_data/sf_feeder_population.rds"))

sf_feeder_population$GRIDID <- as.integer(sf_feeder_population$GRIDID)

feeder_population <- sf_feeder_population %>% 
  st_set_geometry(NULL)
```

### Join Data

``` r
# `geometry` is added to `feeders_year` and then saved as `sf_feeders`.

sf_feeders <- left_join(sf_feeders_info, 
                            feeders_year, 
                            by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))

# `feeder_area_elec` is added to `sf_feeders`.

sf_feeders <- left_join(sf_feeders, 
                        feeder_area_elec,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))

# `feeder_population` is added to `sf_feeders`.

sf_feeders <- left_join(sf_feeders, 
                        feeder_population,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))
```

### Filter Data

``` r
# Filter criteria = elec_norm_area > 1 and remove negative losses

sf_feeders_filtered <- sf_feeders %>% 
  filter(elec_norm_area > 1) %>% 
  filter(losses > 0)
```

## Moran’s I

``` r
losses_sp <- as(sf_feeders_filtered, 'Spatial')
losses_neighbours <- poly2nb(losses_sp)
losses_weights <- nb2listw(losses_neighbours, 
                           style = "W",
                           zero.policy = T)
losses_lag <- lag.listw(losses_weights, losses_sp$losses)

losses_lag_df <- sf_feeders_filtered %>% 
  add_column(losses_lag = losses_lag)

losses_moran <- moran.test(losses_lag_df$losses, losses_weights, 
                           zero.policy = T)

losses_moran
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  losses_lag_df$losses  
    ## weights: losses_weights  n reduced by no-neighbour observations
    ##   
    ## 
    ## Moran I statistic standard deviate = 28.953, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.5548920528     -0.0008841733      0.0003684892

``` r
losses_moran$p.value
```

    ## [1] 1.301294e-184

## OLS Model

``` r
# Remove the received term as it gets included in losses

ols_losses <- lm(losses ~ tot + comRel + resRel + indRel + populationsum + factor(KE_IBC), 
                 data = sf_feeders_filtered)
```

``` r
OLS_table <- tidy(ols_losses)

saveRDS(OLS_table, here::here("analysis/data/derived_data/res_OLS_coefficients.rds"))
```

``` r
# Calculate confidence intervals

ciOLS <- as.data.frame(tidy(ols_losses, conf.int = TRUE))

write.csv(ciOLS, here::here("analysis/data/derived_data/res_OLS_ci.csv"))
```

``` r
# Augment model

sf_feeders_filtered <- augment(ols_losses, 
                               data = sf_feeders_filtered) %>% 
  st_as_sf()

ols_coefficients <- data.frame(coefficients(ols_losses))

# Coeff cleanup
ols_coefficients <- cbind(Names = rownames(ols_coefficients), ols_coefficients)

# Number rows
rownames(ols_coefficients) <- 1:nrow(ols_coefficients)
```

#### Set Weights

``` r
sf_feeders_filtered_sp <- as(sf_feeders_filtered, 'Spatial')
sf_feeders_filtered_neighbours <- poly2nb(sf_feeders_filtered_sp)
sf_feeders_filtered_weights <- nb2listw(sf_feeders_filtered_neighbours, 
                                        style = "W", 
                                        zero.policy = TRUE)
sf_feeders_filtered$.resid_lag <- lag.listw(sf_feeders_filtered_weights, 
                                            sf_feeders_filtered$.resid)
```

``` r
lm(.resid_lag ~ .resid, sf_feeders_filtered)
```

    ## 
    ## Call:
    ## lm(formula = .resid_lag ~ .resid, data = sf_feeders_filtered)
    ## 
    ## Coefficients:
    ## (Intercept)       .resid  
    ##   -0.001075     0.223569

``` r
OLS_moran <- moran.test(sf_feeders_filtered$.resid, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)

OLS_moran$p.value
```

    ## [1] 7.414671e-32

## Spatial Lag Model

``` r
# Rescale variables

rescale_tot <- (scale(as.numeric(sf_feeders_filtered$tot)))

rescale_tot <- as.data.frame(rescale_tot)

# Join rescaled variables

sf_feeders_filtered <- bind_cols(sf_feeders_filtered, rescale_tot)

sf_feeders_filtered <- sf_feeders_filtered %>% 
  rename(rescale_tot = `V1`)
```

``` r
lag_losses <- lagsarlm(losses ~ rescale_tot + comRel + resRel + indRel + populationsum + factor(KE_IBC),
                 data = sf_feeders_filtered,
                 listw = sf_feeders_filtered_weights,
                 na.action(sf_feeders_filtered_weights),
                 zero.policy = T)
```

``` r
lag_estimates <- summary(lag_losses)$Coef[,1]
lag_estimates <- as.data.frame(lag_estimates)
lag_estimates <- cbind(Names = rownames(lag_estimates), lag_estimates)
rownames(lag_estimates) <- 1:nrow(lag_estimates)


lag_stderror <- summary(lag_losses)$Coef[,2]
lag_stderror <- as.data.frame(lag_stderror)
lag_stderror <- cbind(Names = rownames(lag_stderror), lag_stderror)
rownames(lag_stderror) <- 1:nrow(lag_stderror)

lag_prz <- summary(lag_losses)$Coef[,4]
lag_prz <- as.data.frame(lag_prz)

lag_prz <- cbind(Names = rownames(lag_prz), lag_prz)
rownames(lag_prz) <- 1:nrow(lag_prz)

lag_table <- left_join(lag_estimates, lag_stderror)
lag_table <- left_join(lag_table, lag_prz)

# Calculate confidence intervals

ciLag <- as.data.frame(tidy(lag_losses, conf.int = TRUE))
```

``` r
impacts(lag_losses, listw = sf_feeders_filtered_weights)
```

    ## Impact measures (lag, exact):
    ##                                      Direct      Indirect         Total
    ## rescale_tot                    0.0178278544  0.0084628207  0.0262906751
    ## comRel                        -0.0546289442 -0.0259321703 -0.0805611145
    ## resRel                         0.0605934271  0.0287634897  0.0893569168
    ## indRel                        -0.2216521423 -0.1052175032 -0.3268696455
    ## populationsum                  0.0002892557  0.0001373087  0.0004265644
    ## factor(KE_IBC)Baldia           0.1119245560  0.0531301986  0.1650547546
    ## factor(KE_IBC)Bin Qasim        0.1635925124  0.0776567983  0.2412493107
    ## factor(KE_IBC)Clifton         -0.0684863239 -0.0325102204 -0.1009965443
    ## factor(KE_IBC)Defence         -0.0747508102 -0.0354839503 -0.1102347605
    ## factor(KE_IBC)F.B Area        -0.0265884230 -0.0126214322 -0.0392098551
    ## factor(KE_IBC)Gadap            0.1341391424  0.0636753857  0.1978145281
    ## factor(KE_IBC)Garden          -0.0478717022 -0.0227245310 -0.0705962332
    ## factor(KE_IBC)Gulshan-e-Iqbal -0.0809112553 -0.0384082923 -0.1193195475
    ## factor(KE_IBC)Johar-I         -0.0503659627 -0.0239085478 -0.0742745105
    ## factor(KE_IBC)Johar-II         0.0403594711  0.0191585010  0.0595179721
    ## factor(KE_IBC)Johar II         0.1347299458  0.0639558380  0.1986857838
    ## factor(KE_IBC)KIMZ             0.0483584625  0.0229555944  0.0713140569
    ## factor(KE_IBC)Korangi          0.1490587240  0.0707576593  0.2198163833
    ## factor(KE_IBC)Landhi           0.1426292256  0.0677055987  0.2103348243
    ## factor(KE_IBC)Liaquatabad      0.0343093223  0.0162865163  0.0505958385
    ## factor(KE_IBC)Lyari-I          0.0285204017  0.0135385358  0.0420589375
    ## factor(KE_IBC)Lyari-II         0.0760868697  0.0361181731  0.1122050428
    ## factor(KE_IBC)Malir            0.1072776216  0.0509243149  0.1582019365
    ## factor(KE_IBC)Nazimabad        0.0264062257  0.0125349437  0.0389411694
    ## factor(KE_IBC)North Karachi    0.0492632670  0.0233851020  0.0726483690
    ## factor(KE_IBC)North Nazimabad -0.0326567811 -0.0155020607 -0.0481588418
    ## factor(KE_IBC)Orangi-I         0.1830248007  0.0868812381  0.2699060388
    ## factor(KE_IBC)Orangi-II        0.0755475163  0.0358621440  0.1114096602
    ## factor(KE_IBC)Saddar          -0.0123267012 -0.0058514423 -0.0181781434
    ## factor(KE_IBC)Shah Faisal      0.0314673023  0.0149374193  0.0464047216
    ## factor(KE_IBC)SITE             0.0254461597  0.0120792037  0.0375253634
    ## factor(KE_IBC)Surjani          0.0842707023  0.0400030101  0.1242737124
    ## factor(KE_IBC)Tipu Sultan     -0.0243035404 -0.0115368064 -0.0358403468

``` r
sf_feeders_filtered$resid_lagsarlm <- residuals(lag_losses)
```

``` r
lag_moran <- moran.test(sf_feeders_filtered$resid_lagsarlm, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)

lag_moran$p.value
```

    ## [1] 0.1143647

``` r
lag_coefficients <- data.frame(coefficients(lag_losses))

# Coeff cleanup
lag_coefficients <- cbind(Names = rownames(lag_coefficients), lag_coefficients)

# Number rows
rownames(lag_coefficients) <- 1:nrow(lag_coefficients)
```

## Spatial Error Model

``` r
error_losses <- errorsarlm(losses ~ rescale_tot + comRel + resRel + indRel + populationsum + factor(KE_IBC),  data = sf_feeders_filtered,
                 listw = sf_feeders_filtered_weights,
                 na.action(sf_feeders_filtered_weights),
                 zero.policy = T)
```

``` r
error_estimates <- summary(error_losses)$Coef[,1]
error_estimates <- as.data.frame(error_estimates)
error_estimates <- cbind(Names = rownames(error_estimates), error_estimates)
rownames(error_estimates) <- 1:nrow(error_estimates)

error_stderror <- summary(error_losses)$Coef[,2]
error_stderror <- as.data.frame(error_stderror)
error_stderror <- cbind(Names = rownames(error_stderror), error_stderror)
rownames(error_stderror) <- 1:nrow(error_stderror)

error_prz <- summary(error_losses)$Coef[,4]
error_prz <- as.data.frame(error_prz)
error_prz <- cbind(Names = rownames(error_prz), error_prz)
rownames(error_prz) <- 1:nrow(error_prz)

error_table <- left_join(error_estimates, error_stderror)
error_table <- left_join(error_table, error_prz)

saveRDS(error_table, here::here("analysis/data/derived_data/res_error_coefficients.rds"))

# Calculate confidence intervals

ciError <- as.data.frame(tidy(error_losses, conf.int = TRUE))

write.csv(ciError, here::here("analysis/data/derived_data/res_error_ci.csv"))

sf_feeders_filtered$resid_error <- residuals(error_losses)
```

``` r
error_moran <- moran.test(sf_feeders_filtered$resid_error, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)

error_moran$p.value
```

    ## [1] 0.8075699

``` r
error_coefficients <- data.frame(coefficients(error_losses))

# Coeff cleanup
error_coefficients <- cbind(Names = rownames(error_coefficients), error_coefficients)

# Number rows
rownames(error_coefficients) <- 1:nrow(error_coefficients)
```

### Export Residuals

``` r
saveRDS(sf_feeders_filtered, here::here("analysis/data/derived_data/res_model_residuals.rds"))
```
