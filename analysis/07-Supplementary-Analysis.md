07-Supplementary-Analysis
================
(11 June, 2021)

## Load Data

``` r
# Read in feeder data

feeders_year <- read.csv(here::here("analysis/data/raw_data/feeders_year.csv")) %>% 
  select(-X)

feeders_year$GRIDID <- as.integer(feeders_year$GRIDID)
```

## Wrangling for models

``` r
sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))

sf_feeders_info$GRIDID <- as.integer(sf_feeders_info$GRIDID)
```

``` r
sf_feeder_area_elec <- readRDS(here::here("analysis/data/derived_data/sf_feeder_area_elec.rds"))
```

``` r
feeder_area_elec <- sf_feeder_area_elec %>% 
  st_set_geometry(NULL)
```

``` r
feeder_area_elec <- as.data.frame(feeder_area_elec)
```

``` r
feeder_area_elec <- feeder_area_elec %>% 
  select(GRIDID, FEEDER_NAM, KE_IBC, KE_REGION, elec_norm_area)
```

``` r
sf_feeder_population <- readRDS(here::here("analysis/data/derived_data/sf_feeder_population.rds"))

sf_feeder_population$GRIDID <- as.integer(sf_feeder_population$GRIDID)
```

``` r
feeder_population <- sf_feeder_population %>% 
  st_set_geometry(NULL)
```

### Join Data

``` r
sf_feeders <- left_join(sf_feeders_info, 
                            feeders_year, 
                            by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))
```

`geometry` is added to `feeders_year` and then saved as `sf_feeders`.

``` r
sf_feeders <- left_join(sf_feeders, 
                        feeder_area_elec,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))
```

`elec_norm_area` or the electricity density is added to `sf_feeders`.

``` r
sf_feeders <- left_join(sf_feeders, 
                        feeder_population,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))
```

`populationsum` is added to `sf_feeders`.

### Filter Data

Filter criteria = losses &gt; 0. Filters of electricity density are not
used here.

``` r
sf_feeders_filtered <- sf_feeders %>% 
  filter(losses > 0)
```

The filter of `losses > 0` brings it to 1187 feeders from 1258.

### Moran’s I for Losses

``` r
losses_sp <- as(sf_feeders_filtered, 'Spatial')
losses_neighbours <- poly2nb(losses_sp)
losses_weights <- nb2listw(losses_neighbours, 
                           style = "W",
                           zero.policy = T)
losses_lag <- lag.listw(losses_weights, losses_sp$losses)
```

    ## Warning in lag.listw(losses_weights, losses_sp$losses): NAs in lagged values

``` r
losses_lag_df <- sf_feeders_filtered %>% 
  add_column(losses_lag = losses_lag)

losses_moran <- moran.test(losses_lag_df$losses, losses_weights, 
                           zero.policy = T)

losses_moran$p.value
```

    ## [1] 4.470926e-184

### OLS Model

``` r
ols_losses <- lm(losses ~ tot + comRel + resRel + indRel + populationsum + factor(KE_IBC), data = sf_feeders_filtered)

OLS_table <- tidy(ols_losses)

summary(ols_losses)
```

    ## 
    ## Call:
    ## lm(formula = losses ~ tot + comRel + resRel + indRel + populationsum + 
    ##     factor(KE_IBC), data = sf_feeders_filtered)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.42299 -0.06742 -0.00417  0.06278  0.56281 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.711e-01  2.533e-02   6.753 2.29e-11 ***
    ## tot                            1.417e-05  2.980e-06   4.756 2.23e-06 ***
    ## comRel                        -8.468e-02  2.461e-02  -3.441 0.000601 ***
    ## resRel                         6.024e-02  2.076e-02   2.902 0.003773 ** 
    ## indRel                        -2.352e-01  3.805e-02  -6.181 8.84e-10 ***
    ## populationsum                  1.625e-04  3.954e-04   0.411 0.681139    
    ## factor(KE_IBC)Baldia           1.452e-01  2.500e-02   5.807 8.23e-09 ***
    ## factor(KE_IBC)Bin Qasim        1.914e-01  2.490e-02   7.687 3.21e-14 ***
    ## factor(KE_IBC)Clifton         -9.795e-02  2.486e-02  -3.941 8.62e-05 ***
    ## factor(KE_IBC)Defence         -1.139e-01  2.132e-02  -5.342 1.11e-07 ***
    ## factor(KE_IBC)F.B Area        -3.672e-02  2.493e-02  -1.473 0.141118    
    ## factor(KE_IBC)Gadap            2.094e-01  2.701e-02   7.753 1.97e-14 ***
    ## factor(KE_IBC)Garden          -6.330e-02  2.545e-02  -2.487 0.013017 *  
    ## factor(KE_IBC)Gulshan-e-Iqbal -1.092e-01  2.385e-02  -4.577 5.22e-06 ***
    ## factor(KE_IBC)Johar-I         -6.176e-02  2.495e-02  -2.476 0.013443 *  
    ## factor(KE_IBC)Johar-II         8.266e-02  2.758e-02   2.997 0.002785 ** 
    ## factor(KE_IBC)Johar II         2.150e-01  1.195e-01   1.800 0.072158 .  
    ## factor(KE_IBC)KIMZ             5.094e-02  2.407e-02   2.116 0.034548 *  
    ## factor(KE_IBC)Korangi          2.024e-01  2.567e-02   7.886 7.19e-15 ***
    ## factor(KE_IBC)Landhi           2.006e-01  3.018e-02   6.647 4.60e-11 ***
    ## factor(KE_IBC)Liaquatabad      5.369e-02  2.737e-02   1.962 0.050000 *  
    ## factor(KE_IBC)Lyari-I          2.761e-02  2.679e-02   1.030 0.303032    
    ## factor(KE_IBC)Lyari-II         1.120e-01  2.835e-02   3.952 8.23e-05 ***
    ## factor(KE_IBC)Malir            1.526e-01  2.460e-02   6.204 7.67e-10 ***
    ## factor(KE_IBC)Nazimabad        3.390e-02  2.943e-02   1.152 0.249494    
    ## factor(KE_IBC)North Karachi    6.319e-02  2.524e-02   2.504 0.012418 *  
    ## factor(KE_IBC)North Nazimabad -3.662e-02  2.557e-02  -1.432 0.152393    
    ## factor(KE_IBC)Orangi-I         2.281e-01  2.540e-02   8.983  < 2e-16 ***
    ## factor(KE_IBC)Orangi-II        1.242e-01  2.928e-02   4.243 2.39e-05 ***
    ## factor(KE_IBC)Saddar          -2.395e-02  2.269e-02  -1.056 0.291409    
    ## factor(KE_IBC)Shah Faisal      4.806e-02  2.516e-02   1.910 0.056363 .  
    ## factor(KE_IBC)SITE             1.653e-02  2.291e-02   0.721 0.470938    
    ## factor(KE_IBC)Surjani          1.398e-01  2.571e-02   5.437 6.60e-08 ***
    ## factor(KE_IBC)Tipu Sultan     -4.018e-02  2.365e-02  -1.699 0.089556 .  
    ## factor(KE_IBC)Uthal            3.475e-01  5.107e-02   6.805 1.62e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1182 on 1152 degrees of freedom
    ## Multiple R-squared:  0.5591, Adjusted R-squared:  0.5461 
    ## F-statistic: 42.97 on 34 and 1152 DF,  p-value: < 2.2e-16

``` r
# Calculate confidence intervals

ciOLS <- as.data.frame(tidy(ols_losses, conf.int = TRUE))
```

``` r
# Augment model

sf_feeders_filtered <- augment(ols_losses, 
                          data = sf_feeders_filtered) %>% 
  st_as_sf()
```

``` r
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

    ## Warning in lag.listw(sf_feeders_filtered_weights, sf_feeders_filtered$.resid):
    ## NAs in lagged values

Weights are set on the basis of neighbours. This means that adjacent
feeders influence each other.

``` r
OLS_moran <- moran.test(sf_feeders_filtered$.resid, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)
```

### Spatial Lag Model

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

    ## Warning in lagsarlm(losses ~ rescale_tot + comRel + resRel + indRel +
    ## populationsum + : install the spatialreg package

    ## Warning: Function can.be.simmed moved to the spatialreg package

    ## Warning: Function jacobianSetup moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

``` r
summary(lag_losses, Nagelkerke = T)
```

    ## Warning in summary.sarlm(lag_losses, Nagelkerke = T): install the spatialreg
    ## package

    ## Warning in residuals.sarlm(obj): install the spatialreg package

    ## Warning in logLik.sarlm(obj): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in print.summary.sarlm(x): install the spatialreg package

    ## 
    ## Call:lagsarlm(formula = losses ~ rescale_tot + comRel + resRel + indRel + 
    ##     populationsum + factor(KE_IBC), data = sf_feeders_filtered, 
    ##     listw = sf_feeders_filtered_weights, na.action = na.action(sf_feeders_filtered_weights), 
    ##     zero.policy = T)
    ## 
    ## Residuals:

    ## Warning in residuals.sarlm(x): install the spatialreg package

    ##        Min         1Q     Median         3Q        Max 
    ## -0.4075120 -0.0633882 -0.0046109  0.0575358  0.5855326 
    ## 
    ## Type: lag 
    ## Regions with no neighbours included:
    ##  456 713 734 960 
    ## Coefficients: (asymptotic standard errors) 
    ##                                  Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)                    1.2549e-01  2.5360e-02  4.9485 7.478e-07
    ## rescale_tot                    1.7279e-02  4.2396e-03  4.0757 4.589e-05
    ## comRel                        -6.8572e-02  2.3091e-02 -2.9697 0.0029813
    ## resRel                         5.4451e-02  1.9410e-02  2.8054 0.0050258
    ## indRel                        -2.2607e-01  3.5646e-02 -6.3420 2.268e-10
    ## populationsum                  6.0041e-05  3.6991e-04  0.1623 0.8710601
    ## factor(KE_IBC)Baldia           1.0385e-01  2.3773e-02  4.3683 1.252e-05
    ## factor(KE_IBC)Bin Qasim        1.3991e-01  2.3989e-02  5.8322 5.471e-09
    ## factor(KE_IBC)Clifton         -6.9616e-02  2.3450e-02 -2.9687 0.0029902
    ## factor(KE_IBC)Defence         -7.3731e-02  2.0282e-02 -3.6353 0.0002776
    ## factor(KE_IBC)F.B Area        -2.6133e-02  2.3323e-02 -1.1205 0.2625153
    ## factor(KE_IBC)Gadap            1.5480e-01  2.5850e-02  5.9884 2.120e-09
    ## factor(KE_IBC)Garden          -4.7080e-02  2.3815e-02 -1.9769 0.0480549
    ## factor(KE_IBC)Gulshan-e-Iqbal -7.9612e-02  2.2488e-02 -3.5402 0.0003998
    ## factor(KE_IBC)Johar-I         -5.0286e-02  2.3364e-02 -2.1523 0.0313765
    ## factor(KE_IBC)Johar-II         4.7854e-02  2.5916e-02  1.8465 0.0648192
    ## factor(KE_IBC)Johar II         1.2927e-01  1.1175e-01  1.1568 0.2473437
    ## factor(KE_IBC)KIMZ             4.5191e-02  2.2515e-02  2.0072 0.0447316
    ## factor(KE_IBC)Korangi          1.4773e-01  2.4785e-02  5.9606 2.514e-09
    ## factor(KE_IBC)Landhi           1.2998e-01  2.8972e-02  4.4865 7.241e-06
    ## factor(KE_IBC)Liaquatabad      3.4781e-02  2.5645e-02  1.3562 0.1750238
    ## factor(KE_IBC)Lyari-I          2.8462e-02  2.5063e-02  1.1356 0.2561188
    ## factor(KE_IBC)Lyari-II         7.4148e-02  2.6700e-02  2.7771 0.0054845
    ## factor(KE_IBC)Malir            9.9056e-02  2.3584e-02  4.2002 2.667e-05
    ## factor(KE_IBC)Nazimabad        2.7152e-02  2.7540e-02  0.9859 0.3241750
    ## factor(KE_IBC)North Karachi    5.0201e-02  2.3624e-02  2.1250 0.0335880
    ## factor(KE_IBC)North Nazimabad -3.0587e-02  2.3918e-02 -1.2788 0.2009555
    ## factor(KE_IBC)Orangi-I         1.6004e-01  2.4502e-02  6.5317 6.501e-11
    ## factor(KE_IBC)Orangi-II        7.4627e-02  2.7886e-02  2.6761 0.0074485
    ## factor(KE_IBC)Saddar          -9.8556e-03  2.1283e-02 -0.4631 0.6433151
    ## factor(KE_IBC)Shah Faisal      2.4236e-02  2.3600e-02  1.0270 0.3044367
    ## factor(KE_IBC)SITE             2.8069e-02  2.1470e-02  1.3073 0.1910959
    ## factor(KE_IBC)Surjani          9.6256e-02  2.4566e-02  3.9183 8.918e-05
    ## factor(KE_IBC)Tipu Sultan     -2.2735e-02  2.2176e-02 -1.0252 0.3052559
    ## factor(KE_IBC)Uthal            2.5299e-01  4.9203e-02  5.1418 2.721e-07
    ## 
    ## Rho: 0.33582, LR test value: 97.191, p-value: < 2.22e-16
    ## Asymptotic standard error: 0.03313
    ##     z-value: 10.136, p-value: < 2.22e-16
    ## Wald statistic: 102.75, p-value: < 2.22e-16

    ## Warning in logLik.sarlm(x): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## 
    ## Log likelihood: 917.1148 for lag model
    ## ML residual variance (sigma squared): 0.012206, (sigma: 0.11048)
    ## Nagelkerke pseudo-R-squared: 0.59379 
    ## Number of observations: 1187 
    ## Number of parameters estimated: 37

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## AIC: -1760.2, (AIC for lm: -1665)
    ## LM test for residual autocorrelation
    ## test value: 6.8117, p-value: 0.009056

``` r
# Calculate confidence intervals

ciLag <- as.data.frame(tidy(lag_losses, conf.int = TRUE))
```

    ## Warning in summary.sarlm(x): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in coef.sarlm(object): install the spatialreg package

    ## Warning in vcov.sarlm(object): install the spatialreg package

``` r
impacts(lag_losses, listw = sf_feeders_filtered_weights)
```

    ## Warning in impacts.sarlm(lag_losses, listw = sf_feeders_filtered_weights):
    ## install the spatialreg package

    ## Warning in intImpacts(rho = rho, beta = beta, P = P, n = n, mu = mu, Sigma =
    ## Sigma, : install the spatialreg package

    ## Warning in print.lagImpact(x): install the spatialreg package

    ## Impact measures (lag, exact):
    ##                                      Direct      Indirect         Total
    ## rescale_tot                    1.769516e-02  8.291186e-03  2.598635e-02
    ## comRel                        -7.022292e-02 -3.290342e-02 -1.031263e-01
    ## resRel                         5.576225e-02  2.612778e-02  8.189003e-02
    ## indRel                        -2.315101e-01 -1.084756e-01 -3.399857e-01
    ## populationsum                  6.148642e-05  2.880987e-05  9.029629e-05
    ## factor(KE_IBC)Baldia           1.063476e-01  4.982989e-02  1.561775e-01
    ## factor(KE_IBC)Bin Qasim        1.432756e-01  6.713276e-02  2.104084e-01
    ## factor(KE_IBC)Clifton         -7.129265e-02 -3.340465e-02 -1.046973e-01
    ## factor(KE_IBC)Defence         -7.550593e-02 -3.537881e-02 -1.108847e-01
    ## factor(KE_IBC)F.B Area        -2.676220e-02 -1.253961e-02 -3.930181e-02
    ## factor(KE_IBC)Gadap            1.585279e-01  7.427932e-02  2.328073e-01
    ## factor(KE_IBC)Garden          -4.821376e-02 -2.259088e-02 -7.080464e-02
    ## factor(KE_IBC)Gulshan-e-Iqbal -8.152934e-02 -3.820112e-02 -1.197305e-01
    ## factor(KE_IBC)Johar-I         -5.149655e-02 -2.412905e-02 -7.562560e-02
    ## factor(KE_IBC)Johar-II         4.900612e-02  2.296215e-02  7.196827e-02
    ## factor(KE_IBC)Johar II         1.323854e-01  6.203007e-02  1.944155e-01
    ## factor(KE_IBC)KIMZ             4.627950e-02  2.168457e-02  6.796407e-02
    ## factor(KE_IBC)Korangi          1.512901e-01  7.088801e-02  2.221782e-01
    ## factor(KE_IBC)Landhi           1.331109e-01  6.236998e-02  1.954808e-01
    ## factor(KE_IBC)Liaquatabad      3.561814e-02  1.668912e-02  5.230725e-02
    ## factor(KE_IBC)Lyari-I          2.914719e-02  1.365711e-02  4.280430e-02
    ## factor(KE_IBC)Lyari-II         7.593288e-02  3.557886e-02  1.115117e-01
    ## factor(KE_IBC)Malir            1.014415e-01  4.753111e-02  1.489727e-01
    ## factor(KE_IBC)Nazimabad        2.780547e-02  1.302844e-02  4.083391e-02
    ## factor(KE_IBC)North Karachi    5.140964e-02  2.408833e-02  7.549796e-02
    ## factor(KE_IBC)North Nazimabad -3.132328e-02 -1.467673e-02 -4.600001e-02
    ## factor(KE_IBC)Orangi-I         1.638955e-01  7.679433e-02  2.406898e-01
    ## factor(KE_IBC)Orangi-II        7.642361e-02  3.580879e-02  1.122324e-01
    ## factor(KE_IBC)Saddar          -1.009286e-02 -4.729076e-03 -1.482193e-02
    ## factor(KE_IBC)Shah Faisal      2.481972e-02  1.162945e-02  3.644917e-02
    ## factor(KE_IBC)SITE             2.874477e-02  1.346855e-02  4.221332e-02
    ## factor(KE_IBC)Surjani          9.857358e-02  4.618731e-02  1.447609e-01
    ## factor(KE_IBC)Tipu Sultan     -2.328263e-02 -1.090923e-02 -3.419186e-02
    ## factor(KE_IBC)Uthal            2.590832e-01  1.213952e-01  3.804784e-01

``` r
sf_feeders_filtered$resid_lagsarlm <- residuals(lag_losses)
```

    ## Warning in residuals.sarlm(lag_losses): install the spatialreg package

``` r
lag_moran <- moran.test(sf_feeders_filtered$resid_lagsarlm, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)

lag_moran$p.value
```

    ## [1] 0.071005

``` r
lag_coefficients <- data.frame(coefficients(lag_losses))
```

    ## Warning in coef.sarlm(lag_losses): install the spatialreg package

``` r
# Coeff cleanup
lag_coefficients <- cbind(Names = rownames(lag_coefficients), lag_coefficients)

# Number rows
rownames(lag_coefficients) <- 1:nrow(lag_coefficients)
```

``` r
lag_estimates <- summary(lag_losses)$Coef[,1]
```

    ## Warning in summary.sarlm(lag_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
lag_estimates <- as.data.frame(lag_estimates)
lag_estimates <- cbind(Names = rownames(lag_estimates), lag_estimates)
rownames(lag_estimates) <- 1:nrow(lag_estimates)


lag_stderror <- summary(lag_losses)$Coef[,2]
```

    ## Warning in summary.sarlm(lag_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
lag_stderror <- as.data.frame(lag_stderror)
lag_stderror <- cbind(Names = rownames(lag_stderror), lag_stderror)
rownames(lag_stderror) <- 1:nrow(lag_stderror)


lag_prz <- summary(lag_losses)$Coef[,4]
```

    ## Warning in summary.sarlm(lag_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
lag_prz <- as.data.frame(lag_prz)
lag_prz <- cbind(Names = rownames(lag_prz), lag_prz)
rownames(lag_prz) <- 1:nrow(lag_prz)

lag_table <- left_join(lag_estimates, lag_stderror)
```

    ## Joining, by = "Names"

``` r
lag_table <- left_join(lag_table, lag_prz)
```

    ## Joining, by = "Names"

### Spatial Error Model

``` r
error_losses <- errorsarlm(losses ~ rescale_tot + comRel + resRel + indRel + populationsum + factor(KE_IBC),  data = sf_feeders_filtered,
                 listw = sf_feeders_filtered_weights,
                 na.action(sf_feeders_filtered_weights),
                 zero.policy = T)
```

    ## Warning in errorsarlm(losses ~ rescale_tot + comRel + resRel + indRel + :
    ## install the spatialreg package

    ## Warning: Function can.be.simmed moved to the spatialreg package

    ## Warning: Function jacobianSetup moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

    ## Warning: Function do_ldet moved to the spatialreg package

``` r
summary(error_losses, Nagelkerke = T)
```

    ## Warning in summary.sarlm(error_losses, Nagelkerke = T): install the spatialreg
    ## package

    ## Warning in residuals.sarlm(obj): install the spatialreg package

    ## Warning in logLik.sarlm(obj): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in print.summary.sarlm(x): install the spatialreg package

    ## 
    ## Call:errorsarlm(formula = losses ~ rescale_tot + comRel + resRel + 
    ##     indRel + populationsum + factor(KE_IBC), data = sf_feeders_filtered, 
    ##     listw = sf_feeders_filtered_weights, na.action = na.action(sf_feeders_filtered_weights), 
    ##     zero.policy = T)
    ## 
    ## Residuals:

    ## Warning in residuals.sarlm(x): install the spatialreg package

    ##        Min         1Q     Median         3Q        Max 
    ## -0.4117876 -0.0638338 -0.0052241  0.0598369  0.5940279 
    ## 
    ## Type: error 
    ## Regions with no neighbours included:
    ##  456 713 734 960 
    ## Coefficients: (asymptotic standard errors) 
    ##                                  Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)                    0.20484092  0.03008295  6.8092 9.814e-12
    ## rescale_tot                    0.01611687  0.00441448  3.6509 0.0002613
    ## comRel                        -0.08335981  0.02339375 -3.5633 0.0003662
    ## resRel                         0.05092833  0.01917404  2.6561 0.0079048
    ## indRel                        -0.24308445  0.03658603 -6.6442 3.049e-11
    ## populationsum                  0.00017174  0.00040648  0.4225 0.6726514
    ## factor(KE_IBC)Baldia           0.14886746  0.03493472  4.2613 2.032e-05
    ## factor(KE_IBC)Bin Qasim        0.20420901  0.03544560  5.7612 8.352e-09
    ## factor(KE_IBC)Clifton         -0.10875298  0.03606203 -3.0157 0.0025637
    ## factor(KE_IBC)Defence         -0.10711325  0.03087094 -3.4697 0.0005210
    ## factor(KE_IBC)F.B Area        -0.02198197  0.03441703 -0.6387 0.5230216
    ## factor(KE_IBC)Gadap            0.20573171  0.03692068  5.5723 2.515e-08
    ## factor(KE_IBC)Garden          -0.04724487  0.03402493 -1.3885 0.1649736
    ## factor(KE_IBC)Gulshan-e-Iqbal -0.10447542  0.03297029 -3.1688 0.0015308
    ## factor(KE_IBC)Johar-I         -0.05461726  0.03527501 -1.5483 0.1215434
    ## factor(KE_IBC)Johar-II         0.06560124  0.03661278  1.7918 0.0731717
    ## factor(KE_IBC)Johar II         0.11930552  0.11206253  1.0646 0.2870419
    ## factor(KE_IBC)KIMZ             0.03975596  0.03322359  1.1966 0.2314552
    ## factor(KE_IBC)Korangi          0.21002945  0.03626082  5.7922 6.948e-09
    ## factor(KE_IBC)Landhi           0.17656140  0.04117645  4.2879 1.804e-05
    ## factor(KE_IBC)Liaquatabad      0.04727874  0.03740654  1.2639 0.2062601
    ## factor(KE_IBC)Lyari-I          0.04625565  0.03642220  1.2700 0.2040899
    ## factor(KE_IBC)Lyari-II         0.07966585  0.03807986  2.0921 0.0364320
    ## factor(KE_IBC)Malir            0.14930107  0.03545301  4.2112 2.540e-05
    ## factor(KE_IBC)Nazimabad        0.03496285  0.04001077  0.8738 0.3822076
    ## factor(KE_IBC)North Karachi    0.06126234  0.03458210  1.7715 0.0764769
    ## factor(KE_IBC)North Nazimabad -0.03179385  0.03660249 -0.8686 0.3850522
    ## factor(KE_IBC)Orangi-I         0.20053673  0.03438699  5.8318 5.485e-09
    ## factor(KE_IBC)Orangi-II        0.13478235  0.04101331  3.2863 0.0010151
    ## factor(KE_IBC)Saddar          -0.03425069  0.03162813 -1.0829 0.2788446
    ## factor(KE_IBC)Shah Faisal      0.04266223  0.03570411  1.1949 0.2321327
    ## factor(KE_IBC)SITE             0.00139591  0.03154380  0.0443 0.9647027
    ## factor(KE_IBC)Surjani          0.15855934  0.03673717  4.3160 1.588e-05
    ## factor(KE_IBC)Tipu Sultan     -0.03514497  0.03207858 -1.0956 0.2732582
    ## factor(KE_IBC)Uthal            0.42466554  0.07265851  5.8447 5.076e-09
    ## 
    ## Lambda: 0.43319, LR test value: 110.53, p-value: < 2.22e-16
    ## Asymptotic standard error: 0.036119
    ##     z-value: 11.993, p-value: < 2.22e-16
    ## Wald statistic: 143.84, p-value: < 2.22e-16

    ## Warning in logLik.sarlm(x): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## 
    ## Log likelihood: 923.7848 for error model
    ## ML residual variance (sigma squared): 0.011871, (sigma: 0.10896)
    ## Nagelkerke pseudo-R-squared: 0.59833 
    ## Number of observations: 1187 
    ## Number of parameters estimated: 37

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## AIC: -1773.6, (AIC for lm: -1665)

``` r
# Calculate confidence intervals

ciError <- as.data.frame(tidy(error_losses, conf.int = TRUE))
```

    ## Warning in summary.sarlm(x): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in coef.sarlm(object): install the spatialreg package

    ## Warning in vcov.sarlm(object): install the spatialreg package

``` r
sf_feeders_filtered$resid_error <- residuals(error_losses)
```

    ## Warning in residuals.sarlm(error_losses): install the spatialreg package

``` r
error_moran <- moran.test(sf_feeders_filtered$resid_error, 
           sf_feeders_filtered_weights, 
           adjust.n = TRUE, 
           zero.policy = TRUE)

error_moran$p.value
```

    ## [1] 0.6638597

``` r
error_coefficients <- data.frame(coefficients(error_losses))
```

    ## Warning in coef.sarlm(error_losses): install the spatialreg package

``` r
# Coeff cleanup
error_coefficients <- cbind(Names = rownames(error_coefficients), error_coefficients)

# Number rows
rownames(error_coefficients) <- 1:nrow(error_coefficients)
```

``` r
error_estimates <- summary(error_losses)$Coef[,1]
```

    ## Warning in summary.sarlm(error_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
error_estimates <- as.data.frame(error_estimates)
error_estimates <- cbind(Names = rownames(error_estimates), error_estimates)
rownames(error_estimates) <- 1:nrow(error_estimates)


error_stderror <- summary(error_losses)$Coef[,2]
```

    ## Warning in summary.sarlm(error_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
error_stderror <- as.data.frame(error_stderror)
error_stderror <- cbind(Names = rownames(error_stderror), error_stderror)
rownames(error_stderror) <- 1:nrow(error_stderror)

error_prz <- summary(error_losses)$Coef[,4]
```

    ## Warning in summary.sarlm(error_losses): install the spatialreg package

    ## Warning in Wald1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

    ## Warning in LR1.sarlm(object): install the spatialreg package

    ## Warning in logLik.sarlm(object): install the spatialreg package

    ## Warning in residuals.sarlm(object): install the spatialreg package

``` r
error_prz <- as.data.frame(error_prz)
error_prz <- cbind(Names = rownames(error_prz), error_prz)
rownames(error_prz) <- 1:nrow(error_prz)

error_table <- left_join(error_estimates, error_stderror)
```

    ## Joining, by = "Names"

``` r
error_table <- left_join(error_table, error_prz)
```

    ## Joining, by = "Names"

## Table of Results

This file contains tables of results obtained from the spatial models.

``` r
options(digits = 10)
```

``` r
NL_OLS_table <- OLS_table
```

``` r
# Recode rows

NL_OLS_table <- NL_OLS_table %>% 
  mutate(term = recode(term, 
                        `(Intercept)` = "Intercept",
                        tot = "Total",
                        comRel = "Commercial %",
                        resRel = "Residential %",
                        indRel = "Industrial %",
                        populationsum = "Population", 
                         `factor(KE_IBC)Baldia` = "Baldia",
                         `factor(KE_IBC)Bin Qasim` = "Bin Qasim",
                         `factor(KE_IBC)Clifton` = "Clifton",
                         `factor(KE_IBC)Defence` = "Defence",
                         `factor(KE_IBC)F.B Area` = "F.B Area",
                         `factor(KE_IBC)Gadap` = "Gadap",
                         `factor(KE_IBC)Garden` = "Garden",
                         `factor(KE_IBC)Gulshan-e-Iqbal` = "Gulshan-e-Iqbal",
                         `factor(KE_IBC)Johar-I` = "Johar-I",
                         `factor(KE_IBC)Johar-II` = "Johar-II",
                         `factor(KE_IBC)Johar II` = "Johar II",
                         `factor(KE_IBC)KIMZ` = "KIMZ",
                         `factor(KE_IBC)Korangi` = "Korangi",
                         `factor(KE_IBC)Landhi` = "Landhi",
                         `factor(KE_IBC)Liaquatabad` = "Liaquatabad",
                         `factor(KE_IBC)Lyari-I` = "Lyari-I",
                         `factor(KE_IBC)Lyari-II` = "Lyari-II",
                         `factor(KE_IBC)Malir` = "Malir",
                         `factor(KE_IBC)Nazimabad` = "Nazimabad",
                         `factor(KE_IBC)North Karachi` = "North Karachi",
                         `factor(KE_IBC)North Nazimabad` = "North Nazimabad",
                         `factor(KE_IBC)Orangi-I` = "Orangi-I",
                         `factor(KE_IBC)Orangi-II` = "Orangi-II",
                         `factor(KE_IBC)Saddar` = "Saddar",
                         `factor(KE_IBC)Shah Faisal` = "Shah Faisal",
                         `factor(KE_IBC)SITE` = "SITE",
                         `factor(KE_IBC)Surjani` = "Surjani",
                         `factor(KE_IBC)Tipu Sultan` = "Tipu Sultan",
                         `factor(KE_IBC)Uthal` = "Uthal"
                         ))
```

``` r
# round values

NL_OLS_table <- NL_OLS_table %>% 
  mutate(estimate = round(estimate, 5)) %>% 
  mutate(pvalue = round(p.value, 5))

NL_OLS_table <- NL_OLS_table %>% 
  select(term, estimate, pvalue)

NL_OLS_table <- NL_OLS_table %>% 
  rename(Names = term,
         OLSestimates = estimate,
         OLSpvalue = pvalue)
```

``` r
# Recode rows

ciOLS <- ciOLS %>% 
  mutate(term = recode(term, 
                        `(Intercept)` = "Intercept",
                        tot = "Total",
                        comRel = "Commercial %",
                        resRel = "Residential %",
                        indRel = "Industrial %",
                        populationsum = "Population", 
                         `factor(KE_IBC)Baldia` = "Baldia",
                         `factor(KE_IBC)Bin Qasim` = "Bin Qasim",
                         `factor(KE_IBC)Clifton` = "Clifton",
                         `factor(KE_IBC)Defence` = "Defence",
                         `factor(KE_IBC)F.B Area` = "F.B Area",
                         `factor(KE_IBC)Gadap` = "Gadap",
                         `factor(KE_IBC)Garden` = "Garden",
                         `factor(KE_IBC)Gulshan-e-Iqbal` = "Gulshan-e-Iqbal",
                         `factor(KE_IBC)Johar-I` = "Johar-I",
                         `factor(KE_IBC)Johar-II` = "Johar-II",
                         `factor(KE_IBC)Johar II` = "Johar II",
                         `factor(KE_IBC)KIMZ` = "KIMZ",
                         `factor(KE_IBC)Korangi` = "Korangi",
                         `factor(KE_IBC)Landhi` = "Landhi",
                         `factor(KE_IBC)Liaquatabad` = "Liaquatabad",
                         `factor(KE_IBC)Lyari-I` = "Lyari-I",
                         `factor(KE_IBC)Lyari-II` = "Lyari-II",
                         `factor(KE_IBC)Malir` = "Malir",
                         `factor(KE_IBC)Nazimabad` = "Nazimabad",
                         `factor(KE_IBC)North Karachi` = "North Karachi",
                         `factor(KE_IBC)North Nazimabad` = "North Nazimabad",
                         `factor(KE_IBC)Orangi-I` = "Orangi-I",
                         `factor(KE_IBC)Orangi-II` = "Orangi-II",
                         `factor(KE_IBC)Saddar` = "Saddar",
                         `factor(KE_IBC)Shah Faisal` = "Shah Faisal",
                         `factor(KE_IBC)SITE` = "SITE",
                         `factor(KE_IBC)Surjani` = "Surjani",
                         `factor(KE_IBC)Tipu Sultan` = "Tipu Sultan",
                         `factor(KE_IBC)Uthal` = "Uthal"
                         ))
```

``` r
# Keep only the term and confidence intervals

ciOLS <- ciOLS %>% 
  select(term, conf.low, conf.high)

# Round the values off

ciOLS <- ciOLS %>% 
  mutate(conf.low = round(conf.low, 5)) %>% 
  mutate(conf.high = round(conf.high, 5))

# Merge the columns into one

ciOLS <- ciOLS %>% 
  unite(OLSCI,
        2:3,
        remove = TRUE,
        sep = ", ")
```

``` r
# Join Tables

NL_OLS_table <- left_join(NL_OLS_table, ciOLS, by = c("Names" = "term"))
```

``` r
### Spatial Error Model

NL_error_table <- error_table
```

``` r
# Recode rows

NL_error_table <- NL_error_table %>% 
  mutate(Names = recode(Names, 
                        `(Intercept)` = "Intercept",
                        rescale_tot = "Total",
                        comRel = "Commercial %",
                        resRel = "Residential %",
                        indRel = "Industrial %",
                        populationsum = "Population",
                         `factor(KE_IBC)Baldia` = "Baldia",
                         `factor(KE_IBC)Bin Qasim` = "Bin Qasim",
                         `factor(KE_IBC)Clifton` = "Clifton",
                         `factor(KE_IBC)Defence` = "Defence",
                         `factor(KE_IBC)F.B Area` = "F.B Area",
                         `factor(KE_IBC)Gadap` = "Gadap",
                         `factor(KE_IBC)Garden` = "Garden",
                         `factor(KE_IBC)Gulshan-e-Iqbal` = "Gulshan-e-Iqbal",
                         `factor(KE_IBC)Johar-I` = "Johar-I",
                         `factor(KE_IBC)Johar-II` = "Johar-II",
                         `factor(KE_IBC)Johar II` = "Johar II",
                         `factor(KE_IBC)KIMZ` = "KIMZ",
                         `factor(KE_IBC)Korangi` = "Korangi",
                         `factor(KE_IBC)Landhi` = "Landhi",
                         `factor(KE_IBC)Liaquatabad` = "Liaquatabad",
                         `factor(KE_IBC)Lyari-I` = "Lyari-I",
                         `factor(KE_IBC)Lyari-II` = "Lyari-II",
                         `factor(KE_IBC)Malir` = "Malir",
                         `factor(KE_IBC)Nazimabad` = "Nazimabad",
                         `factor(KE_IBC)North Karachi` = "North Karachi",
                         `factor(KE_IBC)North Nazimabad` = "North Nazimabad",
                         `factor(KE_IBC)Orangi-I` = "Orangi-I",
                         `factor(KE_IBC)Orangi-II` = "Orangi-II",
                         `factor(KE_IBC)Saddar` = "Saddar",
                         `factor(KE_IBC)Shah Faisal` = "Shah Faisal",
                         `factor(KE_IBC)SITE` = "SITE",
                         `factor(KE_IBC)Surjani` = "Surjani",
                         `factor(KE_IBC)Tipu Sultan` = "Tipu Sultan",
                         `factor(KE_IBC)Uthal` = "Uthal"
                         ))
```

``` r
# round values

NL_error_table <- NL_error_table %>% 
  mutate(estimates = round(error_estimates, 5)) %>% 
  mutate(prz = round(error_prz, 5))

NL_error_table <- NL_error_table %>% 
  select(-error_estimates, -error_prz, -error_stderror)
```

``` r
# Recode rows

ciError <- ciError %>% 
  mutate(term = recode(term, 
                        `(Intercept)` = "Intercept",
                        rescale_tot = "Total",
                        comRel = "Commercial %",
                        resRel = "Residential %",
                        indRel = "Industrial %",
                        populationsum = "Population", 
                         `factor(KE_IBC)Baldia` = "Baldia",
                         `factor(KE_IBC)Bin Qasim` = "Bin Qasim",
                         `factor(KE_IBC)Clifton` = "Clifton",
                         `factor(KE_IBC)Defence` = "Defence",
                         `factor(KE_IBC)F.B Area` = "F.B Area",
                         `factor(KE_IBC)Gadap` = "Gadap",
                         `factor(KE_IBC)Garden` = "Garden",
                         `factor(KE_IBC)Gulshan-e-Iqbal` = "Gulshan-e-Iqbal",
                         `factor(KE_IBC)Johar-I` = "Johar-I",
                         `factor(KE_IBC)Johar-II` = "Johar-II",
                         `factor(KE_IBC)Johar II` = "Johar II",
                         `factor(KE_IBC)KIMZ` = "KIMZ",
                         `factor(KE_IBC)Korangi` = "Korangi",
                         `factor(KE_IBC)Landhi` = "Landhi",
                         `factor(KE_IBC)Liaquatabad` = "Liaquatabad",
                         `factor(KE_IBC)Lyari-I` = "Lyari-I",
                         `factor(KE_IBC)Lyari-II` = "Lyari-II",
                         `factor(KE_IBC)Malir` = "Malir",
                         `factor(KE_IBC)Nazimabad` = "Nazimabad",
                         `factor(KE_IBC)North Karachi` = "North Karachi",
                         `factor(KE_IBC)North Nazimabad` = "North Nazimabad",
                         `factor(KE_IBC)Orangi-I` = "Orangi-I",
                         `factor(KE_IBC)Orangi-II` = "Orangi-II",
                         `factor(KE_IBC)Saddar` = "Saddar",
                         `factor(KE_IBC)Shah Faisal` = "Shah Faisal",
                         `factor(KE_IBC)SITE` = "SITE",
                         `factor(KE_IBC)Surjani` = "Surjani",
                         `factor(KE_IBC)Tipu Sultan` = "Tipu Sultan",
                         `factor(KE_IBC)Uthal` = "Uthal"
                         ))
```

``` r
# Keep only the term and confidence intervals

ciError <- ciError %>% 
  select(term, conf.low, conf.high)

# Round the values off

ciError <- ciError %>% 
  mutate(conf.low = round(conf.low, 5)) %>% 
  mutate(conf.high = round(conf.high, 5))

# Merge the columns into one

ciError <- ciError %>% 
  unite(errorCI,
        2:3,
        remove = TRUE,
        sep = ", ")
```

``` r
# Join Tables

NL_error_table <- left_join(NL_error_table, ciError, by = c("Names" = "term"))
```

``` r
### Join Tables

results <- left_join(NL_OLS_table, NL_error_table, by = c("Names" = "Names"))
```

``` r
### Add model fit stats

Names <- c("Summary Statistics", "Adjusted R-Squared", "Moran's I", "AIC", "Log-likelihood")
OLSestimates <- c(NA, 0.546, 0.200, -1665.0386, 868.5193)
OLSpvalue <- c(NA, NA, NA, NA, NA)
estimates <- c(NA, 0.598, -0.008, -1773.5696, 923.7848)
prz <- c(NA, NA, NA, NA, NA)

modelStats <- data.frame(Names, OLSestimates, OLSpvalue, estimates, prz)
```

Table 1: Results from Spatial Error Model

``` r
### Viz

NL_error_results <- results %>% 
  slice(1:6)

NL_error_results <- bind_rows(NL_error_results, modelStats)

NL_flex_error_results <- flextable(NL_error_results, col_keys = c("Names", 
                                                    "OLSestimates",
                                                    "OLSpvalue",
                                                    "OLSCI",
                                                    "estimates",
                                                    "prz",
                                                    "errorCI"))

NL_flex_error_results <- colformat_num(NL_flex_error_results, 
                                       col_keys = "OLSpvalue", 
                                       i = ~ OLSpvalue < 0.05,
                                       suffix = "*",
                                       digits = 5)

NL_flex_error_results <- colformat_num(NL_flex_error_results, 
                                       col_keys = "prz", 
                                       i = ~ prz < 0.05,
                                       suffix = "*",
                                       digits = 5)

NL_flex_error_results <- set_header_labels(NL_flex_error_results,
                                      Names = "Term",
                                      OLSestimates = "OLS Model",
                                      OLSpvalue = "OLS Model",
                                      OLSCI = "OLS Model",
                                      estimates = "Spatial Error Model",
                                      prz = "Spatial Error Model",
                                      errorCI = "Spatial Error Model")

NL_flex_error_results <- merge_at(NL_flex_error_results,
                                  i = 1,
                                  j = 2:4,
                                  part = "header")

NL_flex_error_results <- merge_at(NL_flex_error_results,
                                  i = 1,
                                  j = 5:7,
                                  part = "header")

NL_flex_error_results <- add_header_row(NL_flex_error_results,
                                        values = c("",
                                                   "Estimate",
                                                   "p",
                                                   "95% CI",
                                                   "Estimate",
                                                   "Pr(>|z|)",
                                                   "95% CI"),
                                                   top = FALSE)

NL_flex_error_results <- add_footer_lines(NL_flex_error_results,
                                     values = c("* p < 0.05",
                                                "* Pr(>|z|) < 0.05"))

NL_flex_error_results <- align(NL_flex_error_results, 
                          align = "center",
                          part = "header")

NL_flex_error_results <- fontsize(NL_flex_error_results, 
                             part = "header",
                             size = 12)

NL_flex_error_results <- autofit(NL_flex_error_results, add_w = 0, add_h = 0)

NL_flex_error_results <- add_header_lines(NL_flex_error_results,
                                     values = c("Coefficient estimates from different modelling approaches"))

save_as_docx(NL_flex_error_results,
             path = here::here("analysis/07-Sup-Table-of-Results.docx"))
```
