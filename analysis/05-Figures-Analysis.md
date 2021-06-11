05-Figures-Analysis
================
(11 June, 2021)

Visualizations for the residuals (based on the models) are generated
from this file.

## Residuals Map

### Data Wrangling

``` r
sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))
```

``` r
# Karachi Land

khi_land <- readRDS(here::here("analysis/data/raw_data/KHI_land.rds"))
```

``` r
viz_basemap <- readRDS(here::here("analysis/data/derived_data/viz_basemap.rds"))
```

``` r
viz_feeders_boundaries <- readRDS(here::here("analysis/data/derived_data/viz_feeders_boundaries.rds"))
```

``` r
viz_ibc_fill <- readRDS(here::here("analysis/data/derived_data/viz_ibc_fill.rds"))
```

``` r
viz_ibc_boundaries <- readRDS(here::here("analysis/data/derived_data/viz_ibc_boundaries.rds"))
```

``` r
viz_padding <- readRDS(here::here("analysis/data/derived_data/viz_padding.rds"))
```

*model specific*

``` r
model_residuals <- readRDS(here::here("analysis/data/derived_data/res_model_residuals.rds"))
```

``` r
# Clip Geometry

model_residuals_clip <- st_intersection(khi_land, model_residuals)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Custom Palette

myPal3 <- c("#e66101", "#fdb863", "#d9d9d9", "#b2abd2", "#5e3c99")
```

### Visualization

``` r
# Visualisation

viz_error_resid <- model_residuals_clip %>%
  filter(!is.na(GRIDID)) %>% 
  group_by(resid_error, GRIDID) %>%
  tm_shape() + 
  tm_fill(
    col = "resid_error",
    colorNA = "red",
    showNA = F,
    border.alpha = 0.5,
    border.col = "black",
    palette = myPal3,
    breaks = c(-1.97,-0.09,-0.03, 0.02, 0.10, 0.62),
    midpoint = -0.03,
    title = "",
    legend.hist = F) + 
  tm_legend(position = c("left", "top"))
```

``` r
# Combined Visualisation

viz_error_resid_layout <- viz_padding +
  viz_ibc_fill +
  viz_basemap + 
  viz_error_resid + 
  viz_feeders_boundaries + 
  viz_ibc_boundaries + 
  tm_layout(title = "Residuals from the Spatial Error Model", 
            title.size = 0.60, 
            title.position = c("right", "top"),
            title.bg.color = "white",
            frame = F, 
            legend.show = T, 
            legend.outside = F,
            legend.bg.color = "white",
            legend.title.size = 0.45, 
            legend.text.size = 0.55,
            outer.margins = c(0, 0, 0, 0)) + 
  tm_legend(position = c("right", "top"))
```

``` r
# Export

tmap_save(tm = viz_error_resid_layout, 
          filename = here::here("analysis/figures/figure02.png"), 
          dpi = 300, 
          width = 165.2, 
          units = "mm")
```

    ## Map saved to G:\00_Git Repos\electricitylossgovernance\analysis\figures\figure02.png

    ## Resolution: 1951.181 by 1339.45 pixels

    ## Size: 6.503937 by 4.464833 inches (300 dpi)

## Lyari Residuals

``` r
viz_lyari_ibc_bound <- readRDS(here::here("analysis/data/derived_data/viz_lyari_ibc_bound.rds"))
```

``` r
viz_lyari_roads <- readRDS(here::here("analysis/data/derived_data/viz_lyari_roads.rds"))
```

``` r
viz_lyari_boundaries <- readRDS(here::here("analysis/data/derived_data/viz_lyari_boundaries.rds"))
```

``` r
viz_lyari_fill <- readRDS(here::here("analysis/data/derived_data/viz_lyari_fill.rds"))
```

``` r
alt_feeder_names <- readRDS(here::here("analysis/data/derived_data/viz_feeder_names.rds"))

alt_feeder_names <- alt_feeder_names %>% 
  dplyr::select(left_name, right_name_v2)
```

``` r
Lyari <- model_residuals_clip %>%
  filter(str_detect(KE_IBC, "Lyari"))
```

``` r
Lyari <- left_join(Lyari, alt_feeder_names, c("FEEDER_NAM" = "right_name_v2"))
```

``` r
LyariNames <- Lyari %>% 
  select(FEEDER_NAM, geometry)

LyariNames <- LyariNames %>% 
  filter(FEEDER_NAM == "Machar Colony" |
           FEEDER_NAM == "Union Cold Storage" |
           FEEDER_NAM == "Denso Road" |
           FEEDER_NAM == "Behar Colony" |
           FEEDER_NAM == "Gulistan Colony")

LyariNames <- st_cast(LyariNames, "POLYGON")
```

    ## Warning in st_cast.MULTIPOLYGON(X[[i]], ...): polygon from first part only

``` r
LyariNames <- LyariNames %>% 
  mutate(label = case_when(FEEDER_NAM == "Machar Colony" ~ "1",
                           FEEDER_NAM == "Union Cold Storage" ~ "2",
                           FEEDER_NAM == "Denso Road" ~ "3",
                           FEEDER_NAM == "Behar Colony" ~ "4a",
                           FEEDER_NAM == "Gulistan Colony" ~ "4b"))

# Map it

viz_lyari_names <- tm_shape(LyariNames) +
  tm_bubbles(col = "#ffffff",
             size = 0.60,
             border.col = "#000000") +
  tm_text("label",
          col = "#000000",
          size = 0.60)
```

``` r
viz_lyari_resid <- Lyari %>%
  filter(!is.na(GRIDID)) %>%
  group_by(resid_error, GRIDID) %>%
  tm_shape() +
  tm_fill(
    col = "resid_error",
    colorNA = "red",
    showNA = F,
    border.alpha = 0.5,
    border.col = "black",
    palette = myPal3,
    breaks = c(-1.97,-0.09,-0.03, 0.02, 0.10, 0.62),
    midpoint = -0.03,
    title = "",
    legend.hist = F) +
  tm_legend(position = c("right", "bottom"))
```

``` r
viz_lyari_resid_layout <- viz_lyari_fill +
  viz_basemap +
  viz_lyari_resid +
  viz_lyari_ibc_bound +
  viz_lyari_boundaries +
  viz_lyari_names +
  tm_layout(title = "Lyari Residuals", 
            title.size = 0.90, 
            frame = F, 
            legend.show = T, 
            legend.outside = F,
            legend.title.size = 0.55, 
            legend.text.size = 0.65,
            legend.bg.color = "white",
            legend.title.color = "black",
            outer.margins = c(0, 0, 0, 0)) +
  tm_legend(position = c("left", "top"))
```

``` r
# Export

tmap_save(tm = viz_lyari_resid_layout,
          filename = here::here("analysis/figures/figure04.png"), 
          dpi = 300, 
          width = 165.2, 
          units = "mm")
```

    ## Map saved to G:\00_Git Repos\electricitylossgovernance\analysis\figures\figure04.png

    ## Resolution: 1951.181 by 2242.956 pixels

    ## Size: 6.503937 by 7.47652 inches (300 dpi)

## IBC Coefficients

### Data Wrangling

``` r
# Read Shape File (IBC)

ibc_sf <- readRDS(here::here("analysis/data/raw_data/ibc-voronoi.rds"))
```

``` r
viz_ibc_labels <- readRDS(here::here("analysis/data/derived_data/viz_ibc_labels.rds"))
```

``` r
error_coefficients <- readRDS(here::here("analysis/data/derived_data/res_error_coefficients.rds"))
```

``` r
# Select

ibc_coefficients <- error_coefficients %>%
  filter(row_number() > 6)
```

``` r
# Recode rows

ibc_coefficients <- ibc_coefficients %>% 
  mutate(KE_IBC = recode(Names, 
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
# Join Datasets to show co-efficients on map

ibc_coefficients_joined <- left_join(ibc_coefficients, 
                              sf_feeders_info, 
                              by = c("KE_IBC" = "KE_IBC"))

ibc_coefficients_joined <- ibc_coefficients_joined %>%
  st_as_sf(ibc_coefficients_joined$geometry, CRS = 32642)

ibc_coefficients_clipped <- st_intersection(khi_land, ibc_coefficients_joined)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Join datasets to show significant IBCs

ibc_boundaries_joined <- left_join(ibc_coefficients,
                                   ibc_sf,
                                   by = c("KE_IBC" = "KE_IBC"))

ibc_boundaries_joined <- ibc_boundaries_joined %>%
  st_as_sf(ibc_boundaries_joined$geometry, CRS = 32642)

ibc_boundaries_clipped <- st_intersection(khi_land, ibc_boundaries_joined)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Clip Geometry

sf_feeders_info_clip <- st_intersection(khi_land, sf_feeders_info)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### Visualization

``` r
viz_ibc_padding_map <- tm_shape(sf_feeders_info_clip) +
  tm_polygons(col = "white")
```

``` r
# Significant IBCs

viz_ibc_significant <- ibc_boundaries_clipped %>% 
  filter(error_prz < 0.05) %>% 
  tm_shape() +
  tm_borders(col = "black",
             lwd = 1.8)
```

``` r
# InSignificant IBCs

viz_ibc_insignificant <- ibc_boundaries_clipped %>% 
  filter(error_prz > 0.05) %>% 
  tm_shape() +
  tm_fill(col = "white",
          alpha = 0.4)
```

``` r
# Custom Palette

myPal4 <- c("#b35806", "#f1a340", "#fee0b6", "#d8daeb", "#998ec3", "#542788")
```

``` r
# Visualisation

viz_ibc_coefficients <- ibc_coefficients_clipped %>%
  group_by(error_estimates, GRIDID) %>%
  tm_shape() + 
  tm_fill(col = "error_estimates",
          palette = myPal4, 
          breaks = c(-0.10, -0.066, 0, 0.080, 0.171, 0.262, 0.353), 
          midpoint = 0,
          legend.hist = F,
          title = "") + 
  tm_legend(position = c("left", "top"))
```

``` r
# Combined Visualisation

viz_coefficients_layout <- viz_ibc_padding_map + 
  viz_ibc_fill + 
  viz_ibc_coefficients + 
  viz_basemap + 
  viz_feeders_boundaries +
  viz_ibc_boundaries +
  viz_ibc_significant +
  viz_ibc_labels +
  tm_layout(title = "IBC Coefficients from the Spatial Error Model", 
            title.size = 0.60, 
            frame = F, 
            legend.show = T, 
            legend.outside = F,
            legend.title.size = 0.65, 
            legend.text.size = 0.55,
            outer.margins = c(0, 0, 0, 0)) + 
  tm_legend(position = c("left", "top"))
```

``` r
# Export

tmap_save(tm = viz_coefficients_layout, 
          filename = here::here("analysis/figures/figure03.png"), 
          dpi = 300, 
          width = 165.2, 
          units = "mm")
```

    ## Warning: Values have found that are less than the lowest break

    ## Warning: Values have found that are less than the lowest break

    ## Map saved to G:\00_Git Repos\electricitylossgovernance\analysis\figures\figure03.png

    ## Resolution: 1951.181 by 1237.463 pixels

    ## Size: 6.503937 by 4.124878 inches (300 dpi)
