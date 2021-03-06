04-Figures-Descriptive
================
(11 June, 2021)

In this file, visualizations are created showing the electricity
received and losses across Karachi.

## Received and Losses Map

### Load Data

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

### Elec Data

``` r
sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))

sf_feeders_info$GRIDID <- as.integer(sf_feeders_info$GRIDID)
```

``` r
feeders_year <- read.csv(here::here("analysis/data/raw_data/feeders_year.csv")) %>% 
  select(-X)

feeders_year$GRIDID <- as.integer(feeders_year$GRIDID)
```

``` r
sf_feeder_area_elec <- readRDS(here::here("analysis/data/derived_data/sf_feeder_area_elec.rds"))

sf_feeder_area_elec$GRIDID <- as.integer(sf_feeder_area_elec$GRIDID)
```

``` r
feeder_area_elec <- sf_feeder_area_elec %>% 
  st_set_geometry(NULL)

feeder_area_elec <- as.data.frame(feeder_area_elec)

feeder_area_elec <- feeder_area_elec %>% 
  dplyr::select(GRIDID, FEEDER_NAM, KE_IBC, KE_REGION, elec_norm_area)
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
#`geometry` is added to `feeders_year` and then saved as `sf_feeders`.

sf_feeders <- left_join(sf_feeders_info, 
                            feeders_year, 
                            by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))

# `feeder_area_elec` or the electricity density is added to `sf_feeders`.

sf_feeders <- left_join(sf_feeders, 
                        feeder_area_elec,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))

sf_feeders <- left_join(sf_feeders, 
                        feeder_population,
                        by = c("GRIDID" = "GRIDID",
                                   "FEEDER_NAM" = "FEEDER_NAM",
                                   "KE_IBC" = "KE_IBC",
                                   "KE_REGION" = "KE_REGION"))
```

### Filter Data

``` r
# Filter data to keep only the core area of Karachi

sf_feeders_filtered <- sf_feeders %>% 
  filter(elec_norm_area > 1) %>% 
  filter(losses > 0)
  
# Clip Geometry

sf_feeders_filtered <- st_intersection(khi_land, sf_feeders_filtered)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### Visualization

#### Received Map

``` r
# Calculate % values for tmap legends

sf_feeders_filtered <- sf_feeders_filtered %>% 
  mutate(receivedrel = (received/sum(received))*100)
```

``` r
# Custom Palette

myPal <- c("#feedde", "#fdd0a2", "#fdae6b", "#fd8d3c", "#f16913", "#d94801", "#8c2d04")
```

``` r
# Visualisation

viz_received_map <- sf_feeders_filtered %>%
  group_by(received, GRIDID) %>%
  tm_shape() + 
  tm_fill(col = "received", 
          colorNA = NULL,
          colorNULL = NULL,
          palette = myPal, 
          title = "",
          breaks = c(00036432.00, 
                     03814258.80, 
                     07209724.60, 
                     10107165.35, 
                     12781678.40, 
                     15887889.60, 
                     19950542.40, 
                     40646516.40),
          legend.hist = F,
          legend.is.portrait = T,
          legend.format = c(text.align="center"))
```

``` r
# Combined Visualisation

viz_received_layout <- viz_padding +
  viz_ibc_fill +
  viz_received_map + 
  viz_basemap +
  viz_feeders_boundaries + 
  viz_ibc_boundaries + 
  tm_layout(title = "Electricity Consumption per Feeder", 
            title.size = 0.90, 
            title.position = c("right","top"),
            title.bg.color = "white",
            frame = F, 
            legend.show = T, 
            legend.outside = F,
            legend.bg.color = "white",
            legend.title.size = 0.75, 
            legend.text.size = 0.85,
            outer.margins = c(0, 0, 0, 0)) + 
  tm_legend(position = c("right", "top"))
```

#### Losses Map

``` r
# Custom Palette

myPal2 <- c("#feedde", "#fdd0a2", "#fdae6b", "#fd8d3c", "#e6550d", "#a63603")
```

``` r
sf_feeders_filtered <- sf_feeders_filtered %>% 
  mutate(lossesperc = losses * 100)
```

``` r
# Visualisation

viz_losses_map <- sf_feeders_filtered %>%
  group_by(losses, GRIDID) %>%
  tm_shape() + 
  tm_fill(col = "lossesperc", 
          breaks = c(-180, 0, 015.0, 030.0, 045.00, 060.00, 075.00, 095.00), 
          midpoint = 0, 
          palette = myPal, 
          title = "",
          legend.hist = F,
          legend.format=list(text.align="center",
                             fun=function(x) paste0(formatC(x, 
                                                            digits=0, 
                                                            format="f")," %"))) + 
  tm_legend(position = c("left", "top"))
```

``` r
# Combined Visualisation

viz_losses_layout <- viz_padding +
  viz_ibc_fill +
  viz_losses_map + 
  viz_basemap + 
  viz_feeders_boundaries + 
  viz_ibc_boundaries + 
  tm_layout(title = "Electricity Losses per Feeder", 
            title.size = 0.90, 
            title.position = c("right","top"),
            title.bg.color = "white",
            frame = F, 
            legend.show = T, 
            legend.outside = F,
            legend.bg.color = "white",
            legend.title.size = 0.75, 
            legend.text.size = 0.85,
            outer.margins = c(0, 0, 0, 0)) + 
  tm_legend(position = c("right", "top"))
```

## Location Map

### Load Data

``` r
# Read Shape File (IBC)

ibc_sf <- readRDS(here::here("analysis/data/raw_data/ibc-voronoi.rds"))

# Set Geometry

ibc_sf <- ibc_sf %>%
  st_as_sf(ibc_sf$geometry, CRS = 32642)
```

### Base Map

``` r
# Clip Geometry

ibc_sf_clip <- st_intersection(khi_land, ibc_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
#Labels
# 
# viz_loc_labels <- mapview(ibc_sf) %>% 
#   editMap() %>%
#   pluck("finished")
# 
# saveRDS(viz_loc_labels, here::here("analysis/data/derived_data/viz_loc_labels.rds"))

viz_loc_labels <- readRDS(here::here("analysis/data/derived_data/viz_loc_labels.rds"))

viz_loc_labels <- viz_loc_labels %>% 
  mutate(ID = row_number())

viz_loc_labels <- viz_loc_labels %>% 
  mutate(text = case_when((ID == 1) ~ "Clifton",
                          (ID == 2) ~ "Defence",
                          (ID == 3) ~ "Orangi",
                          (ID == 4) ~ "Jinnah International Airport",
                          (ID == 5) ~ "Karachi Northern Bypass",
                          (ID == 6) ~ "Karachi Hyderabad Expressway"))
```

``` r
# Clip Geometry

sf_feeders_info_clip <- st_intersection(khi_land, sf_feeders_info)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### Map

``` r
# Prepare a separate padding map

viz_loc_padding <- tm_shape(sf_feeders_info_clip) +
  tm_borders() +
  tm_layout(frame = F)
```

``` r
viz_loc <- viz_loc_padding +
  tm_shape(ibc_sf_clip) +
  tm_polygons(col = "#f0f0f0") +
  viz_basemap + 
  viz_feeders_boundaries + 
  viz_ibc_boundaries +
  tm_shape(viz_loc_labels) +
  tm_bubbles(col = "#542788",
             size = 0.75,
             border.lwd = NA) +
  tm_text("ID",
          col = "white",
          size = 0.90) +
  tm_layout(title = "Karachi, Pakistan", 
            title.size = 1.25, 
            title.position = c("left", "top"),
            frame = F, 
            legend.show = F,
            outer.margins = c(0, 0, 0, 0)) +
  tm_credits("1. Clifton     2. Defence     3. Orangi New Town\n4. Jinnah International Airport     5. Karachi Northern Bypass\n6. Karachi Hyderabad Expressway",
             size = 1.0,
             just = "left",
             position = c("left", "top"))
```

## Arrange Panels

``` r
upperRow <- tmap_arrange(viz_loc,
             nrow = 1,
             outer.margins = 0)
```

``` r
lowerRow <- tmap_arrange(viz_received_layout, 
             viz_losses_layout,
             nrow = 1,
             outer.margins = 0)
```

``` r
# Export

tmap_save(tm = upperRow, 
          filename = here::here("analysis/figures/figure01a.png"), 
          dpi = 300, 
          width = 330,
          height = 165,
          units = "mm")
```

    ## Map saved to G:\00_Git Repos\electricitylossgovernance\analysis\figures\figure01a.png

    ## Resolution: 3897.638 by 1948.819 pixels

    ## Size: 12.99213 by 6.496063 inches (300 dpi)

``` r
# Export

tmap_save(tm = lowerRow, 
          filename = here::here("analysis/figures/figure01b.png"), 
          dpi = 300, 
          width = 330,
          height = 165,
          units = "mm")
```

    ## Map saved to G:\00_Git Repos\electricitylossgovernance\analysis\figures\figure01b.png

    ## Resolution: 3897.638 by 1948.819 pixels

    ## Size: 12.99213 by 6.496063 inches (300 dpi)

``` r
# Load the two halves of the panel back in

panelTop <- image_read(here::here("analysis/figures/figure01a.png"))
panelBottom <- image_read(here::here("analysis/figures/figure01b.png"))

# Arrange the two images

panel <- c(panelTop, panelBottom)

panelArranged <- image_append(image_scale(panel, "4000"), stack = TRUE)

# Save the combined image

image_write(panelArranged,
            here::here("analysis/figures/figure01.png"),
            format = "png")
```
