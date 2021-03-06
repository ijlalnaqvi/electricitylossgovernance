03-Figures-Prep
================
(07 June, 2021)

In this file, I prepare the base and contextual information for the
figures.

## Base Map

### Data Wrangling

``` r
# Karachi Land

khi_land <- readRDS(here::here("analysis/data/raw_data/KHI_land.rds"))

# Karachi Coastline

khi_coast <- readRDS(here::here("analysis/data/raw_data/KHI_water.rds"))

# Karachi Roads

khi_roads <- readRDS(here::here("analysis/data/raw_data/KHI_roads.rds"))

# Filter & Select Roads

khi_roads <- khi_roads %>%
  filter(highway == "motorway" | highway == "trunk" | highway == "trunk_link" | highway == "primary")

# Filter other layers

khi_roads <- khi_roads %>%
  select(-waterway, -aerialway, -barrier, -man_made, -other_tags)

# Assign weights

khi_roads <- khi_roads %>%
  mutate(lineweight = case_when(
    (highway == "trunk") ~ 3.5,
    (highway == "trunk_link") ~ 3.5,
    (highway == "motorway") ~ 2.5,
    (highway == "primary") ~ 2.5
  ))

# Clip Geometry

khi_roads <- st_intersection(khi_land, khi_roads)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### Visualization

``` r
# Combine Admin and Coast

viz_basemap <- tm_shape(khi_coast) + 
  tm_fill(col = "#9ecae1") +
  tm_shape(khi_roads) + 
  tm_lines(lwd = "lineweight", 
           col = "white", 
           lwd.legeld.col = FALSE,
           legend.lwd.show = F)
```

``` r
saveRDS(viz_basemap, here::here("analysis/data/derived_data/viz_basemap.rds"))
```

## Padding Map

### Data Wrangling

``` r
sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))

sf_feeders_info$GRIDID <- as.integer(sf_feeders_info$GRIDID)

# Clip Geometry

sf_feeders_info_clip <- st_intersection(khi_land, sf_feeders_info)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Add row ID to join later

sf_feeders_info_clip <- sf_feeders_info_clip %>% 
  mutate(ID = row_number())

# boundary <- mapview(sf_feeders_info_clip) %>%
#   editMap() %>%
#   pluck("finished")
# 
# saveRDS(boundary, here::here("analysis/data/derived_data/viz_boundary_padding.rds"))

boundary <- readRDS(here::here("analysis/data/derived_data/viz_boundary_padding.rds"))
```

### Visualization

``` r
viz_padding <- tm_shape(boundary) +
  tm_borders(alpha = 0)
```

``` r
saveRDS(viz_padding, here::here("analysis/data/derived_data/viz_padding.rds"))
```

## Feeder Boundaries

### Data Wrangling

``` r
feeders_year <- read.csv(here::here("analysis/data/raw_data/feeders_year.csv")) %>% 
  select(-X)

feeders_year$GRIDID <- as.integer(feeders_year$GRIDID)
```

``` r
# Join Datasets

sf_feeders <- left_join(sf_feeders_info, 
                        feeders_year, 
                        by = c("GRIDID" = "GRIDID", 
                                "KE_IBC" = "KE_IBC", 
                                "KE_REGION" = "KE_REGION", 
                                "FEEDER_NAM" = "FEEDER_NAM"))

# Clip Geometry

sf_feeders_clip <- st_intersection(khi_land, sf_feeders)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Hatch patterns

feeder_hatch <- hatchedLayer(sf_feeders_clip, "left2right", mode = "sfc", density = 8)
```

    ## Warning in if (geotype == "MULTIPOLYGON") {: the condition has length > 1 and
    ## only the first element will be used

    ## Warning in if (geotype == "POLYGON") {: the condition has length > 1 and only
    ## the first element will be used

``` r
feeder_hatch <- st_sf(geometry = feeder_hatch)
```

### Visualization

``` r
# Feeder Boundaries

viz_feeder_boundaries <- sf_feeders_clip %>%
  tm_shape() + 
  tm_borders(col = "black", 
             lwd = 0.1,
             alpha = 0.5)
```

``` r
saveRDS(viz_feeder_boundaries, here::here("analysis/data/derived_data/viz_feeders_boundaries.rds"))
```

## IBC Boundaries

### Data Wrangling

``` r
# Read Shape File (IBC)

ibc_sf <- readRDS(here::here("analysis/data/raw_data/ibc-voronoi.rds"))

# Set Geometry

ibc_sf <- ibc_sf %>%
  st_as_sf(ibc_sf$geometry, CRS = 32642)

# Clip Geometry

ibc_sf_clip <- st_intersection(khi_land, ibc_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
ibc_boundaries <- ibc_sf_clip %>%
  group_by(KE_IBC) %>%
  slice(1) %>% 
  ungroup()
```

### Visualization

``` r
# Hatch patterns

ibc_hatch <- hatchedLayer(ibc_boundaries, 
                          "left2right", 
                          mode = "sfc", 
                          density = 10,
                          lwd = 0.5)
```

    ## Warning in if (geotype == "MULTIPOLYGON") {: the condition has length > 1 and
    ## only the first element will be used

``` r
ibc_hatch <- st_sf(geometry = ibc_hatch)

viz_ibc_fill <- tm_shape(ibc_hatch) + 
  tm_lines(col = "black",
           alpha = 0.2,
           lwd.legeld.col = FALSE)
```

``` r
saveRDS(viz_ibc_fill, here::here("analysis/data/derived_data/viz_ibc_fill.rds"))
```

``` r
viz_ibc_boundaries <- ibc_boundaries %>%
  tm_shape() + 
  tm_borders(col = "black", lwd = 0.75)
```

``` r
saveRDS(viz_ibc_boundaries, here::here("analysis/data/derived_data/viz_ibc_boundaries.rds"))
```

## IBC Labels

``` r
# IBC Names

ibc_boundaries <- ibc_boundaries %>% 
  mutate(ibcID = row_number())

# ibc_points <- mapview(ibc_boundaries) %>% 
#   editMap() %>% 
#   pluck("finished")
# 
# ibc_points <- ibc_points %>% 
#   slice(1:30)
# 
# ibc_points <- ibc_points %>% 
#   st_as_sf(ibc_points$geometry, CRS = 32642) %>% 
#   mutate(ibcID = row_number()) %>% 
#   select(ibcID, geometry)
# 
# saveRDS(here::here("analysis/data/derived_data/viz_ibc_points.rds"))

ibc_points <- readRDS(here::here("analysis/data/derived_data/viz_ibc_points.rds"))

ibc_labels <- ibc_boundaries %>% 
  st_set_geometry(NULL)

ibc_labels <- left_join(ibc_points, ibc_labels, by = c("ibcID" = "ibcID"))

ibc_labels <- ibc_labels %>% 
  select(ibcID, KE_IBC, geometry)

viz_ibc_labels <- tm_shape(ibc_labels) +
  tm_bubbles(col = "#542788",
             size = 0.35,
             border.lwd = NA) +
  tm_text("ibcID",
          col = "white",
          size = 0.45)
```

``` r
saveRDS(viz_ibc_labels, here::here("analysis/data/derived_data/viz_ibc_labels.rds"))
```

## Lyari Maps

### Feeder Boundaries

``` r
lyari_feeders <- sf_feeders_clip %>% 
  filter(str_detect(KE_IBC, "Lyari"))
```

``` r
# Hatch patterns

lyari_hatch <- hatchedLayer(lyari_feeders, "left2right", mode = "sfc", density = 4)
```

    ## Warning in if (geotype == "MULTIPOLYGON") {: the condition has length > 1 and
    ## only the first element will be used

    ## Warning in if (geotype == "POLYGON") {: the condition has length > 1 and only
    ## the first element will be used

``` r
lyari_feeder_hatch <- st_sf(geometry = lyari_hatch)
```

``` r
viz_lyari_fill <- tm_shape(lyari_feeder_hatch) +
  tm_lines(col = "black", 
           alpha = 0.5,
           lwd.legeld.col = FALSE)
```

``` r
viz_lyari_boundaries <- lyari_feeders %>% 
  tm_shape() +
  tm_borders(col = "black", lwd = 0.5)
```

``` r
saveRDS(viz_lyari_fill, here::here("analysis/data/derived_data/viz_lyari_fill.rds"))
```

``` r
saveRDS(viz_lyari_boundaries, here::here("analysis/data/derived_data/viz_lyari_boundaries.rds"))
```

### IBC Boundaries

``` r
Lyari_IBC <- ibc_sf_clip %>%
  group_by(KE_IBC) %>%
  slice(1) %>% 
  filter(str_detect(KE_IBC, "Lyari"))

viz_lyari_ibc_bound <- Lyari_IBC %>% 
  tm_shape() +
  tm_borders(col = "black", lwd = 1.8)
```

``` r
saveRDS(viz_lyari_ibc_bound, here::here("analysis/data/derived_data/viz_lyari_ibc_bound.rds"))
```

### Roads

``` r
lyari_roads <- st_intersection(lyari_feeders, khi_roads)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
viz_lyari_roads <- tm_shape(lyari_roads) + 
  tm_lines(lwd = "lineweight", 
           col = "white", 
           legend.lwd.show = F,
           lwd.legeld.col = FALSE)
```

``` r
saveRDS(viz_lyari_roads, here::here("analysis/data/derived_data/viz_lyari_roads.rds"))
```
