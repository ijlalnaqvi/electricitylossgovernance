01-Data-Wrangling
================
(11 June, 2021)

In this file four key data wrangling steps are performed.

1.  Raw electricity data is joined with spatial data (containing
    locations of feeders) & a base file is prepared which is used in all
    analysis.

2.  Areas are calculated for each of these feeders.

3.  Electricity density for each feeder is calculated by dividing the
    amount of electricity received by the area of each feeder.

4.  Population data for Karachi is extracted from the [World Pop
    Database for the
    year 2016.](https://www.worldpop.org/geodata/summary?id=5418)

## Preparation of Base File

Raw data as given by KE was processed as described in the paper and
filtered/ wrangled using the code chunk below to arrive at a clean
dataset `feeders_year`.

``` r
# Summarise feeder data per feeder

feeders_year <- feeders_joined %>%
  filter(Months > "2015-07-01" & Months < "2016-07-01") %>%
  group_by(FEEDER_NAM, GRIDID, KE_IBC, KE_REGION) %>%
  drop_na(FEEDER_NAM) %>%
  summarise(
    received = sum(`Consumption (units)`, na.rm = T),
    billed = sum(`Billed Units`, na.rm = T),
    com = mean(`Commercial Consumers`, na.rm = T),
    res = mean(`Residential Consumers`, na.rm = T),
    ind = mean(`Industrial Consumers`, na.rm = T),
    tot = mean(`No. of Consumers`, na.rm = T) + 1
  ) %>%
  mutate(losses = (received - billed) / received) %>%
  mutate(comRel = com / tot) %>%
  mutate(resRel = res / tot) %>%
  mutate(indRel = ind / tot) %>%
  filter(received > 10000)
```

``` r
# Read in feeder data

feeders_year <- read.csv(here::here("analysis/data/raw_data/feeders_year.csv")) %>% 
  select(-X)
```

------------------------------------------------------------------------

## Calculate Feeder Areas

### Load Datasets

``` r
sf_feeders_info <- readRDS(here::here("analysis/data/raw_data/sf_feeders_info.rds"))
```

### Calculate Areas

``` r
feeder_areas <- st_area(sf_feeders_info)

feeder_areas <- as.data.frame(feeder_areas)

sf_feeders_info_areas <- bind_cols(sf_feeders_info, feeder_areas)

sf_feeders_info_areas$GRIDID <- as.integer(sf_feeders_info_areas$GRIDID)
```

------------------------------------------------------------------------

## Calculate Electricity Density

### Load Data

``` r
# Karachi Land

khi_land <- readRDS(here::here("analysis/data/raw_data/KHI_land.rds"))
```

### Join Data

``` r
# Clip Geometry and join elec info

sf_feeders_info_areas <- st_intersection(khi_land, sf_feeders_info_areas)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
sf_feeder_area_elec <- left_join(sf_feeders_info_areas, 
                                 feeders_year,
                                 by = c("GRIDID", "FEEDER_NAM", "KE_IBC", "KE_REGION"))
```

``` r
# Calculate electricity density

sf_feeder_area_elec <- sf_feeder_area_elec %>% 
  mutate(elec_norm_area = received / feeder_areas)

sf_feeder_area_elec$elec_norm_area <- as.double(sf_feeder_area_elec$elec_norm_area)
```

### Save Data

``` r
saveRDS(sf_feeder_area_elec,
        here::here("analysis/data/derived_data/sf_feeder_area_elec.rds"))
```

------------------------------------------------------------------------

## Extract Population

### Load Datasets

The population data can be found at this link:
<https://www.worldpop.org/geodata/summary?id=5418> A `.rds` version of
this data is included in this compendium in the `raw-data` folder.

``` r
# Karachi Population

khi_pop <- raster(here::here("analysis/data/raw_data/worldpop_pak_ppp_2016.tif"))
```

### Population Map

``` r
# Reproject data

khi_land_reproj <- st_transform(khi_land, 
                              CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extract population

khi_pop_crop <- crop(khi_pop, extent(khi_land_reproj))
khi_pop_mask <- mask(khi_pop_crop, khi_land_reproj)

populationdata <- raster::extract(khi_pop_mask, sf_feeders_info, weights=FALSE, fun=sum)
```

    ## Warning in .local(x, y, ...): Transforming SpatialPolygons to the CRS of the
    ## Raster

``` r
populationdata <- as.data.frame(populationdata)

populationdata <- populationdata %>% 
  rename(populationsum = V1)

sf_feeder_population <- bind_cols(sf_feeders_info, populationdata)

sf_feeder_population$populationsum[is.na(sf_feeder_population$populationsum)] <- 0
```

``` r
# Express the populations in multiples of 1000

sf_feeder_population <- sf_feeder_population %>% 
  mutate(populationsum = populationsum/1000)
```

### Save Data

``` r
saveRDS(sf_feeder_population, here::here("analysis/data/derived_data/sf_feeder_population.rds"))
```
