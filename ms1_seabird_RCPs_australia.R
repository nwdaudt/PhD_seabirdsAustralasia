# ******************************************************************************
# Seabird assemblages and their relationship with environmental variables
# around Australia
#
# Daudt et al. (202*) Journal of Biogeography <doi>
#
# Code by Nicholas W Daudt
# ******************************************************************************

# Session Info ####
sessioninfo::session_info()

# paste here #

# Libraries ####

# pckgs <- c("tidyverse", "sf", "raster", "RCPmod")
# install.packages(pckgs)

library(tidyverse)
library(sf)
library(raster)
library(RCPmod)

## DATA Seabird ####
seabirds_dt <- readr::read_csv("./data/ms1_australia_RCPs/birds_occ_asg20162021.csv")

## DATA Environmental ####

## DATA Grid ####
grid <- sf::read_sf("./data/australia/grids/grid_asg20162021_1.shp")
# mapview::mapview(grid)

## Data wrangling & final data table ####

# Exploratory Data Analysis ####
### Maps: counts total & per year/season ####

## ASG all data 
map_ASG_data <- 
  aus_base_map + 
  ggplot2::geom_sf(data = df_Australia_spatial, 
                   alpha = 0.5)

ggplot2::ggsave(plot = map_ASG_data, 
                filename = "total.pdf",
                path = "./data/ms1_australia_RCPs/",
                width = 20, height = 15, units = "cm", dpi = 300)

## ASG faceted by year and season
map_ASG_data_wrap_by_year <- 
  aus_base_map + 
  ggplot2::geom_sf(data = df_Australia_spatial, 
                   aes(color = season)) + 
  ggplot2::facet_wrap(year ~ ., nrow = 2)

ggplot2::ggsave(plot = map_ASG_data_wrap_by_year, 
                filename = "by_year_season.pdf",
                path = "./data/ms1_australia_RCPs/",
                width = 30, height = 24, units = "cm", dpi = 300)

### Map: counts (IDs) per grid ####

df_Australia_occ_spatial <-
  df_Australia_occ %>% 
  dplyr::mutate(long1 = longitude, lat1 = latitude) %>% 
  sf::st_as_sf(coords = c("long1", "lat1")) %>% 
  sf::st_set_crs(4326)

grid_1 <- sf::read_sf("./data/ms1_australia_RCPs/grid_asg20162021_1.gpkg")
grid_1$id <- 1:nrow(grid_1)

grid_1_aus <- sf::st_difference(grid_1, aus_sf)     ## 1224
grid_0.5_aus <- sf::st_difference(grid_0.5, aus_sf) ## 9294
mapview::mapview(grid_1_aus)
mapview::mapview(grid_0.5_aus)

grid_0.5 <- sf::read_sf("./data/ms1_australia_RCPs/grid_asg20162021_0.5.gpkg")
grid_0.5$id <- 1:nrow(grid_0.5)

## 1x1 degree
countNumber1 <- sf::st_join(df_Australia_occ_spatial, grid_1)

countNumber1 <- 
  as.data.frame(countNumber1) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(n = dplyr::n_distinct(ID))

countNumber1 <- 
  sp::merge(grid_1, countNumber1, by = "id") %>% 
  sf::st_as_sf()

n_brks <- classInt::classIntervals(countNumber1$n, n = 10, style = "quantile")

countNumber1 <- 
  countNumber1 %>% 
  dplyr::mutate(n_breaks = cut(n, n_brks$brks))

countNumber_1 <- 
  aus_base_map + 
  geom_sf(data = countNumber1, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1, 
                    name = "Number of counts") +
  theme(legend.position = c(0.15, 0.19),
        legend.key.size = unit(4, 'mm'), 
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(linetype = "solid", colour = "black")) + 
  annotate(geom = "text", x = 120, y = -10, label = "n_grids = 262",
           fontface = "bold", color = "black", size = 6)

countNumber_1_hist <- 
  ggplot(data = countNumber1, aes(x = n)) + 
  geom_histogram(bins = 300) + 
  geom_vline(xintercept = 10, color = "red") +
  geom_vline(xintercept = 20, color = "blue") +
  annotate(geom = "text", x = 500, y = 35, label = "1 grid",
           color = "black", size = 6) + 
  annotate(geom = "text", x = 650, y = 35, label = "10 counts",
           color = "red", size = 6) + 
  annotate(geom = "text", x = 650, y = 25, label = "20 counts",
           color = "blue", size = 6) +
  theme_bw()

## 0.5x0.5 degree
countNumber0.5 <- sf::st_join(df_Australia_occ_spatial, grid_0.5)

countNumber0.5 <- 
  as.data.frame(countNumber0.5) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(n = dplyr::n_distinct(ID))

countNumber0.5 <- 
  sp::merge(grid_0.5, countNumber0.5, by = "id") %>% 
  sf::st_as_sf()

n_brks <- classInt::classIntervals(countNumber0.5$n, n = 9, style = "quantile")

countNumber0.5 <- 
  countNumber0.5 %>% 
  dplyr::mutate(n_breaks = cut(n, n_brks$brks))

countNumber_0.5 <- 
  aus_base_map + 
  geom_sf(data = countNumber0.5, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1, 
                    name = "Number of counts") +
  theme(legend.position = c(0.15, 0.18), 
        legend.key.size = unit(4, 'mm'), 
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(linetype = "solid", colour = "black")) + 
  annotate(geom = "text", x = 120, y = -10, label = "n_grids = 992",
           fontface = "bold", color = "black", size = 6)

countNumber_0.5_hist <- 
  ggplot(data = countNumber0.5, aes(x = n)) + 
  geom_histogram(bins = 300) + 
  geom_vline(xintercept = 10, color = "red") +
  geom_vline(xintercept = 20, color = "blue") +
  annotate(geom = "text", x = 500, y = 200, label = "0.5 grid",
           color = "black", size = 6) + 
  theme_bw()

## Put both maps side by side and save
fig_effort <- (countNumber_1 | countNumber_0.5)
ggplot2::ggsave(plot = fig_effort, 
                filename = "fig_effort.pdf",
                path = "./data/ms1_australia_RCPs/",
                width = 30, height = 18, units = "cm", dpi = 300)

fig_hist_effort <- (countNumber_1_hist / countNumber_0.5_hist)
ggplot2::ggsave(plot = fig_hist_effort, 
                filename = "fig_hist_effort.pdf",
                path = "./data/ms1_australia_RCPs/",
                width = 30, height = 18, units = "cm", dpi = 300)

# Analysis ####

# Graphs & Figs?? ####

### Test ####
## Seabird data ****************************************************************
seabirds_dt <- readr::read_csv("./data/ms1_australia_RCPs/asg20162021_birds_occ.csv")
## UPDATE FILE...

test_sp <- 
  seabirds_dt %>% 
  dplyr::mutate(in_out = ifelse(latitude <= -22 & latitude >= -45 & 
                                  longitude >= 148 & longitude <= 157.5, 
                                "in", "out")) %>%   ## 3244 'in' (28% of the data set)
  # Remove records 'out'                            ## 3024 IDs
  dplyr::filter(! in_out == "out") %>% 
  dplyr::select(- in_out)

test_sp <-
  test_sp %>%
  dplyr::mutate(long1 = longitude, lat1 = latitude) %>%
  sf::st_as_sf(coords = c("long1","lat1"), crs = 4326)
# mapview::mapview(test_sp)

## Grid test *******************************************************************
grid_test <- sf::read_sf("./data/ms1_australia_RCPs/grid_asg20162021_1.gpkg")

grid_test <- 
  sf::st_crop(grid_test, test_sp)
# mapview::mapview(grid_test)

grid_test <- 
  grid_test %>% 
  dplyr::mutate(id = 1:nrow(.))

## Environmental data **********************************************************
## Bat / Slope / DistCoast -- SST / SSS / CHL / EKE

#### BAT & Slope ***************************************************************

bat <- sp::read.asciigrid(
  "./raw_data/GEBCO_bathymetry/test/gebco_2020_n-22.0_s-45.0_w148.0_e157.5.asc")

bat <- 
  bat %>% 
  tibble::as_tibble() %>% 
  dplyr::select(lon = 2, lat = 3, elevation = 1) %>% 
  dplyr::filter(! elevation > 0)

## As "raster"
raster_bat <- raster::rasterFromXYZ(bat, crs = 4326)
# raster::plot(raster_bat)

## Slope
raster_slope <- raster::terrain(raster_bat, opt = "slope")
# raster::plot(raster_slope)

## Extract mean BAT values related to each analysis grid
analysis_grid_bat <- 
  as.data.frame(raster::extract(raster_bat, grid_test, fun = mean))

analysis_grid_bat <- 
  analysis_grid_bat %>% 
  dplyr::mutate(id = 1:nrow(.)) %>%
  dplyr::rename(bat_mean = V1)

analysis_grid_bat <- 
  sp::merge(grid_test, analysis_grid_bat, by = "id")
# mapview::mapview(analysis_grid_bat, zcol = "bat_mean")

## Extract mean SLOPE values related to each analysis grid
analysis_grid_slope <- 
  as.data.frame(raster::extract(raster_slope, grid_test, fun = mean))

analysis_grid_slope <- 
  analysis_grid_slope %>% 
  dplyr::mutate(id = 1:nrow(.)) %>% 
  dplyr::rename(slope_mean = V1)

analysis_grid_slope <- 
  sp::merge(grid_test, analysis_grid_slope, by = "id")
# mapview::mapview(analysis_grid_slope, zcol = "slope_mean")

### Join them 
analysis_grid <- 
  analysis_grid_bat %>% 
  as.data.frame() %>% 
  dplyr::select(id, bat_mean) %>% 
  dplyr::left_join(analysis_grid_slope, ., by = "id")

rm(bat, raster_bat, raster_slope, 
   analysis_grid_bat, analysis_grid_slope)

#### dist_coast ****************************************************************

## Australia spatial feature
aus_sf <- 
  rnaturalearth::ne_countries(scale = "medium", 
                              country = "australia", 
                              returnclass = "sf")

aus_sf <- 
  sf::st_cast(aus_sf, 'MULTILINESTRING') %>% 
  sf::st_cast('LINESTRING', do_split = TRUE) %>%
  dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>%
  sf::st_cast('POLYGON')

aus_sf <- aus_sf %>% dplyr::filter(npts == 1154)
# mapview::mapview(aus_sf)


#### SST ***********************************************************************

raster_sst <- raster::raster("./data/ms1_australia_RCPs/raster_sst_mean.gpkg")
# raster::plot(raster_sst)

# Extract mean SST values related to each analysis grid
analysis_grid_sst <- 
  as.data.frame(raster::extract(raster_sst, grid_test, fun = mean))

analysis_grid_sst <- 
  analysis_grid_sst %>% 
  dplyr::mutate(id = 1:nrow(.), 
                # Kelvin to Celsius:
                sst_mean = (V1 - 273)) %>% 
  dplyr::select(- V1)

analysis_grid_sst <- 
  sp::merge(grid_test, analysis_grid_sst, by = "id")
# mapview::mapview(analysis_grid_sst, zcol = "sst_mean")

### Join them 
analysis_grid <- 
  analysis_grid_sst %>% 
  as.data.frame() %>% 
  dplyr::select(id, sst_mean) %>% 
  dplyr::left_join(analysis_grid, ., by = "id")

rm(raster_sst, analysis_grid_sst)

## CHL 
raster_chl <- raster::raster("./data/ms1_australia_RCPs/raster_chl_mean.gpkg")
# raster::plot(raster_chl)

# Extract mean CHL values related to each analysis grid
analysis_grid_chl <- 
  as.data.frame(raster::extract(raster_chl, grid_test, fun = mean, na.rm = TRUE))

analysis_grid_chl <- 
  analysis_grid_chl %>% 
  dplyr::mutate(id = 1:nrow(.)) %>% 
  dplyr::rename(chl_mean = V1)

analysis_grid_chl <- 
  sp::merge(grid_test, analysis_grid_chl, by = "id")
# mapview::mapview(analysis_grid_chl, zcol = "chl_mean")

### Join them 
analysis_grid <- 
  analysis_grid_chl %>% 
  as.data.frame() %>% 
  dplyr::select(id, chl_mean) %>% 
  dplyr::left_join(analysis_grid, ., by = "id")

rm(raster_chl, analysis_grid_chl)
