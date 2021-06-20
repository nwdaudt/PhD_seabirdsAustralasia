##
## Wrangling process for build up the final data tables for analysis
##
## Code by Nicholas W Daudt
##
## sessioninfo::session_info()
## R version 4.0.5 (2021-03-31)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.2 LTS
## *****************************************************************************

rm(list = ls())

## Libraries ####
library(plyr)
library(tidyverse)
library(mapview)
library(sf)
library(rnaturalearth)
# library(marmap)

## Spatial features ####

# ****************** Countries *************************************************
## New Zealand
nz_sf <- 
  rnaturalearth::ne_countries(scale = "medium", 
                              country = "new zealand", 
                              returnclass = "sf")
## Australia
aus_sf <- 
  rnaturalearth::ne_countries(scale = "medium", 
                              country = "australia", 
                              returnclass = "sf")
# ******************************************************************************

# ***************** Transects **************************************************
## Far Out Research Collective, Northland NZ

# trial 1 Guzman's ####
# https://gis.stackexchange.com/questions/270725/r-sf-package-points-to-multiple-lines-with-st-cast
df_FarOut_transects <- data.frame(
  lat = c(-34.643, -34.399, -34.554, -34.305, -34.461, -34.214, -34.362),
  lon = c(173.567, 173.674, 173.413, 173.520, 173.258, 173.378, 173.113)
)

multipoints <- sf::st_multipoint(as.matrix(df_FarOut_transects))
points <- sf::st_cast(sf::st_geometry(multipoints), "POINT")

st_crs(points) <- 4326
mapview::mapview(points)

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- sf::st_combine(c(points[x], points[x + 1]))
  line <- sf::st_cast(pair, "LINESTRING")
  return(line)
})

str(linestrings)
l1 <- linestrings[[1]]
st_crs(l1) <- 4326
str(l1)
mapview::mapview(l1)

st_crs(linestrings) <- 4326

mapview::mapview(linestrings)

t1 <- linestrings[1] %>% %>% sf::st_as_sf()

# trial 2 'st_segment' Spacedman's ####
# https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates

st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}

df_FarOut_transects1 <- data.frame(
  lat = c(-34.643, -34.399, -34.554, -34.305, -34.461, -34.214),
  lon = c(173.567, 173.674, 173.413, 173.520, 173.258, 173.378),
  
  lat2 = c(-34.399, -34.554, -34.305, -34.461, -34.214, -34.362),
  lon2 = c(173.674, 173.413, 173.520, 173.258, 173.378, 173.113)
)

df_FarOut_transects1$geom <- 
  sf::st_sfc(sapply(1:nrow(df_FarOut_transects1), 
                    function(i){st_segment(df_FarOut_transects1[i,])}, simplify=FALSE),
             crs = 4326)

df_transects1 <- 
  sf::st_sf(df_FarOut_transects1)

mapview::mapview(df_transects1)
mapview::mapview(df_FarOut_transects1)

# trial 3 'make_line' HAVB's ####
# https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates

df_FarOut_tbl <- tibble::tribble(
  ~lat,   ~lat2,   ~lon,   ~lon2,
  -34.643,-34.399, 173.567, 173.674,
  -34.399,-34.554, 173.674, 173.413,
  -34.554,-34.305, 173.413, 173.520,
  -34.305,-34.461, 173.520, 173.258,
  -34.461,-34.214, 173.258, 173.378,
  -34.214,-34.362, 173.378, 173.113)


make_line <- function(lat, lon, lat2, lon2) {
  st_linestring(matrix(c(lat, lat2, lon, lon2), 2, 2))
}

df <- 
  df_FarOut_tbl %>%
  select(lat, lon, lat2, lon2) %>% 
  pmap(make_line) %>% 
  st_as_sfc(crs = 4326) 

df1 <- df[[1]]

mapview::mapview(df)

# trial 4 r-spatial website ####
# https://r-spatial.github.io/sf/articles/sf3.html
# Getting and setting coordinate reference systems of sf objects

geom = st_sfc(st_point(c(0,1)), st_point(c(11,12)))
s = st_sf(a = 15:16, geometry = geom)
st_crs(s)
s1 = s
st_crs(s1) <- 4326
mapview::mapview(s1)
# ****************************************************

geom1 <- st_sfc(
  st_point(c(-34.643, 173.567)),
  st_point(c(-34.399, 173.674)),
  st_point(c(-34.554, 173.413)),
  st_point(c(-34.305, 173.520)),
  st_point(c(-34.461, 173.258)),
  st_point(c(-34.214, 173.378)),
  st_point(c(-34.362, 173.113))
)

try1 <- st_sf(geometry = geom1)


st_crs(try1) <- 4326
st_crs(geom1) <- 4326

mapview::mapview(geom1)
# trial 5 obrl_soil's answer ####
# https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates

df_tbl <- tibble::tribble(
  ~lat,   ~lat2,   ~lon,   ~lon2,
  -34.643,-34.399, 173.567, 173.674,
  -34.399,-34.554, 173.674, 173.413,
  -34.554,-34.305, 173.413, 173.520,
  -34.305,-34.461, 173.520, 173.258,
  -34.461,-34.214, 173.258, 173.378,
  -34.214,-34.362, 173.378, 173.113)

rows <- split(df_tbl, seq(nrow(df_tbl)))
lines <- lapply(rows, function(row) {
  lmat <- matrix(unlist(row[1:4]), ncol = 2, byrow = TRUE)
  st_linestring(lmat)
})
lines <- st_sfc(lines)
lines_sf <- st_sf('ID' = 1:6, 'geometry' = lines)

mapview::mapview(lines)
#######
## Munida transect, off Otago Peninsula, South Island NZ

# ******************************************************************************
## Base maps ####
## New Zealand
nz_base_map <- 
  ggplot2::ggplot(data = nz_sf) + 
  ggplot2::geom_sf(color = "black", fill = "lightgrey") + 
  ggplot2::coord_sf(xlim = c(164, 180), ylim = c(-53, -34)) + 
  ggplot2::theme_bw()

## Australia
aus_base_map <- 
  ggplot2::ggplot(data = aus_sf) + 
  ggplot2::geom_sf(color = "black", fill = "lightgrey") + 
  ggplot2::coord_sf(xlim = c(107, 160), ylim = c(-45, -7)) + 
  ggplot2::theme_bw()

## DATA Far Out Research Collective (2019 -) ####
df_FarOut <-
  dir(path = "./raw_data/far_out/", pattern = "*.csv", full.names = TRUE) %>% 
  purrr::map_df(~readr::read_csv(., col_types = cols(.default = "c")))

df_FarOut <- 
  df_FarOut %>% 
  dplyr::select(Date, Time, Lat, Lon, 
                Swell, BF, 'Home screen', 
                Seabirds, Albatross, Mollymawk, Shearwater, Petrel, 
                'Storm & diving petrel', Prion, Gull, Tern, 
                'Australasian gannet', Skua, Penguin, 'Other seabird', 
                Count, 'Seabird note', Note)

## Tidying up column names
# All to lower case
names(df_FarOut) <- tolower(names(df_FarOut))

# Replace 'spaces' with 'underscore'
names(df_FarOut) <- gsub(" ", "_", names(df_FarOut))

## Set up right column classes
# Date and time
df_FarOut$date <- lubridate::dmy(df_FarOut$date)
df_FarOut$time <- lubridate::hms(df_FarOut$time)
df_FarOut <- df_FarOut %>% dplyr::rename(hour = time)
# df_FarOut$time <- strptime(df_FarOut$time, format = "%T") # %H:%M:%S

# Factor
factor_cols <- c("swell", "bf", "home_screen", "seabirds", "albatross", 
                 "mollymawk", "shearwater", "petrel", "storm_&_diving_petrel", 
                 "prion", "gull", "tern", "australasian_gannet", "skua", 
                 "penguin", "other_seabird")
df_FarOut[factor_cols] <- lapply(df_FarOut[factor_cols], as.factor)

# Numeric
numeric_cols <- c("lat", "lon", "count")
df_FarOut[numeric_cols] <- lapply(df_FarOut[numeric_cols], as.numeric)

## Fill conditions (swell & bf) for the whole 'df'
df_FarOut <- 
  df_FarOut %>% 
  tidyr::fill(swell, .direction = "updown") %>% 
  tidyr::fill(bf, .direction = "updown")

## Filter just *seabird* information and Conditions, and drop unused levels
df_FarOut <- 
  df_FarOut %>% 
  dplyr::filter(home_screen == "Conditions" | 
                home_screen == "Note" | 
                home_screen == "Seabird START" | 
                home_screen == "Seabird END" | 
                home_screen == "Seabird count") %>% 
  droplevels(.)

## Create an ID number for each seabird count ('id'), 
## which is between (including) every 'Seabird START' and 'Seabird END' from
## 'home_screen' variable
df_FarOut <- 
  df_FarOut %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                              NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## The raw dataset contains some input errors. Next, I fix them --

## Delete wrong data inputs (e.g. double 'Seabird START/END', no 'Seabird START/END'...);
## input (add) new rows (e.g. 'Seabird START/END'); and modify some cells.

## Delete
df_FarOut <- 
  df_FarOut %>% 
  dplyr::filter(!c(id == 108 & date == "2019-11-16" & hour == "8H 8M 30S" & home_screen == "Seabird END"),
                !c(date == "2019-11-16" & hour == "8H 9M 2S" & home_screen == "Note"),
                !c(id == 117 & date == "2019-11-16" & hour == "9H 25M 56S" & home_screen == "Seabird END"),
                !c(id == 140 & date == "2019-11-16" & hour == "11H 27M 48S" & home_screen == "Seabird END"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 43M 20S" & home_screen == "Seabird START"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 45M 24S" & home_screen == "Seabird count"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 46M 21S" & home_screen == "Seabird END"),
                !c(date == "2019-11-16" & hour == "12H 47M 28S" & home_screen == "Note"),
                !c(id == 353 & date == "2020-01-27" & hour == "10H 8M 27S" & home_screen == "Seabird START"),
                !c(id == 577 & date == "2020-01-28" & hour == "16H 28M 3S" & home_screen == "Seabird START"),
                !c(id == 823 & date == "2020-02-02" & hour == "8H 44M 37S" & home_screen == "Seabird START"),
                !c(date == "2020-02-02" & hour == "8H 45M 24S" & home_screen == "Note"),
                !c(id == 1773 & date == "2021-01-22" & hour == "9H 1M 23S" & home_screen == "Seabird START"),
                !c(id == 1874 & date == "2021-01-23" & hour == "12H 39M 2S" & home_screen == "Seabird START"),
                !c(id == 1874 & date == "2021-01-23" & hour == "12H 40M 19S" & home_screen == "Seabird END"),
                !c(date == "2021-01-23" & hour == "12H 40M 48S" & home_screen == "Note"),
                !c(id == 1889 & date == "2021-01-23" & hour == "13H 22M 6S" & home_screen == "Seabird START"),
                !c(date == "2021-01-23" & hour == "13H 25M 5S" & home_screen == "Note"))

## Input
df_input <- data.frame(
  date = lubridate::ymd(c("2019-11-16", "2020-02-03", "2020-02-03", 
                          "2021-01-12", "2021-01-15", "2021-01-15", 
                          "2021-01-15", "2021-01-16", "2021-01-16", 
                          "2021-01-22")),
  hour = lubridate::hms(c("9H 8M 56S", "8H 49M 53S", "9H 36M 13S", 
                          "10H 21M 10S", "12H 41M 48S", "13H 15M 28S", 
                          "17H 32M 14S", "10H 7M 27S", "10H 55M 41S", 
                          "12H 54M 6S")),
  lat = as.numeric(c(-34.10391, -35.07099, -35.03732, 
                     -34.42122, -34.24153, "NA", 
                     -34.43571, -34.26202, -34.31040, 
                     -34.90750)),
  lon = as.numeric(c(174.1118, 175.1295, 175.1657,
                      173.3327, 173.3656, "NA", 
                      173.2771, 174.1062, 174.1172, 
                      175.1360)),
  home_screen = as.factor(c("Seabird START", "Seabird END", "Seabird END",
                            "Seabird END", "Seabird END", "Seabird START", 
                            "Seabird END", "Seabird START", "Seabird END", 
                            "Seabird START"))
)

df_FarOut <- 
  dplyr::bind_rows(df_input, df_FarOut) %>% 
  dplyr::arrange(date, hour)

## Modify


## Need to create delete and create the 'id' column again, with data set fixed
df_FarOut <- 
  df_FarOut %>% 
  dplyr::select(- id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                            NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## Create a column indicating if the seabird count was complete (10 min) or not
# 'Period' objects as "time" are measured in seconds, so 10 min = 600 sec.

test <- df_FarOut %>% 
  dplyr::select(id, hour, home_screen) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END") %>% 
  tidyr::pivot_wider(names_from = home_screen, values_from = hour, values_fn = list) %>% 
  dplyr::rename(seabird_start = "Seabird START", 
                seabird_end = "Seabird END") %>% 
  tidyr::unnest(cols = c(seabird_start, seabird_end)) %>% 
  dplyr::mutate(time_diff = seabird_end - seabird_start) ## %>% 
##  dplyr::mutate(complete_count = ifelse(time_diff >= 10, "yes", "no"))

## DATA Munida ####


## DATA Australia ASG Jan2016-Jan2021 ####

####
## 08Jan2016 - 24Jan2021
## This is a **presence-only** dataset
####

df_Australia <- readr::read_csv("./raw_data/australia/ASG_2016_2021.csv")
# original row number 22,911

## Set up right column classes, and create some useful ones
df_Australia$year <- lubridate::year(df_Australia$date)
df_Australia$month <- lubridate::month(df_Australia$date)
df_Australia <- df_Australia %>% 
  dplyr::mutate(season = ifelse(month == 12 | month == 1 | month == 2, "summer",
                         ifelse(month == 3 | month == 4 | month == 5, "autumn",
                         ifelse(month == 6 | month == 7 | month == 8, "winter", 
                                "spring"))))

# as Factor
factor_cols <- c("observer", "voyage", "ship_activity", "sea_state", "windforce", 
                 "cloud_cover", "cloud_cover_okta", "precipitation", "visibility", 
                 "sun_glare", "speciesid", "wov_code")

df_Australia[factor_cols] <- lapply(df_Australia[factor_cols], as.factor)

# as Numeric
numeric_cols <- c("latitude", "longitude", "ship_course", "ship_speed", "depth", 
                  "salinity", "sea_temperature", "wind_direction", "air_pressure", 
                  "air_temperature", 
                  "total_ct", "feeding_ct", "sitting_on_water_ct", "sitting_on_ice_ct", 
                  "sitting_on_ship_ct", "in_hand_ct", "flying_past_ct", 
                  "accompanying_ct", "following_wake_ct")

df_Australia[numeric_cols] <- lapply(df_Australia[numeric_cols], as.numeric)

# Clean some empty columns, columns with no interest for analysis, 
# and missing values in species ID and geographic coordinates
df_Australia <- 
  df_Australia %>% 
  dplyr::select(- c(observer, ship_heading, 
                    sitting_on_ice_ct, in_hand_ct, bird_direction)) %>% 
  dplyr::filter(latitude != 0 &
                longitude != 0) %>%                             # 22,611
  dplyr::filter(!grepl("null", species, ignore.case = TRUE) &   # 22,339
                !is.na(wov_code) &                              # 22,309
                !is.na(speciesid))                              # 22,308

## Good data - according to Eric
df_Australia <- 
  df_Australia %>% 
  dplyr::filter(voyage == "fk201228" | #voyage == fk210206 | 
                  
                  voyage == "in2016_t02" | voyage == "in2016_v06" | 
                  
                  voyage == "in2017_v02" | voyage == "in2017_t01" | 
                  voyage == "in2017_t02" | 
                  
                  voyage == "in2018_c01" | voyage == "in2018_t02" | 
                  voyage == "in2018_v04" | voyage == "in2018_v06" | 
                  
                  voyage == "in2019_t01" | voyage == "in2019_t02" | 
                  voyage == "in2019_t03" | voyage == "in2019_v04" | 
                  voyage == "in2019_v07") %>% 
  droplevels(.)

# Replace missing values in 'total_ct' column, with the sum of 
# other counting columns

# Check for NAs
# plyr::count(is.na(df_Australia$total_ct))

df_Australia <- 
  df_Australia %>% 
  dplyr::mutate(total_ct = matrixStats::rowSums2(as.matrix(.[, c(
    "feeding_ct", "sitting_on_water_ct", "flying_past_ct", 
    "accompanying_ct", "following_wake_ct")]), na.rm = TRUE))

# Check for NAs again
# plyr::count(is.na(df_Australia$total_ct))

## Note: I did not considered records from birds on the ship (col = "sitting_on_ship_ct")

# Exclude rows summing "0" -- they mean all columns were "NA" or "0"
# plyr::count(df_Australia$total_ct == 0)

df_Australia <- 
  df_Australia %>% 
  dplyr::filter(total_ct != 0) %>%    # 21,140
  droplevels(.)

## Create an ID number for sample units
df_Australia <- 
  df_Australia %>% 
  dplyr::group_by(date) %>% 
  dplyr::mutate(ID = dplyr::cur_group_id()) %>%    # 13,333 unique IDs
  dplyr::relocate(ID, .before = everything()) %>% 
  dplyr::arrange(date) %>% 
  dplyr::ungroup()

## Quick plot to check
df_Australia_spatial <-
  df_Australia %>%
  dplyr::mutate(longitude1 = longitude,
                latitude1 = latitude) %>%
  sf::st_as_sf(coords = c("longitude1", "latitude1"), crs = 4326)

mapview::mapview(df_Australia_spatial)
# mapview::mapview(df_Australia_spatial, zcol = "year")
# mapview::mapview(df_Australia_spatial, zcol = "season")

# ************* Create grid for analysis MS_1***********************************
## 0.5 x 0.5 degree
# grid_AUS_0.5 <- 
#   sf::st_make_grid(df_Australia_spatial, cellsize = c(0.5, 0.5), crs = 4326)
# 
# mapview::mapview(df_Australia_spatial) + grid_AUS_0.5
# sf::st_write(grid_AUS_0.5, "./data/ms1_australia_RCPs/grid_asg20162021_0.5.gpkg")

## 1 x 1 degree -- this is the one
grid_AUS_1 <- 
  sf::st_make_grid(df_Australia_spatial, cellsize = c(1, 1), crs = 4326)

# mapview::mapview(df_Australia_spatial) + grid_AUS_1
sf::st_write(grid_AUS_1, "./data/ms1_australia_RCPs/grid_asg20162021_1.gpkg")

## 2 x 2 degree
# grid_AUS_2 <- 
#   sf::st_make_grid(df_Australia_spatial, cellsize = c(2, 2), crs = 4326)
# 
# mapview::mapview(df_Australia_spatial) + grid_AUS_2
# sf::st_write(grid_AUS_2, "./data/ms1_australia_RCPs/grid_asg20162021_2.gpkg")

# ******************************************************************************

# ******************** Save data.frames ****************************************
## Abundance data set
readr::write_csv(df_Australia, "./data/ms1_australia_RCPs/birds_abund_asg20162021.csv")

## Occurrence data set - only birds identified to **species level**
df_Australia_occ <- 
  df_Australia %>% 
  dplyr::filter(!stringr::str_detect(species, "sp.") & 
                !stringr::str_detect(species, "spp.")) %>% 
  droplevels(.)

readr::write_csv(df_Australia_occ, "./data/ms1_australia_RCPs/birds_occ_asg20162021.csv")

## Occurrence data set: 18,157 rows, 12,466 unique ID

## *****************************************************************************

#### ... ####
