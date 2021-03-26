##
## Wrangling process for build up the final data tables for analysis
##
## Code by Nicholas W Daudt
##
## sessioninfo::session_info()
## R version 3.6.3 (2020-02-29)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.1 LTS
################################################################################

rm(list = ls())

## Libraries ####
library(plyr)
# library(reshape2)
library(tidyverse)
# library(mapview)
# library(sf)

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

## Setting up right column classes
# Date and time
df_FarOut$date <- lubridate::dmy(df_FarOut$date)
df_FarOut$time <- lubridate::hms(df_FarOut$time)
df_FarOut$time <- strptime(df_FarOut$time, format = "%T") # %H:%M:%S
df_FarOut <- df_FarOut %>% dplyr::rename(hour = time)

# Factor
factor_cols <- c("swell", "bf", "home_screen", "seabirds", "albatross", 
                 "mollymawk", "shearwater", "petrel", "storm_&_diving_petrel", 
                 "prion", "gull", "tern", "australasian_gannet", "skua", 
                 "penguin", "other_seabird")
df_FarOut[factor_cols] <- lapply(df_FarOut[factor_cols], as.factor)

# Numeric
numeric_cols <- c("lat", "lon", "count")
df_FarOut[numeric_cols] <- lapply(df_FarOut[numeric_cols], as.numeric)

## Copying conditions (swell & bf) for the whole 'df'
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
  droplevels()

## Create an ID number for each seabird count, 
## which is between (including) every 'Seabird START' and 'Seabird END' from
## 'home_screen' variable
df_FarOut <- 
  df_FarOut %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                              NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## Create a column indicating if the seabird count was complete (10 min) or not
# 'Period' objects as "time" are measured in seconds, so 10 min = 600 sec.

test1 <- df_FarOut %>% 
  dplyr::select(id, hour, home_screen) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END") %>% 
  tidyr::pivot_wider(names_from = home_screen, values_from = hour, values_fn = list) %>% 
  rename(seabird_start = "Seabird START", 
         seabird_end = "Seabird END") %>% 
  tidyr::unnest(cols = c(seabird_start, seabird_end)) %>% 
  dplyr::mutate(time_diff = seabird_end - seabird_start) ## %>% 
##  dplyr::mutate(complete_count = ifelse(time_diff >= 10, "yes", "no"))

## DATA Munida ####


## DATA Australia ####

####
## 08jan2016 - 24jan2021
####

df_Australia <- readr::read_csv("./raw_data/australia/ASG_2016_2021.csv")
# original row number 22,911

## Set up right column classes, and create some useful ones
df_Australia$year <- lubridate::year(df_Australia$date)
df_Australia$month <- lubridate::month(df_Australia$date)
df_Australia <- df_Australia %>% 
  dplyr::mutate(season = ifelse(month == 12 | month == 1 | month == 2, "summer",
                         ifelse(month == 3 | month == 4 | month == 5, "autumn",
                         ifelse(month == 6 | month == 7 | month == 8, "winter", "spring"))))

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
  dplyr::select(- c(observer, voyage, ship_heading, 
                    sitting_on_ice_ct, in_hand_ct, bird_direction)) %>% 
  dplyr::filter(latitude != 0 &
                longitude != 0) %>%                             # 22,611
  dplyr::filter(!grepl("null", species, ignore.case = TRUE) &   # 22,339
                !is.na(wov_code) &                              # 22,309
                !is.na(speciesid))                              # 22,308

# Replace missing values in 'total_ct' column, with the sum of 
# other counting columns
# plyr::count(is.na(df_Australia$total_ct))

df_Australia <- 
  df_Australia %>% 
  dplyr::mutate(total_ct = matrixStats::rowSums2(as.matrix(.[, c(
    "feeding_ct", "sitting_on_water_ct", "flying_past_ct", "accompanying_ct", 
                                                   "following_wake_ct")]), na.rm = TRUE))

# Check
# plyr::count(is.na(df_Australia$total_ct))

## Note: I did not considered records from birds on the ship (col = "sitting_on_ship_ct")

# Exclude these rows -- they mean all columns were "NA" or "0"
# plyr::count(df_Australia$total_ct == 0)

df_Australia <- 
  df_Australia %>% 
  dplyr::filter(total_ct != 0) # 21,140

## Create an ID number for sample units

dplyr::relocate(stretch, .before = beach) 
dplyr::arrange(date)

## Quick Histogram of counts
hist(df_Australia$total_ct, breaks = 500)
hist(log(df_Australia$total_ct), breaks = 500)

## Quick plot
df_Australia_spatial <- 
  df_Australia %>% 
  dplyr::mutate(longitude1 = longitude,
                latitude1 = latitude) %>% 
  sf::st_as_sf(coords = c("longitude1", "latitude1"), crs = 4326)

# mapview::mapview(df_Australia_spatial, zcol = "year")
# mapview::mapview(df_Australia_spatial, zcol = "season")

# ------ Create grid for analysis ----------------------------------------------
## 1 x 1 degree -- That is probably too fine-scale for the study aim
grid_AUS_1 <- 
  sf::st_make_grid(df_Australia_spatial, cellsize = c(1, 1))

# mapview::mapview(df_Australia_spatial) + grid_AUS_1

## 2 x 2 degree -- Looks great...
grid_AUS_2 <- 
  sf::st_make_grid(df_Australia_spatial, cellsize = c(2, 2))

sf::st_write(grid_AUS_2, "./raw_data/australia/grid_AUS20162019_2.shp")

# mapview::mapview(df_Australia_spatial) + grid_AUS_2
# ------------------------------------------------------------------------------

## Abund data.frame (just birds)
readr::write_csv()
