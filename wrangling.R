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
library(reshape2)
library(tidyverse)
# library(mapview)
# library(sf)

## DATA Far Out Research Collective (2019 -) ####
df <-
  dir(path = "./raw_data/far_out/", pattern = "*.csv", full.names = TRUE) %>% 
  purrr::map_df(~readr::read_csv(., col_types = cols(.default = "c")))

df <- df %>% 
  dplyr::select(Date, Time, Lat, Lon, 
                Swell, BF, 'Home screen', 
                Seabirds, Albatross, Mollymawk, Shearwater, Petrel, 
                'Storm & diving petrel', Prion, Gull, Tern, 
                'Australasian gannet', Skua, Penguin, 'Other seabird', 
                Count, 'Seabird note', Note)

## Tidying up column names
# All to lower case
names(df) <- tolower(names(df))

# Replace 'spaces' with 'underscore'
names(df) <- gsub(" ", "_", names(df))

## Setting up right column classes
# Date and time
df$date <- lubridate::dmy(df$date)
df$time <- lubridate::hms(df$time)
df$time <- strptime(df$time, format="%T") # %H:%M:%S
df <- df %>% dplyr::rename(hour = time)

# Factor
factor_cols <- c("swell", "bf", "home_screen", "seabirds", "albatross", 
                 "mollymawk", "shearwater", "petrel", "storm_&_diving_petrel", 
                 "prion", "gull", "tern", "australasian_gannet", "skua", 
                 "penguin", "other_seabird")
df[factor_cols] <- lapply(df[factor_cols], as.factor)

# Numeric
numeric_cols <- c("lat", "lon", "count")
df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)

## Copying conditions (swell & bf) for the whole 'df'
df <- df %>% 
  tidyr::fill(swell, .direction = "updown") %>% 
  tidyr::fill(bf, .direction = "updown")

## Filter just *seabird* information and Conditions, and drop unused levels
df <- df %>% 
  dplyr::filter(home_screen == "Conditions" | 
                home_screen == "Note" | 
                home_screen == "Seabird START" | 
                home_screen == "Seabird END" | 
                home_screen == "Seabird count") %>% 
  droplevels()

## Create an ID number for each seabird count, 
## which is between (including) every 'Seabird START' and 'Seabird END' from
## 'home_screen' variable
df <- df %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                              NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## Create a column indicating if the seabird count was complete (10 min) or not
# 'Period' objects as "time" are measured in seconds, so 10 min = 600 sec.

test1 <- df %>% 
  dplyr::select(id, hour, home_screen) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END") %>% 
  tidyr::pivot_wider(names_from = home_screen, values_from = hour, values_fn = list) %>% 
  rename(seabird_start = "Seabird START", 
         seabird_end = "Seabird END") %>% 
  tidyr::unnest(cols = c(seabird_start, seabird_end)) %>% 
  dplyr::mutate(time_diff = seabird_end - seabird_start) ## %>% 
##  dplyr::mutate(complete_count = ifelse(time_diff >= 10, "yes", "no"))

