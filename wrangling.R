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

names(df)

df <- df %>% 
  dplyr::select(Date, Time, Lat, Lon, 
                Swell, BF, 'Home screen', 
                'Seabird START', 'Seabird END', Seabirds, 
                Albatross, Mollymawk, Shearwater, Petrel, 'Storm & diving petrel', 
                Prion, Gull, Tern, 'Australasian gannet', Skua, Penguin, 
                'Other seabird', Count, 'Seabird note', Note)
