library(rnoaa)
library(tidyverse)
library(cubble)

# all the Australian stations that are still active in 2020 and have all of
# the three PRCP, TMAX, and TMIN recorded
aus_stations <- ghcnd_stations() %>%
  filter(str_starts(id, "ASN")) %>%
  filter(last_year >= 2020) %>%
  mutate(wmo_id = as.numeric(wmo_id),
         name = str_to_lower(name)) %>%
  select(-state, -gsn_flag) %>%
  filter(element %in% c("PRCP", "TMAX", "TMIN")) %>%
  nest(element: last_year) %>%
  rowwise() %>%
  filter(nrow(data) == 3) %>%
  select(-data) %>%
  filter(id != "ASN00079105")

# query the climate data for the selected station (may take a while if run for
# the first time)
aus_climate_raw <- aus_stations %>%
  rowwise() %>%
  mutate(ts = list(meteo_pull_monitors(monitors = id, var = c("PRCP", "TMAX", "TMIN"),
                                       date_min = "2016-01-01",
                                       date_max = "2020-12-31") %>%
                     select(-id))) %>%
  rename(lat = latitude, long = longitude, elev = elevation)

aus_climate_cubble <- aus_climate_raw %>%
  cubble::as_cubble(index = date, key = id, coords = c(long, lat))

# subset stations that don't have missing and records in 2020
aus_climate <- aus_climate_cubble %>%
  stretch() %>%
  filter(lubridate::year(date) == 2020) %>%
  tamp() %>%
  add_missing_prct(prcp:tmin) %>%
  filter(prcp_missing == 0, tmax_missing == 0, tmin_missing == 0) %>%
  select(-contains("missing"))

usethis::use_data(aus_climate, overwrite = TRUE)

############################################################
climate_flat <- aus_climate %>%
  head(5) %>%
  unnest() %>%
  ungroup()

usethis::use_data(climate_flat, overwrite = TRUE)

############################################################
set.seed(123)
climate_missing <- aus_climate_cubble %>%
  slice_sample(n = 50)

usethis::use_data(climate_missing, overwrite = TRUE)

