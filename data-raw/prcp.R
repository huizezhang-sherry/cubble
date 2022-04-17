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

climate_full <- aus_climate_raw %>%
  cubble::as_cubble(index = date, key = id, coords = c(long, lat)) %>%
  stretch(ts) %>%
  mutate(tmax = tmax/10,
         tmin = tmin/10) %>%
  tamp()


prcp_aus <- climate_full |> face_temporal() |> select(-tmax, -tmin) |> face_spatial()
usethis::use_data(prcp_aus, overwrite = TRUE, compress = "bzip2")
