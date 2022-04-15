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
                                       date_min = "2020-01-01",
                                       date_max = "2020-12-31") %>%
                     select(-id))) %>%
  rename(lat = latitude, long = longitude, elev = elevation)

aus_climate_cubble <- aus_climate_raw %>%
  cubble::as_cubble(index = date, key = id, coords = c(long, lat))

############################################################
climate_aus <- aus_climate_cubble %>%
  stretch() %>%
  mutate(tmax = tmax/10, tmin = tmin/ 10)%>%
  tamp()
usethis::use_data(climate_aus, overwrite = TRUE)

############################################################
set.seed(123)
climate_subset <- climate_aus %>%
  slice_sample(n = 30)
usethis::use_data(climate_subset, overwrite = TRUE)

############################################################
id_vec <- c("ASN00009021", "ASN00010311", "ASN00010614", "ASN00014015", "ASN00015131")
climate_flat <- climate_aus %>%
  filter(id %in% id_vec) %>%
  unnest() %>%
  ungroup()
usethis::use_data(climate_flat, overwrite = TRUE)

############################################################
stations <- climate_aus %>% select(id: wmo_id)
usethis::use_data(stations, overwrite = TRUE)

climate <- climate_aus %>% stretch() %>% as_tibble()
usethis::use_data(climate, overwrite = TRUE)
