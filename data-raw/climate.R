library(rnoaa)
library(tidyverse)
library(cubble)

# all the Australian stations that are still active in 2020 and have all of
# the three PRCP, TMAX, and TMIN recorded
aus_stations <- ghcnd_stations() |>
  filter(str_starts(id, "ASN")) |>
  filter(last_year >= 2020) |>
  mutate(wmo_id = as.numeric(wmo_id),
         name = str_to_lower(name)) |>
  select(-state, -gsn_flag) |>
  filter(element %in% c("PRCP", "TMAX", "TMIN")) |>
  nest(element: last_year) |>
  rowwise() |>
  filter(nrow(data) == 3) |>
  select(-data) |>
  filter(id != "ASN00079105")

# query the climate data for the selected station (may take a while if run for
# the first time)
aus_climate_raw <- aus_stations |>
  rowwise() |>
  mutate(ts = list(meteo_pull_monitors(
    monitors = id, var = c("PRCP", "TMAX", "TMIN"),
    date_min = "2020-01-01",
    date_max = "2020-12-31") |>
      select(-id))) |>
  rename(lat = latitude, long = longitude, elev = elevation)

clean <- aus_climate_raw |>
  select(id, long, lat, elev, name, wmo_id, ts) %>%
  unnest(ts) |>
  mutate(tmax = tmax/10, tmin = tmin/ 10)

climate_aus <- clean |>
  cubble::as_cubble(index = date, key = id, coords = c(long, lat))
usethis::use_data(climate_aus, overwrite = TRUE)

############################################################
id_vec <- c("ASN00086282", "ASN00086038", "ASN00086077")
climate_flat <- clean |> filter(id %in% id_vec, date <= as.Date("2020-01-10"))
usethis::use_data(climate_flat, overwrite = TRUE)

stations <- climate_flat |> select(id: wmo_id) |> distinct()
usethis::use_data(stations, overwrite = TRUE)

stations_sf <- stations |>
  sf::st_as_sf(coords=  c("long", "lat"),  crs = sf::st_crs("OGC:CRS84"))
usethis::use_data(stations_sf, overwrite = TRUE)

meteo <- climate_flat |> select(id, date: tmin)
usethis::use_data(meteo, overwrite = TRUE)

meteo_ts <- meteo |> tsibble::as_tsibble(key = id, index = date)
usethis::use_data(meteo_ts, overwrite = TRUE)

climate_mel <- make_cubble(
  spatial = stations, temporal = meteo,
  key = id, index = date, coords = c(long, lat)
  )
usethis::use_data(climate_mel, overwrite = TRUE)

