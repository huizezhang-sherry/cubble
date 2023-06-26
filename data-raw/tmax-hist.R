library(tidyverse)
library(cubble)
all_stations <- rnoaa::ghcnd_stations() |>
  filter(str_starts(id, "ASN")) |> # Australian stations start wiht "ASN"
  filter(last_year >= 2020) |>
  mutate(wmo_id = as.numeric(wmo_id), name = str_to_lower(name)) |>
  select(-state, -gsn_flag) |>
  select(id, longitude, latitude, elevation, name,
         wmo_id, element, first_year, last_year) |>
  rename(long = longitude, lat = latitude, elev = elevation)

tmax_stations <- all_stations |>
  filter(element == "TMAX", first_year < 1970, !is.na(wmo_id)) %>%
  filter(between(as.numeric(stringr::str_sub(id, 7, 8)), 46, 90)) %>%
  filter(!id %in% c("ASN00047048", "ASN00085279", "ASN00085298")) %>%
  select(-c(element:last_year))

tmax71 <- rnoaa::meteo_pull_monitors(
  monitors = tmax_stations$id, var = "TMAX",
  date_min = glue::glue("1971-01-01"),
  date_max = glue::glue("1975-12-31")
)

tmax16 <- rnoaa::meteo_pull_monitors(
  monitors = tmax_stations$id, var = "TMAX",
  date_min = glue::glue("2016-01-01"),
  date_max = glue::glue("2020-12-31")
)

tmax <- tmax71 %>%
  bind_rows(tmax16) %>%
  arrange(id, date) %>%
  mutate(tmax = tmax /10)

historical_tmax <- make_cubble(
  spatial = tmax_stations, temporal = tmax,
  key = id, index = date, coords = c(long, lat))

usethis::use_data(historical_tmax, overwrite = TRUE)
