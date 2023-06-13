## code to prepare `DATASET` dataset goes here
library(rnoaa)
library(tidyverse)
library(cubble)
all_stations <- ghcnd_stations() %>%
  filter(str_starts(id, "ASN")) %>%
  filter(last_year >= 2020) %>%
  mutate(wmo_id = as.numeric(wmo_id),
         name = str_to_lower(name)) %>%
  select(-state, -gsn_flag)

cand <- all_stations %>%
  filter(element == "TMAX",
         first_year < 1970,
         !is.na(wmo_id))

historical_tmax <- cand %>%
  rowwise() %>%
  mutate(ts = list(meteo_pull_monitors(
    monitors = id, var = "TMAX",
    date_min = glue::glue("{first_year}-01-01"),
    date_max = glue::glue("{last_year}-12-31")) |>
      select(-id))) %>%
  as_cubble(index = date, key = id, coords = c(longitude, latitude))

tmax_hist <- historical_tmax |>
  face_temporal() |>
  mutate(tmax = tmax / 10) |>
  filter(between(lubridate::year(date), 1970, 1975) |
           lubridate::year(date) > 2015) |>
  mutate(yearmonth = tsibble::yearmonth(date)) |>
  group_by(yearmonth) |>
  summarise(tmax = mean(tmax, na.rm = TRUE)) |>
  face_spatial() |>
  select(-c(element:last_year)) |>
  filter(stringr::str_sub(id, 7, 8) >= 76)

usethis::use_data(tmax_hist, overwrite = TRUE,  compress = "gzip")
