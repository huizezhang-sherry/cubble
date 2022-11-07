## code to prepare `prcp_aus` dataset goes here
library(rnoaa)
library(tidyverse)
library(cubble)

aus_stations <- all_stations %>%
  filter(element == "PRCP", !is.na(wmo_id))

# query the climate data for the selected station (may take a while if run for
# the first time)
raw <- aus_stations %>%
  rowwise() %>%
  mutate(ts = list(
    meteo_pull_monitors(
      monitors = id,
      var = c("PRCP"),
      date_min = "2016-01-01",
      date_max = "2020-12-31"
    ) %>%
      select(-id)
  )
  )

raw_cb <- raw %>%
  select(-c(element:last_year)) %>%
  unnest(ts) %>%
  cubble::as_cubble(key = id, index = date, coords = c(long, lat))

prcp_aus <- raw_cb %>%
  face_temporal(ts) %>%
  mutate(wk = lubridate::week(date)) %>%
  group_by(wk) %>%
  summarise(prcp = sum(prcp, na.rm = TRUE)) %>%
  face_spatial()

usethis::use_data(prcp_aus, overwrite = TRUE)
