## code to prepare `world_climate` dataset goes here
library(tidyverse)
library(countrycode)
library(rnoaa)
library(cubble)

world <- ghcnd_stations() %>%
  rename(lat = latitude, long = longitude, elev = elevation) %>%
  mutate(country_abb = str_sub(id, 1, 2)) %>%
  filter(country_abb %in% c("US", "AS", "AU", "SP", "FR", "CH")) %>%
  filter((wmo_id) != "", gsn_flag == "GSN") %>%
  filter(first_year <= 2016, last_year >= 2020) %>%
  filter(element %in% c("PRCP", "TMAX", "TMIN")) %>%
  nest(element:last_year) %>%
  rowwise() %>%
  filter(nrow(data) == 3) %>%
  select(-data, -state, -gsn_flag) %>%
  mutate(
    # country = countrycode(country_abb,
    #                       origin = "fips",
    #                       destination = "country.name"),
    continent = countrycode(country_abb,
                            origin = "fips",
                            destination = "continent")
  ) %>%
  select(-country_abb)

world_raw <- world %>%
  rowwise() %>%
  mutate(ts = list(meteo_pull_monitors(monitors = id,
                                       var = c("PRCP", "TMAX", "TMIN"),
                                       date_min = "2016-01-01",
                                       date_max = "2020-12-31") %>%
                     select(-id)))

world_climate <- world_raw %>%
  as_cubble(index = date, key = id, coords = c(long, lat)) %>%
  stretch() %>%
  mutate(tmax = tmax / 10, tmin = tmin / 10) %>%
  tamp()

usethis::use_data(world_climate, overwrite = TRUE, compress = "bzip2")

