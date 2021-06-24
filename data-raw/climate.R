## code to prepare `climate` dataset goes here
library(rnoaa)
library(tidyverse)
library(progress)

# extract all the australian stations
all_stations <- ghcnd_stations() %>%
  filter(str_detect(id, "ASN")) %>%
  filter(last_year >= 2020, first_year <= 2000) %>%
  mutate(wmo_id = as.numeric(wmo_id))

# create station metadata
station_meta <- all_stations %>%
  select(-c(element, first_year, last_year)) %>%
  distinct() %>%
  mutate(
    id = as.factor(id),
    name = str_to_title(name)
  ) %>%
  rename(lat = latitude, long = longitude, elev = elevation)

# create climate metadata to query
climate_meta <- all_stations %>%
  select(id, element, first_year, last_year) %>%
  mutate(row_id = row_number()) %>%
  nest(meta = id:last_year)

# query climate sheet based on meta
pb <- progress_bar$new(
  total = nrow(main_raw),
  format = "Number of  station: [:bar] :current/ :total"
)

climate_raw <- climate_meta %>%
  mutate(raw = map(meta, ~ {
    pb$tick()
    ncdc(
      datasetid = "GHCND", stationid = glue::glue("GHCND:", .x$id),
      startdate = "2020-01-01", enddate = "2020-12-31",
      limit = 1000, datatypeid = .x$element
    )
  })) %>%
  transmute(data = map(raw, ~ .x$data)) %>%
  unnest(data)

############################################################
############################################################

# cleaning climate data queried from rnoaa
climate <- climate_raw %>%
  mutate(
    station = as.factor(str_sub(station, start = 7)),
    date = as.Date(date),
    datatype = tolower(datatype)
  ) %>%
  as_tsibble(key = c(station, datatype), index = date) %>%
  pivot_wider(id_cols = c(date, station), names_from = datatype, values_from = value) %>%
  mutate(across(c(tmax, tmin, prcp, tavg, starts_with("md")), ~ .x / 10))

# filter out good stations with less than 3 measures
climate_nested <- climate %>%
  nest(-station) %>%
  mutate(var_recorded = map_dbl(
    data,
    function(dt) {
      dt %>%
        as_tibble() %>%
        group_by(datatype) %>%
        summarise(missing = sum(value, na.rm = TRUE)) %>%
        filter(missing != 0) %>%
        nrow()
    }
  ))

good <- climate_nested %>%
  filter(var_recorded >= 3) %>%
  pull(station)

station <- station_meta %>%
  select(-c(state, gsn_flag)) %>%
  filter(id %in% good)

############################################################
############################################################
samples <- sample(station$id, 100)
nested <- climate %>%
  nest_by(station) %>%
  left_join(station, by = c("station" = "id")) %>%
  mutate(data = list(as_tsibble(data, index = date))) %>%
  filter(station %in% samples)

usethis::use_data(climate, overwrite = TRUE)
usethis::use_data(station, overwrite = TRUE)
usethis::use_data(nested, overwrite = TRUE)

# temporarily include oz_climate data for easier examples (filtered with year >= 2015)
usethis::use_data(oz_climate, overwrite = TRUE)
