library(rnoaa)
library(tidyverse)
library(lubridate)
library(progress)
library(sf)



set.seed(123)
station_sample <- station_selected %>%
  filter(row_number() %in% sample(1:nrow(station_selected), size = 50))
library(rmapshaper)
library(ggplot2)
state_map <- ms_simplify(ozmaps::abs_ste, keep = 2e-3)
plot_map(state_map) +
  geom_point(data = close, aes(x = long, y = lat, color = aws))

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
  mutate(across(c(tmax, tmin, prcp, tavg, starts_with("md")), ~ .x / 10)) %>%
  select(date:tmin, tavg)

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

# save in the inst/extdata to reduce the data/ size, even after compression
save(climate, file = here::here("inst/extdata/climate.rda"), compress = "xz")

############################################################
############################################################
climate_large <- climate %>%
  left_join(station, by = c("station" = "id")) %>%
  as_cubble(key = station, index = date, coords = c(long, lat)) %>%
  mutate(tmax_missing = all(is.na(ts$tmax)),
         tmin_missing = all(is.na(ts$tmin)),
         tmin_missing_first = is.na(ts$tmin[1])) %>%
  filter(!tmax_missing, !tmin_missing, !tmin_missing_first) %>%
  mutate(station = fct_drop(station)) %>%
  select(-contains("missing"))

usethis::use_data(station, overwrite = TRUE, compress = "xz")
usethis::use_data(climate_large, overwrite = TRUE, compress = "xz")

# temporarily include oz_climate data for easier examples (filtered with year >= 2015)
############################################################
############################################################
climate_small <- oz_climate %>%
  mutate(
    station = as.factor(station),
    name = as.factor(str_to_lower(str_remove(name, ", AS")))) %>%
  filter(between(year(date), 2015, 2020)) %>%
  as_tsibble(index = date, key = station) %>%
  as_cubble(key = station, index = date, coords = c(long, lat)) %>%
  mutate(tmax_missing = all(is.na(ts$tmax)),
         tmin_missing = all(is.na(ts$tmin)),
         tmin_missing_first = is.na(ts$tmin[1])) %>%
  filter(!tmax_missing, !tmin_missing, !tmin_missing_first,
         name != "coffs harbour mo",# too many missing from 2016 - 2020 for coffs harbour mo
         !station %in% c("ASN00016098", # almost all missing for tmin
                         "ASN00090015")  # have no prcp recorded (either 0 or NA)
) %>%
  mutate(station = fct_drop(station)) %>%
  select(-contains("missing"))

climate_flat <- climate_small %>%
  mutate(ts = list(as_tibble(ts))) %>%
  unnest() %>%
  ungroup() %>%
  filter(year(date) == 2020, station %in% c("ASN00001019", "ASN00002012")) %>%
  mutate(station = fct_drop(station)) %>%
  as_tibble()

usethis::use_data(climate_flat, overwrite = TRUE, compress = "xz")
usethis::use_data(climate_small, overwrite = TRUE, compress = "xz")

