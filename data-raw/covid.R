## code to prepare `covid` dataset goes here
library(tidyverse)
library(sf)
raw <- read_csv(here::here("data/NCOV_COVID_Cases_by_LGA_Source_20220324.csv"),
                col_names = c("date", "postcode", "source", "lga"), skip = 1)

covid <- raw |>
  count(date, lga, source, postcode) |>
  filter(date >= as.Date("2022-01-01")) |>
  filter(source == "Contact with a confirmed case") |>
  select(-source) |>
  group_by(date, lga) |>
  count() |>
  ungroup(date) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  mutate(avg_7day = slider::slide_dbl(n, mean, .before = 6)) |>
  tsibble::as_tsibble(key = lga, index = date) |>
  tsibble::fill_gaps(.full = TRUE) %>%
  ungroup()

usethis::use_data(covid, overwrite = TRUE)


lga <- strayr::read_absmap("lga2018") |>
  filter(state_name_2016 == "Victoria") |>
  select(lga_name_2018, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.1)

usethis::use_data(lga, overwrite = TRUE)
