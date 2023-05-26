library(lubridate)
library(cubble)
library(dplyr)

test_that("basic creation",{
  cb <- make_cubble(
    spatial = stations, temporal = meteo,
    key = id, index = date, coords = c(long, lat)
  )
  expect_true(is_cubble(cb))
})


test_that("when there are mismatch",{
  stations2 <- stations %>% head(2)
  expect_message(
    cb <- make_cubble(
      spatial = stations2, temporal = meteo,
      key = id, index = date, coords = c(long, lat)
    )
  )

  # check_key
  key_summary <- check_key(spatial = stations2, temporal = meteo)

  # resolve unmatch
  meteo2 <- meteo %>% filter(!id %in% key_summary$others$temporal)

  # create cubble again with no warning
  expect_message(
    cb <- make_cubble(
      spatial = stations2, temporal = meteo2,
      key = id, index = date, coords = c(long, lat)
    ), regexp = NA
  )

})


test_that("key variable has different names in spaital and temporal data",{
  meteo2 <- meteo %>% rename(station = id)

  expect_message(
    cb <- make_cubble(
      spatial = stations, temporal = meteo2, by = c("id" = "station"),
      key = id, index = date, coords = c(long, lat)
    ), regexp = NA
  )

})


test_that("auto match on names",{
  # deliberately make the unmatch perth vs. perth airport
  ts_wname <- meteo %>% left_join(stations %>% select(id, name)) %>% select(-id)
  stations2 <- stations %>% mutate(name = ifelse(id == "ASN00086038", "essendon", name))

  expect_message(
    make_cubble(
      spatial = stations2, temporal = ts_wname,
      key = name, index = date, coords = c(long, lat)
    )
  )

  outputs <- check_key(spatial = stations2, temporal = ts_wname)
  expect_equal(outputs$others$temporal, "essendon airport")
  expect_equal(outputs$others$spatial, "essendon")
})

