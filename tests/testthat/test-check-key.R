library(lubridate)
library(cubble)
library(dplyr)
stations
ts <- climate %>% filter(lubridate::month(date) == 1)

test_that("basic creation",{
  cb <- as_cubble(
    list(spatial = stations, temporal = ts),
    key = id, index = date, coords = c(long, lat)
  )
  expect_true(is_cubble(cb))
})


test_that("when there are mismatch",{
  stations3 <- stations %>% head(3)
  expect_message(
    cb <- as_cubble(
      list(spatial = stations3, temporal = ts),
      key = id, index = date, coords = c(long, lat)
    )
  )

  # check_key
  key_summary <- check_key(spatial = stations3, temporal = ts)

  # resolve unmatch
  ts3 <- ts %>% filter(!id %in% key_summary$others$temporal)

  # create cubble again with no warning
  expect_message(
    cb <- as_cubble(
      list(spatial = stations3, temporal = ts3),
      key = id, index = date, coords = c(long, lat)
    ), regexp = NA
  )

})


test_that("key variable has different names in spaital and temporal data",{
  ts2 <- ts %>% rename(station = id)

  expect_message(
    cb <- as_cubble(
      list(spatial = stations, temporal = ts2), by = c("id" = "station"),
      key = id, index = date, coords = c(long, lat)
    ), regexp = NA
  )

})


test_that("auto match on names",{
  # deliberately make the unmatch perth vs. perth airport
  ts_wname <- climate %>% left_join(stations %>% select(id, name)) %>% select(-id)
  stations2 <- stations %>% mutate(name = ifelse(id == "ASN00009021", "perth", name))

  expect_message(
    as_cubble(
      list(spatial = stations2, temporal = ts_wname),
      key = name, index = date, coords = c(long, lat)
    )
  )

  outputs <- check_key(spatial = stations2, temporal = ts_wname)
  expect_equal(outputs$others$temporal, "perth airport")
  expect_equal(outputs$others$spatial, "perth")
})

