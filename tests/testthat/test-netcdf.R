library(lubridate)
test_that("NetCDF: parse origin time with timezone", {
  input <- parse_time("seconds since 1992-10-8 15:15:42.5 -6:00")

  dttm <- format(input)
  expected_dttm <- "1992-10-08 15:15:42"
  expect_equal(dttm, expected_dttm)

  tz <- lubridate::tz(input)
  expected_tz <- "America/Bahia_Banderas"
  expect_equal(tz, expected_tz)
})

test_that("NetCDF: parse origin time with date time", {
  input <- parse_time("hours since 1900-01-01 03:00:0.0")

  dttm <- format(input)
  expected_dttm <- "1900-01-01 03:00:00"
  expect_equal(dttm, expected_dttm)

  tz <- lubridate::tz(input)
  expected_tz <- "UTC"
  expect_equal(tz, expected_tz)
})

test_that("NetCDF: parse origin time with only date", {
  input <- parse_time("days since 2000-1-1")
  dttm <- format(input)
  expected_dttm <- "2000-01-01"
  expect_equal(dttm, expected_dttm)

  tz <- lubridate::tz(input)
  expected_tz <- "UTC"
  expect_equal(tz, expected_tz)
})
