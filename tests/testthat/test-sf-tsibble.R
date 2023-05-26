library(dplyr)
library(sf)
library(tsibble)

weather <- make_cubble(
  spatial = stations_sf, temporal = meteo_ts,
  key = id, index = date, coords = c("long", "lat"))

####################################
####################################
test_that("the sf class is preserved at creation", {
  expect_true(inherits(weather, "sf"))
})

test_that("the tsibble class is preserved at creation", {
  expect_true(inherits(weather$ts[[1]], "tbl_ts"))
})

####################################
####################################
long <- weather %>% face_temporal()
test_that("the tsibble class is preserved after face_temporal()", {
  expect_true(inherits(long, "tbl_ts"))
})

test_that("the sf class is preserved after face_temporal() in spatial()", {
  expect_true(inherits(spatial(long), "sf"))
})

####################################
####################################
back <- weather %>% face_temporal() %>% face_spatial()
test_that("the tsibble class is preserved after pivoting back and forth", {
  expect_true(inherits(back$ts[[1]], "tbl_ts"))
})

test_that("the sf class is preserved after pivoting back and forth", {
  expect_true(inherits(back, "sf"))
})

####################################
####################################
out <- weather %>% face_temporal() %>% fill_gaps()
test_that("the tsibble class is preserved after fill_gaps()", {
  expect_true(inherits(out, "tbl_ts"))
})

test_that("the sf class is preserved after fill_gaps() in spaital()", {
  expect_true(inherits(spatial(out), "sf"))
})

####################################
####################################
small <- weather[1:5,]
test_that("the tsibble class is preserved after subsetting", {
  expect_true(inherits(small$ts[[1]], "tbl_ts"))
})

test_that("the sf class is preserved after subsetting", {
  expect_true(inherits(small, "sf"))
})



