library(dplyr)
library(sf)
library(tsibble)

raw <- weatherdata::historical_tmax %>%
  filter(between(stringr::str_sub(id, 7, 8), 46, 75))

stations_sf <- raw %>%
  select(id: wmo_id) %>%
  ungroup() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4283, remove = FALSE)

ts <- raw %>%
  face_temporal() %>%
  as_tsibble(key = id) %>%
  slice_sample(prop = 0.9) %>%
  arrange(id, date)

weather <- as_cubble(
  list(spatial = stations_sf, temporal = ts),
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



