test_that("classes are set up when creating a cubble from tibble", {
  expect_equal("cubble_df" %in% class(climate_flat %>% tamp(station)), TRUE)
  expect_equal("rowwise_df" %in% class(climate_flat %>% tamp(station)), TRUE)
})

test_that("creating a cubble from tsibble", {
  a <- tsibble::pedestrian %>% tamp(Sensor)
  expect_equal("cubble_df" %in% class(a), TRUE)
  expect_equal("rowwise_df" %in% class(a), TRUE)
  expect_equal("tbl_ts" %in% class(a$ts[[1]]), TRUE)
})

