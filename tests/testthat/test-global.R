test_that("classes are set up when creating a cubble from tibble", {
  expect_equal("cubble_df" %in% class(climate_flat %>% global(station)), TRUE)
  expect_equal("rowwise_df" %in% class(climate_flat %>% global(station)), TRUE)
})

test_that("creating a cubble from tsibble", {
  a <- tsibble::pedestrian %>% global(Sensor)
  expect_equal("cubble_df" %in% class(a), TRUE)
  expect_equal("rowwise_df" %in% class(a), TRUE)
  expect_equal("tbl_ts" %in% class(a$ts[[1]]), TRUE)
})

