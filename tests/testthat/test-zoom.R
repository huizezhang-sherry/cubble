test_that("switch a cubble in list-column form into a long form (no tsibble)", {
  expect_equal("cubble_df" %in% class(climate_flat %>% global(station) %>% zoom()), TRUE)
  expect_equal("grouped_df" %in% class(climate_flat %>% global(station) %>% zoom()), TRUE)
})

test_that("switch a cubble in list-column form into a long form (with tsibble)", {
  a <- tsibble::pedestrian %>% select(-Date) %>% global(Sensor) %>% zoom()
  expect_equal("cubble_df" %in% class(a), TRUE)
  expect_equal("grouped_df" %in% class(a), TRUE)
  expect_equal("tbl_ts" %in% class(a), TRUE)
  expect_equal("grouped_ts" %in% class(a), TRUE)
})

