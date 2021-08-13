test_that("switch a cubble in the nested form into a long form (no tsibble)", {
  expect_equal("cubble_df" %in% class(climate_flat %>% tamp(station) %>% stretch()), TRUE)
  expect_equal("grouped_df" %in% class(climate_flat %>% tamp(station) %>% stretch()), TRUE)
})

test_that("switch a cubble in the nested form into a long form (with tsibble)", {
  a <- tsibble::pedestrian %>% select(-Date) %>% tamp(Sensor) %>% stretch()
  expect_equal("cubble_df" %in% class(a), TRUE)
  expect_equal("grouped_df" %in% class(a), TRUE)
  expect_equal("tbl_ts" %in% class(a), TRUE)
  expect_equal("grouped_ts" %in% class(a), TRUE)
})

