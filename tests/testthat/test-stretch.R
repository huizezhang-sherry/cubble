test_that("switch a cubble in the nested form into a long form (no tsibble)", {
  out <- climate_flat %>%
    as_cubble(key = station, index = date, coords = c(long, lat)) %>%
    stretch()
  expect_equal("cubble_df" %in% class(out), TRUE)
  expect_equal("grouped_df" %in% class(out), TRUE)
})

test_that("switch a cubble in the nested form into a long form (with tsibble)", {
  # a <- tsibble::pedestrian %>% select(-Date) %>% tamp(Sensor) %>% stretch()
  # expect_equal("cubble_df" %in% class(a), TRUE)
  # expect_equal("grouped_df" %in% class(a), TRUE)
  # expect_equal("tbl_ts" %in% class(a), TRUE)
  # expect_equal("grouped_ts" %in% class(a), TRUE)
})

