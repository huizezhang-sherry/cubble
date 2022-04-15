test_that("classes are set up when creating a cubble from tibble", {
  out <- climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))
  expect_equal("cubble_df" %in% class(out), TRUE)
  expect_equal("rowwise_df" %in% class(out), TRUE)
})

test_that("creating a cubble from tsibble", {
  #
  # a <- tsibble::pedestrian |> as_cubble(Sensor)
  # expect_equal("cubble_df" %in% class(a), TRUE)
  # expect_equal("rowwise_df" %in% class(a), TRUE)
  # expect_equal("tbl_ts" %in% class(a$ts[[1]]), TRUE)
})

