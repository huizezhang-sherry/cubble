test_that("multiplication works", {
  # setup
  a <- make_cubble(spatial = stations_sf, temporal = meteo_ts)
  b <- a %>% face_temporal()
  test_class_tsibble <- function(obj){
    class(obj)[1] == "temporal_cubble_df" &&
      class(obj)[2] == "cubble_df" &&
      class(obj)[3] == "tbl_ts"
  }

  # fill gaps
  expect_error((res <- climate_aus %>% face_temporal() %>% fill_gaps()), NA)
  expect_error((res <- b %>% fill_gaps()), NA)
  expect_true(test_class_tsibble(res))

  # fill gaps
  expect_error((res <- climate_aus %>% face_temporal() %>% scan_gaps()), NA)
  expect_error((res <- b %>% scan_gaps()), NA)
  expect_true(test_class_tsibble(res))
})


