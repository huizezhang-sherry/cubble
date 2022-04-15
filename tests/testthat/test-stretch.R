test_that("switch a cubble in the nested form into a long form (no tsibble)", {
  out <- climate_flat |>
    as_cubble(key = id, index = date, coords = c(long, lat)) |>
    face_temporal()
  expect_equal("cubble_df" %in% class(out), TRUE)
  expect_equal("grouped_df" %in% class(out), TRUE)
})

test_that("switch a cubble in the nested form into a long form (with tsibble)", {
  # a <- tsibble::pedestrian |> select(-Date) |> face_spatial(Sensor) |> face_temporal()
  # expect_equal("cubble_df" %in% class(a), TRUE)
  # expect_equal("grouped_df" %in% class(a), TRUE)
  # expect_equal("tbl_ts" %in% class(a), TRUE)
  # expect_equal("grouped_ts" %in% class(a), TRUE)
})

