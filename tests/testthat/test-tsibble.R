test_that("cubble works for tsibble objects", {
  expect_snapshot(climate_mel |> face_temporal() |> make_temporal_tsibble())
  expect_snapshot(climate_aus |> face_temporal() |> fill_gaps())
  expect_snapshot(climate_aus |> face_temporal() |> scan_gaps())

  expect_snapshot(
    res <- make_cubble(stations_sf, meteo_ts) |>
      face_temporal() |>
      tsibble::index_by(week = lubridate::week(date))
  )

  expect_snapshot(res |> summarise(tmax = mean(tmax, na.rm = TRUE)))


})
