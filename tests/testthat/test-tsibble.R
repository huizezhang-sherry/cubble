test_that("cubble works for tsibble objects", {
  expect_snapshot(climate_mel |> face_temporal() |> make_temporal_tsibble())
  expect_snapshot(climate_aus |> face_temporal() |> fill_gaps())
  expect_snapshot(climate_aus |> face_temporal() |> scan_gaps())
})
