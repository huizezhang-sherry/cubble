test_that("cubble works with sf", {
  expect_snapshot(climate_mel |> make_spatial_sf())
  expect_snapshot(climate_mel |> rename(x = long, y = lat) |> make_spatial_sf())
})
