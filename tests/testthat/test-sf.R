test_that("cubble works with sf", {
  expect_snapshot(climate_mel |> make_spatial_sf())
})
