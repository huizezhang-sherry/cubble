test_that("face_spatial() and face_temporal() works", {
  expect_snapshot(climate_mel |> face_temporal())
  expect_snapshot(climate_mel |> face_temporal() |> face_spatial())
})
