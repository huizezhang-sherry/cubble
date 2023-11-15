test_that("unfold() works", {
  expect_snapshot(climate_mel |> face_temporal() |> unfold(long, lat))
  expect_snapshot(climate_mel |> face_temporal() |> unfold(dplyr::starts_with("l")))
})
