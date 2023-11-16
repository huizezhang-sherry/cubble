test_that("unfold() works", {
  expect_snapshot(climate_mel |> face_temporal() |> unfold(long, lat))
  expect_snapshot(climate_mel |> face_temporal() |> unfold(dplyr::starts_with("l")))
  expect_snapshot(climate_mel |> unfold(), error = TRUE)
  expect_snapshot(climate_mel |> face_temporal() |> make_temporal_tsibble() |> unfold(long, lat))
})
