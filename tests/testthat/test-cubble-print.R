test_that("from issue 21", {
  a <- climate_mel |>
    rename(x = long, y = lat) |>
    face_temporal() |>
    unfold(x)

  expect_snapshot(a)
})
