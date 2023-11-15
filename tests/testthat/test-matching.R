test_that("matching examples work", {
  library(dplyr)
  climate_aus <- mutate(climate_aus, type = "climate")
  expect_snapshot(match_spatial(climate_aus, river))
  # turn with different distance calculation:
  expect_snapshot(match_spatial(climate_aus, river, which = "Hausdorff"))
  # tune the number of matches in each group
  expect_snapshot(
    match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2))

  a1 <- match_spatial(climate_aus, river, return_cubble = TRUE) |> bind_rows()
  expect_snapshot(
    match_temporal(a1, data_id = type, match_id = group,
                   temporal_by = c("prcp" = "Water_course_level"))
    )
})
