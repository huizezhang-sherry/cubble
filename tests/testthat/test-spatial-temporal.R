cb_nested <- climate_mel
cb_long <- face_temporal(climate_mel)

test_that("Apply face_temporal on nest should get long and vice versa",{
  expect_identical(cb_nested %>% face_temporal(), cb_long)
  expect_identical(cb_long %>% face_spatial(), cb_nested)
})


test_that("Apply face_temporal & face_temporal should result
          in the identical object", {
  expect_identical(cb_nested %>% face_temporal() %>% face_spatial(), cb_nested)
  expect_identical(cb_long %>% face_spatial() %>% face_temporal(), cb_long)
})




test_that("when there are sf and tsibble",{
  # setup
  cb_nested <- make_cubble(spatial = stations_sf, temporal = meteo_ts)
  cb_long <- face_temporal(cb_nested)

  expect_identical(cb_nested %>% face_temporal(), cb_long)
  expect_identical(cb_long %>% face_spatial(), cb_nested)
  expect_identical(cb_nested %>% face_temporal() %>% face_spatial(), cb_nested)
  expect_identical(cb_long %>% face_spatial() %>% face_temporal(), cb_long)
})


test_that("make cubble with partial match", {
  lga2 <- lga |>
    rename(lga = lga_name_2018) |>
    dplyr::filter(stringr::str_detect(lga, "Kingston|Latrobe"))
  covid2 <- covid |> filter(stringr::str_detect(lga, "Kingston|Latrobe")) |>
    as_tsibble(key = lga)
  check_res <- check_key(spatial = lga2, temporal = covid2)

  expect_snapshot(
    make_cubble(lga2, covid2, potential_match = check_res) |>
      dplyr::pull(lga))

  expect_snapshot(
    make_cubble(lga2, covid2,
                potential_match = check_res, key_use = "spatial") |>
      dplyr::pull(lga)
  )
})
