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
