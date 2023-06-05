test_that("dplyr verbs work on nest/long cubble", {
  # setup
  a <- make_cubble(spatial = stations, temporal = meteo,
                   key = id, index = date, coords = c(long, lat))
  b <- a %>% face_temporal()
  c <- b %>% face_spatial()

  # dply_row_slice
  expect_error(a %>% filter(elev > 40), NA)
  expect_error(b %>% filter(prcp > 0), NA)

  # dply_col_modify
  expect_error(a %>% mutate(elev2 = elev + 10), NA)
  expect_error(b %>% mutate(prcp2 = prcp + 10), NA)
})


##########################################################################################
test_that("dplyr verbs also work when there is sf /tsibble", {
  # setup
  a <- make_cubble(spatial = stations_sf, temporal = meteo_ts)
  b <- a %>% face_temporal()
  c <- b %>% face_spatial()

  test_class_sf <- function(obj){
    class(obj)[1] == "spatial_cubble_df" &&
      class(obj)[2] == "cubble_df" &&
      class(obj)[3] == "sf"
  }

  test_class_tsibble <- function(obj){
    class(obj)[1] == "temporal_cubble_df" &&
      class(obj)[2] == "cubble_df" &&
      class(obj)[3] == "tbl_ts"
  }

  # mutate - dply_row_slice
  expect_error((res <- a %>% filter(elev > 40)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% filter(prcp > 0)), NA)
  expect_true(test_class_tsibble(res))

  # mutate - dply_col_modify
  expect_error((res <- a %>% mutate(elev2 = elev + 10)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% mutate(prcp2 = prcp + 10)), NA)
  expect_true(test_class_tsibble(res))

  # arrange -  sf: dplyr_row_slice, tsibble: arrange.temporal_cubble_df
  expect_error((res <- a %>% arrange(wmo_id)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% arrange(prcp)), NA)
  expect_true(test_class_tsibble(res))

  # select -sf: select.spatial_cubble_df, tsibble: arrange.temporal_cubble_df
  expect_error((res <- a %>% select(-elev)), NA)
  expect_true(test_class_sf(res))
  #expect_error((res <- b %>% select(-prcp)), NA) # not working
  #expect_true(test_class_tsibble(res))

})


