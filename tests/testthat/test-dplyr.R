test_that("dplyr verbs work on nest/long cubble", {
  # setup
 cb_nested <- climate_mel
 cb_long <- face_temporal(climate_mel)

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

  # filter - currently filter.spatial_cubble_df, dply_row_slice,
  expect_error(cb_nested %>% filter(elev > 40), NA)
  expect_error(cb_long %>% filter(prcp > 0), NA)
  expect_error((res <- a %>% filter(elev > 40)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% filter(prcp > 0)), NA)
  expect_true(test_class_tsibble(res))

  # mutate - curerntly mutate.spatial_cubble_df, dply_col_modify
  expect_error(cb_nested %>% mutate(elev2 = elev + 10), NA)
  expect_error(cb_long %>% mutate(prcp2 = prcp + 10), NA)
  expect_error((res <- a %>% mutate(elev2 = elev + 10)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% mutate(prcp2 = prcp + 10)), NA)
  expect_true(test_class_tsibble(res))

  # arrange - currently arrange.spatial_cubble_df, arrange.temporal_cubble_df
  expect_error((res <- cb_nested %>% arrange(wmo_id)), NA)
  expect_error((res <- cb_long %>% arrange(prcp)), NA)
  expect_error((res <- a %>% arrange(wmo_id)), NA)
  expect_true(test_class_sf(res))
  expect_warning((res <- b %>% arrange(prcp)))
  expect_true(test_class_tsibble(res))

  # select -  select.spatial_cubble_df,  select.temporal_cubble_df
  expect_error((res <- a2 %>% select(-elev)), NA)
  expect_true(test_class_sf(res))
  #expect_error((res <- b %>% select(-prcp)), NA) # not working
  #expect_true(test_class_tsibble(res))


  # rename - rename.spatial_cubble_df, rename.temporal_cubble_df
  expect_error((res <- cb_nested %>% rename(elev2 = elev)), NA)
  expect_error((res <- cb_long %>% rename(prcp2 = prcp)), NA)
  # rename on key attributes
  expect_error((res <- cb_nested %>% rename(id2 = id)), NA)
  expect_error((res <- cb_nested %>% rename(long2 = long)), NA)
  expect_error((res <- cb_long %>% rename(date2 = date)), NA)
  expect_error((res <- cb_long %>% rename(id2 = id) %>% face_spatial()), NA)
  expect_error((res <- a %>% rename(elev2 = elev)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% rename(prcp2 = prcp)), NA)
  expect_true(test_class_tsibble(res))

})


