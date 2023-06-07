library(dplyr)
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
  expect_error((res <- a %>% select(-elev)), NA)
  expect_true(test_class_sf(res))
  #expect_error((res <- b %>% select(-prcp)), NA) # not working
  #expect_true(test_class_tsibble(res))


  # rename - rename.spatial_cubble_df, rename.temporal_cubble_df
  expect_error((res <- cb_nested %>% rename(elev2 = elev)), NA)
  expect_error((res <- cb_long %>% rename(prcp2 = prcp)), NA)
  # rename on key attributes
  expect_error(cb_nested %>% rename(id2 = id), NA)
  expect_error(cb_nested %>% rename(long2 = long), NA)
  expect_error(cb_long %>% rename(date2 = date), NA)
  expect_error(cb_long %>% rename(id2 = id) %>% face_spatial(), NA)
  expect_error((res <- a %>% rename(elev2 = elev)), NA)
  expect_true(test_class_sf(res))
  expect_error((res <- b %>% rename(prcp2 = prcp)), NA)
  expect_true(test_class_tsibble(res))


  # join - mutate_join - dplyr_reconstruct()
  # join - filter_join - dplyr_row_slice()
  df1 <- climate_mel %>% as_tibble() %>% select(id, name) %>% head(2)
  nested <- climate_mel %>% select(-name)
  expect_error(nested %>% left_join(df1, by = "id"), NA)
  expect_error(nested %>% right_join(df1, by = "id"), NA)
  expect_error(nested %>% inner_join(df1, by = "id"), NA)
  expect_error(nested %>% full_join(df1, by = "id"), NA)
  expect_error(nested %>% anti_join(df1, by = "id"), NA)
  df2 <- face_temporal(climate_mel) %>% as_tibble() %>% select(id, date, prcp)
  long <- face_temporal(climate_mel)
  long[,"prcp"] <- NULL
  expect_error(long %>% left_join(df2, by = c("id", "date")), NA)
  expect_error(long %>% right_join(df2, by = c("id", "date")), NA)
  expect_error(long %>% inner_join(df2, by = c("id", "date")), NA)
  expect_error(long %>% full_join(df2, by = c("id", "date")), NA)
  expect_error(long %>% anti_join(df2, by = c("id", "date")), NA)

  # bind_rows - dplyr_reconstruct, bind_rows.temporal_cubble_df
  df1 <- climate_mel %>% head(1)
  df2 <- climate_mel %>% tail(2)
  expect_error(bind_rows(df1, df2), NA)
  df1 <- climate_mel %>% face_temporal() %>% head(10)
  df2 <- climate_mel %>% face_temporal() %>% tail(20)
  expect_error(bind_rows(df1, df2), NA)

  # relocate - dplyr_col_select, dplyr_col_select
  expect_error(cb_nested %>% relocate(ts, .before = name), NA)
  expect_error(cb_nested %>% face_temporal() %>% relocate(tmin), NA)

  # slice - all the slice_* uses dplyr::slice(), which uses dplyr_row_slice()
  expect_error(cb_nested %>% slice_head(n = 2), NA)
  expect_error(cb_nested %>% slice_tail(n = 2), NA)
  expect_error(cb_nested %>% slice_max(elev), NA)
  expect_error(cb_nested %>% slice_min(elev), NA)
  expect_error(cb_nested %>% slice_sample(n = 2), NA)

  # rowwise - rowwise.spatial_cubble_df, rowwise_temporal_cuble_df
  expect_error(cb_nested %>% rowwise(), NA)
  expect_error(cb_long %>% rowwise(), NA)
  expect_error(a %>% rowwise(), NA)
  expect_error(b %>% rowwise(), NA)
})









