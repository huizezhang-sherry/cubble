test_that("dplyr verbs work", {
  library(dplyr)
  cb_nested <- climate_mel
  cb_long <- face_temporal(climate_mel)

  # filter - currently filter.spatial_cubble_df, dply_row_slice
  expect_snapshot(cb_nested |> filter(elev > 40))
  expect_snapshot(cb_long |> filter(prcp > 0))

  # mutate - curerntly mutate.spatial_cubble_df, dply_col_modify
  expect_snapshot(cb_nested |> mutate(elev2 = elev + 10))
  expect_snapshot(cb_long |> mutate(prcp2 = prcp + 10))

  # arrange - currently arrange.spatial_cubble_df, arrange.temporal_cubble_df
  expect_snapshot(cb_nested |> arrange(wmo_id))
  expect_snapshot(cb_long |> arrange(prcp))

  # summarise - summarise.spatial_cubble_df,  summarise.temporal_cubble_df
  expect_snapshot(
    cb_long |> 
      group_by(first_5 = ifelse(lubridate::day(date) <=5, 1, 2 )) |> 
      summarise(tmax = mean(tmax))
  )
  expect_snapshot(
    cb_long |> 
      mutate(first_5 = ifelse(lubridate::day(date) <=5, 1, 2)) |> 
      summarise(t = mean(tmax), .by = first_5)
    )

  # select -  select.spatial_cubble_df,  select.temporal_cubble_df
  expect_snapshot(cb_nested |> select(name))
  expect_snapshot(cb_nested |> select(-id, -name))
  expect_snapshot(cb_long |> select(prcp))
  expect_snapshot(cb_long |> select(-prcp, -date))

  # rename - rename.spatial_cubble_df, rename.temporal_cubble_df
  expect_snapshot(cb_nested |> rename(elev2 = elev))
  expect_snapshot(cb_long |> rename(prcp2 = prcp))
  # rename on key attributes)
  expect_snapshot(cb_nested |> rename(id2 = id))
  expect_snapshot(cb_long |> rename(date2 = date))

  # join - mutate_join - dplyr_reconstruct()
  # join - filter_join - dplyr_row_slice()
  df1 <- cb_nested |> as_tibble() |> select(id, name) |> head(2)
  nested <- cb_nested |> select(-name)
  expect_snapshot(nested |> left_join(df1, by = "id"))
  expect_snapshot(nested |> right_join(df1, by = "id"))
  expect_snapshot(nested |> inner_join(df1, by = "id"))
  expect_snapshot(nested |> full_join(df1, by = "id"))
  expect_snapshot(nested |> anti_join(df1, by = "id"))

  # bind_rows - dplyr_reconstruct, bind_rows.temporal_cubble_df
  df1 <- cb_nested |> head(1)
  df2 <- cb_nested |> tail(2)
  expect_snapshot(bind_rows(df1, df2))
  df1 <- cb_long |> head(10)
  df2 <- cb_long |> tail(20)
  expect_snapshot(bind_rows(df1, df2))

  # relocate - dplyr_col_select, dplyr_col_select
  expect_snapshot(cb_nested |> relocate(ts, .before = name))
  expect_snapshot(cb_nested |> face_temporal() |> relocate(tmin))

  # slice - all the slice_* uses dplyr::slice(), which uses dplyr_row_slice()
  expect_snapshot(cb_nested |> slice_head(n = 2))
  expect_snapshot(cb_nested |> slice_tail(n = 2))
  expect_snapshot(cb_nested |> slice_max(elev))
  expect_snapshot(cb_nested |> slice_min(elev))
  #set.seed(123)
  #expect_snapshot(cb_nested |> slice_sample(n = 2))

  # rowwise - rowwise.spatial_cubble_df, rowwise.temporal_cuble_df
  expect_snapshot(cb_nested |> rowwise())
  expect_snapshot(cb_long |> rowwise())

  # group_by & ungroup -
  expect_snapshot((res <- cb_nested |> mutate(group1 = c(1, 1, 2)) |> group_by(group1)))
  expect_snapshot(res |> ungroup())
  expect_snapshot((res2 <- res |> face_temporal() |> unfold(group1) |> group_by(group1)))
  expect_snapshot(res2 |> ungroup())
  res2 |> mutate(first_5 = ifelse(lubridate::day(date) <= 5, 1, 6)) |> 
    group_by(first_5) |> 
    ungroup(group1)
})
