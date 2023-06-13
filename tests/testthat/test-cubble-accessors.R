nested <- climate_mel
nested_sf <- climate_mel %>% make_spatial_sf()
long <- climate_mel %>% face_temporal()
test_that("subset with `[`", {
  expect_equal(class(nested[1:3])[1], "tbl_df")
  expect_equal(class(nested[1])[1], "tbl_df")
  expect_equal(class(nested[c(1:3, 7)])[1], "spatial_cubble_df")
  expect_equal(class(nested_sf[1:3])[1], "sf")

  expect_equal(class(long[3:5])[1], "tbl_df")
  expect_equal(class(long[3])[1], "tbl_df")
  expect_equal(class(long[1:3])[1], "temporal_cubble_df")
})

test_that("renaming", {
  nested2 <- nested
  names(nested2)[1] <- "skdjflks"
  expect_equal(key_vars(nested2), "skdjflks")

  names(nested2)[2:3] <- c("xxx", "ccc")
  expect_equal(coords(nested2), c("xxx", "ccc"))

  long2 <- long
  names(long2)[1] <- "skdjflks"
  expect_equal(key_vars(long2), "skdjflks")
  expect_error(face_spatial(long2), NA)
  expect_equal(names(spatial(long2))[1], "skdjflks")

  names(long2)[2] <- "ttt"
  expect_equal(index_var(long2), "ttt")

})
