cb <- climate_aus %>% head(5)

test_that("selecting on single variable to calculate missingness", {
   res <- cb %>% add_missing_prct(prcp)
   expect_equal(res$prcp_missing, map_dbl(cb$ts, ~mean(is.na(.x$prcp))))
})


test_that("selecting multiple variables using `a:b` to calculate missingness", {
  res <- cb %>% add_missing_prct(prcp:tmin)
  expect_equal(ncol(res), ncol(cb) + 3)
  expect_equal(res$prcp_missing, map_dbl(cb$ts, ~mean(is.na(.x$prcp))))
  expect_equal(res$tmin_missing, map_dbl(cb$ts, ~mean(is.na(.x$tmin))))
  expect_equal(res$tmax_missing, map_dbl(cb$ts, ~mean(is.na(.x$tmax))))
})

test_that("selecting multiple variables using `c()` to calculate missingness", {
  res <- cb %>% add_missing_prct(c(prcp, tmax))
  expect_equal(ncol(res), ncol(cb) + 2)
  expect_equal(res$prcp_missing, map_dbl(cb$ts, ~mean(is.na(.x$prcp))))
  expect_equal(res$tmax_missing, map_dbl(cb$ts, ~mean(is.na(.x$tmax))))
})

