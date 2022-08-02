library(dplyr)
cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat))

# temporarily mute arrange check
# test_that("nested: arrange", {
#   out <- cb %>%  arrange(lat)
#
#   expect_equal(all(sort(out$lat) == out$lat), TRUE)
#   expect_equal(nrow(out) == nrow(cb), TRUE)
#   expect_equal(ncol(out) == ncol(cb), TRUE)
#
# })

test_that("nested: filter", {
  out <- cb %>%  filter(long > 120)

  expect_equal(nrow(out) == sum(cb$long > 120), TRUE)
  expect_equal(ncol(out) == ncol(cb), TRUE)

})


test_that("nested: slice", {
  out <- cb %>%  slice_head(n = 3)
  expect_equal(nrow(out) == 3, TRUE)

  out <- cb %>%  slice_max(elev, n = 3)
  expect_equal(nrow(out) == 3, TRUE)

  out <- cb %>%  slice_min(elev, n = 3)
  expect_equal(nrow(out) == 3, TRUE)

  out <- cb %>%  slice_sample(n = 3)
  expect_equal(nrow(out) == 3, TRUE)

  out <- cb %>%  slice_tail(n = 3)
  expect_equal(nrow(out) == 3, TRUE)

})

test_that("nested: semi_join", {

})

test_that("nested: anti_join", {

})

test_that("nested: mutate", {

  out <- cb %>%  mutate(elev_km = elev/1000)
  expect_equal(is_cubble(out), TRUE)
  expect_equal(ncol(out), ncol(cb) + 1)
})


test_that("nested: transmute", {

  out <- cb %>%  transmute(elev_km = elev/1000)
  expect_equal(is_cubble(out), FALSE) # no long a cubble
})


test_that("nested: summarise", {

})

test_that("nested: select", {

  vars <- c("id", "lat", "long", "elev", "ts")
  out <- cb %>%  select(all_of(vars))
  expect_equal(ncol(out), length(vars))

  # select for only incomplete attributes
})


test_that("nested: rename", {

  out <- cb %>%  rename(elevation = elev)
  expect_equal("elevation" %in% names(out), TRUE)
  expect_equal(nrow(out), nrow(cb))
  expect_equal(ncol(out), ncol(cb))
})



test_that("nested: relocate", {

  out <- cb %>%  relocate(elev, .before = id)
  expect_equal(is_cubble(out), TRUE)
})

test_that("nested: distinct", {

})
