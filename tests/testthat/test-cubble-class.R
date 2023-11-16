test_that("creating cubble works", {
  cubble(
    id = rep(c("perth", "melbourne", "sydney"), each = 3),
    date = rep(as.Date("2020-01-01") + 0:2, times = 3),
    long = rep(c(115.86, 144.96, 151.21), each = 3),
    lat = rep(c(-31.95, -37.81, -33.87), each = 3),
    value = rnorm(n = 9),
    key = id, index = date, coords = c(long, lat)
  )

  # stations and climate are in-built data in cubble
  expect_snapshot(
    make_cubble(spatial = stations, temporal = meteo,
                key = id, index = date, coords = c(long, lat))
  )

  expect_snapshot(key(climate_mel))
})
