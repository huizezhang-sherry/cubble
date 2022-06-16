test_that("construct_match_ll works", {
  # work with cubble object
  dt <- construct_match_ll(tmax_hist)
  expect_equal(nrow(dt), nrow(tmax_hist))
  expect_equal(ncol(dt), 3)
  expect_equal(colnames(dt), c("key1", "long1", "lat1"))

  # work with a tibble object and on minor
  acorn_stations <- tibble::tribble(
    ~number,                          ~name,    ~lat,    ~long,   ~elev,   ~first_year,
    23000L,                   "Adelaide", -34.93, 138.58,   29L, 1910L,
    9999L,                     "Albany", -34.94, 117.82,   68L, 1910L,
    15590L,              "Alice Springs",  -23.8, 133.89,  546L, 1910L,
    40004L,                   "Amberley", -27.63, 152.71,   24L, 1941L,
    36007L,                 "Barcaldine", -23.55, 145.29,  267L, 1962L,
  )
  dt <- construct_match_ll(acorn_stations, data_ll = c("long", "lat"), major = FALSE)
  expect_equal(nrow(dt), nrow(acorn_stations))
  expect_equal(ncol(dt), 3)
  expect_equal(colnames(dt), c("key2", "long2", "lat2"))


})


matched <- match_spatial2(tmax_hist, acorn_stations, minor_ll = c("long", "lat"), spatial_dist_max = 400)
