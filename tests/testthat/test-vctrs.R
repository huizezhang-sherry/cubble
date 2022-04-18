test_that("combine two cubbles", {
  cb1 <- climate_flat %>%  as_cubble(key = id, index = date, coords = c(long, lat))
  cb2 <- climate_aus[6:10, ]
  out <- dplyr::bind_rows(cb1, cb2)

  expect_equal(is_cubble(out), TRUE)
  expect_equal(nrow(out), nrow(cb1) + nrow(cb2))
})


test_that("combine a cubble and a tibble",{
  cb <- climate_flat %>%  as_cubble(key = id, index = date, coords = c(long, lat))
  tb <- climate_aus[6:10,] %>%  as_tibble()
  out <- dplyr::bind_rows(cb, tb)

  expect_equal(is_cubble(out), TRUE)
  expect_equal(nrow(out), nrow(cb) + nrow(tb))
})

test_that("bind columns of a cubble and a tibble",{
  cb <- climate_flat %>% 
    as_cubble(key = id, index = date, coords = c(long, lat))
  elev <- cb$elev
  cb$elev <- NULL
  out <- dplyr::bind_cols(cb, tibble::tibble(elev = elev))

  expect_equal(is_cubble(out), TRUE)
  expect_equal(nrow(out), 5)
  expect_equal(ncol(out), ncol(cb) + 1)
})
