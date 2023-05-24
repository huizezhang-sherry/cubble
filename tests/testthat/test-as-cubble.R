create_cubble <- function(data){
  data %>% as_cubble(key = id, index= date, coords = c(long, lat))
}

test_that("different accepted index classes", {

  # date, POSIXct, POSIXlt
  expect_error(climate_flat %>% create_cubble(), NA)
  expect_error(climate_flat %>% mutate(date = as.POSIXct(date)) %>% create_cubble(), NA)
  expect_error(climate_flat %>% mutate(date = as.POSIXlt(date)) %>% create_cubble(), NA)

  # yearmonth, yearweek, yearquarter
  expect_error(climate_flat %>% mutate(date = tsibble::yearmonth(date)) %>% create_cubble(), NA)
  expect_error(climate_flat %>% mutate(date = tsibble::yearweek(date)) %>% create_cubble(), NA)
  expect_error(climate_flat %>% mutate(date = tsibble::yearquarter(date)) %>% create_cubble(), NA)

})
