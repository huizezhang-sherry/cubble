test_that("multiplication works", {
  colnames(lga) <- c("lga", "geometry")
  check_res <- check_key(stations, meteo)
  class(check_res) <- "list"
  expect_snapshot(
    make_cubble(spatial = lga, temporal = covid, potential_match = check_res),
    error = TRUE
    )

  expect_snapshot(
    make_cubble(lga, covid, potential_match = check_res, key_use = "aaa"),
    error = TRUE
  )
})
