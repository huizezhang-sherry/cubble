test_that("check_key works", {
  expect_snapshot(check_key(stations, meteo))

  # make_cubble() will prompt to use check_key if there are key mis-match:
  colnames(lga) <- c("lga", "geometry")
  suppressMessages(cb <- make_cubble(spatial = lga, temporal = covid))
  expect_snapshot((check_res <- check_key(lga, covid)))
  expect_snapshot(
    make_cubble(spatial = lga, temporal = covid, potential_match = check_res)
  )


})
