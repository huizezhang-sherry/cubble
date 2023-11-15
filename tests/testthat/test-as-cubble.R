test_that("as_cubble() works", {

  expect_snapshot(
    climate_flat |>
      as.data.frame() |>
      as_cubble(key = id, index = date, coords = c(long, lat))
    )

  expect_snapshot(
    climate_flat |>
      as_cubble(key = id, index = date, coords = c(long, lat))
    )

  # only need `coords` if created from a tsibble
  dt <- climate_flat |>  tsibble::as_tsibble(key = id, index = date)
  expect_snapshot(dt |>  as_cubble(coords = c(long, lat)))

  # only key and index if created from a sf
  dt <- climate_flat |>  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84"))
  expect_snapshot(dt |>  as_cubble(key = id, index = date))

  # stars
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  x <-  stars::read_stars(tif)
  expect_snapshot(x |> as_cubble())


  # netcdf
  path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
  raw <- ncdf4::nc_open(path)
  expect_snapshot(as_cubble(raw))
  # subset degree
  expect_snapshot(
    dt <- as_cubble(raw,vars = c("q", "z"),
                  long_range = seq(113, 153, 3),
                  lat_range = seq(-53, -12, 3))
  )

  # don't have to supply coords if create from a sftime
  dt <- climate_flat |> 
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84")) |> 
    sftime::st_as_sftime()
  expect_snapshot(dt |> as_cubble(key = id, index = date))
})
