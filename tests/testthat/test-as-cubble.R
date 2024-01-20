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

  expect_snapshot(
    climate_flat |>
      as_cubble(key = id, index = date, coords = c(long, lat), crs = sf::st_crs(4326))
  )

  expect_snapshot(
    climate_flat |>
      nest(data = date:tmin) |>
      as_cubble(key = id, index = date, coords = c(long, lat))
  )

  expect_snapshot(
    climate_flat |>
      nest(data = prcp:tmin) |>
      as_cubble(key = id, index = date, coords = c(long, lat)), error = TRUE
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
  res <- x |>  as_cubble(index = band)
  expect_snapshot(res)
  expect_snapshot(res |> face_temporal())

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

  library(tibble)
  library(sf)
  library(stars)
  # a vector data cube example
  spatial_sf <- tibble(
    id = c(1,2),
    long = c(130, 140),
    lat = c(-38, -44),
    name = c("station 1", "station 2")
  ) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  temporal <- tibble(
    id = c(rep(1, each = 5), rep(2, each = 5)),
    time = c(seq(as.Date("2023-03-21"), as.Date("2023-03-25"), 1),
             seq(as.Date("2023-03-21"), as.Date("2023-03-25"), 1)),
    var1 = c(3, 6, 8, 2, 5, 7, 9, 14, 5, 9)
  )

  # with stars
  station_vec <- st_geometry(spatial_sf)
  time_vec <- unique(temporal$time)
  var_vec <- spatial_sf$name
  d <- st_dimensions(station = station_vec, time = time_vec)
  var1_array <- temporal$var1 |>
    matrix(byrow = TRUE, nrow = 2, ncol = 5) |>
    array(dim = dim(d))
  data_stars <- st_as_stars(list(var1 = var1_array), dimensions = d)
  expect_snapshot(data_stars |> as_cubble(key = id, index= time))


})
