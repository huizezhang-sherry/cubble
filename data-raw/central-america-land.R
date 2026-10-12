# Land outline over the GGally::nasa grid (Central America), used in the
# glyph map vignette. Natural Earth 1:110m, no country borders, small islands
# dropped. Saved as a file so the vignette doesn't depend on rnaturalearth.
library(dplyr)

lim_x <- range(GGally::nasa$long) + c(-1.3, 1.3)
lim_y <- range(GGally::nasa$lat) + c(-1.3, 1.3)

sf::sf_use_s2(FALSE)
land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf") |>
  sf::st_union() |>
  sf::st_crop(xmin = lim_x[1] - 5, xmax = lim_x[2] + 5,
              ymin = lim_y[1] - 5, ymax = lim_y[2] + 5) |>
  sf::st_cast("POLYGON") |>
  sf::st_as_sf()
land <- land[as.numeric(sf::st_area(land)) > 5e10, ]
sf::sf_use_s2(TRUE)

sf::st_write(land, "inst/extdata/central-america-land.geojson",
             delete_dsn = TRUE, quiet = TRUE)
