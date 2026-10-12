# Sydney beach water quality, used in the glyph map examples vignette.
# Data: NSW Beachwatch water quality and Sydney rainfall, TidyTuesday 2025-05-20
# (https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-05-20).
# For each swim site: the share of samples above 40 enterococci / 100 mL (the
# NHMRC guideline) on days 0-6 after a day with >= 20 mm of rain, 2015 onwards.
library(dplyr)
library(lubridate)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/"
water <- readr::read_csv(paste0(url, "water_quality.csv")) |>
  arrange(date) |> filter(year(date) >= 2015)
weather <- readr::read_csv(paste0(url, "weather.csv")) |>
  arrange(date) |> filter(year(date) >= 2015)

# days since the last heavy-rain day
weather <- weather |>
  mutate(last_rain = if_else(precipitation_mm >= 20, date, as.Date(NA))) |>
  tidyr::fill(last_rain) |>
  mutate(days_after_rain = as.integer(date - last_rain)) |>
  select(-last_rain)

beach <- water |>
  filter(!is.na(enterococci_cfu_100ml), region != "Western Sydney") |>
  left_join(select(weather, date, days_after_rain), by = "date") |>
  filter(!is.na(days_after_rain), days_after_rain <= 6) |>
  group_by(swim_site, region, long = longitude, lat = latitude,
           days_after_rain) |>
  summarise(unsafe = mean(enterococci_cfu_100ml > 40), n = n(),
            .groups = "drop")

# central Sydney; harbour sites >= 7 km from the Heads count as inner harbour
beach <- beach |>
  filter(lat > -33.99, lat < -33.74, long > 151.1) |>
  mutate(
    km_heads = sqrt(((long - 151.287) * 92.6)^2 + ((lat + 33.834) * 111)^2),
    water = ifelse(region == "Sydney Harbour" & km_heads >= 7,
                   "Inner harbour", "Ocean & outer harbour")
  ) |>
  select(-km_heads)

# keep sites whose glyph boxes do not overlap: inner harbour sites first,
# then the best-sampled sites
gw <- 0.028; gh <- 0.012 # glyph size (degrees), as in the vignette
sites <- beach |>
  group_by(swim_site, water, long, lat) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(desc(water == "Inner harbour"), desc(n))
keep <- integer(0)
for (i in seq_len(nrow(sites))) {
  clear <- abs(sites$long[i] - sites$long[keep]) > gw |
    abs(sites$lat[i] - sites$lat[keep]) > gh * 1.05
  if (all(clear)) keep <- c(keep, i)
}
beach <- beach |>
  filter(swim_site %in% sites$swim_site[keep]) |>
  mutate(unsafe = round(unsafe, 4))
readr::write_csv(beach, "inst/extdata/sydney-beach.csv")

# land around Sydney from council boundaries, which gives a coastline that
# includes Sydney Harbour
sf::sf_use_s2(FALSE)
sydney <- ozmaps::abs_lga |>
  sf::st_union() |>
  sf::st_crop(xmin = 151.1, xmax = 151.35, ymin = -34.02, ymax = -33.7) |>
  sf::st_simplify(dTolerance = 0.0002) |>
  sf::st_as_sf()
sf::sf_use_s2(TRUE)
sf::st_write(sydney, "inst/extdata/sydney-land.geojson",
             delete_dsn = TRUE, quiet = TRUE)
