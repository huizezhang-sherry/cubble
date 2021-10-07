#' Australia climate data
#'
#' One year worth (2020) of climate data recorded  in 72 stations across Australia.
#' The data is computed from National Oceanic and Atmospheric Administration (NOAA) using the `rnoaa` package.
#'
#' @format A cubble object with 5 station-related variables and 3 time-related variables (PRCP, TMAX, and TMIN), nested in the `ts` column
#' \describe{
#'   \item{id}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation (WMO) station number}
#'   \item{ts}{a list-column that nests all the time-wise measures: date, prcp, tmax, and tmin}
#' }
#' @examples
#' library(rmapshaper)
#' library(ggplot2)
#' state_map <- ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' plot_map(state_map) +
#'   geom_point(data = aus_climate, aes(x = long, y = lat))
"aus_climate"

#' Australia climate data (tibble)
#'
#' A minimal 2020 climate dataset recorded in 5 stations in a tibble format. This data is used to
#' show how to create a cubble object from tibble.
#' @format A tibble object with 1830 rows and 10 columns
#' \describe{
#'   \item{id}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{prcp}{precipitation}
#'   \item{tmax}{maximum temperature}
#'   \item{tmin}{minimum temperature}
#' }
"climate_flat"

#' Worldwide climate data
#'
#' The dataset contains weather data from WMO and GSN stations in
#' the United States, Australia, Austria, Spain, France, and China.
#'
#' @format A tibble object with 177 rows and 9 columns
#' \describe{
#'   \item{id}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{name of the station}
#'   \item{wmo_id}{the World Meterological Organisation (WMO) station number}
#'   \item{country}{country name}
#'   \item{continent}{continent name}
#'   \item{ts}{a list-column that nests all the time-wise measures: date, prcp, tmax, and tmin}
#' }
#' @examples
#' \dontrun{
#' library(ggplot2)
#' map <- sf::st_as_sf(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))
#' plot_map(map) +
#'   geom_point(data = world_climate, aes(x = long, y = lat))
#' }
#'
"world_climate"

#' Climate data with missing value
#'
#' @format a cubble object with 50 rows and 7 columns
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation (WMO) station number}
#'   \item{ts}{a list-column that nests all the time-wise measures: date, prcp, tmax, and tmin}
#' }
"climate_missing"
