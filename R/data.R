#' Australian weather stations
#' @format A tibble with 1,451 rows and 6 columns
#' \describe{
#'   \item{id}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation station number, if has one}
#' }
#' @seealso climate_large
"station"

#' Australia climate data (large)
#'
#' One year worth (2020) of climate data recorded  in 478 stations across Australia.
#' The data is computed from National Oceanic and Atmospheric Administration (NOAA) using the `rnoaa` package.
#'
#' @format A cubble object with 5 spatio-related variables and 11 time-related variables, nested in the `ts` column
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation station number, if has one}
#'   \item{ts}{a list-column that nests all the time-wise measures:
#'  date, prcp, tmax, tmin, dapr, datn, datx, dwpr, mdpr, mdtn, mdtx, tavg}
#' }
#' @seealso station
"climate_large"

#' Australia climate data (minimal)
#'
#' A minimal 2020 climate dataset recorded in 2 stations in a tibble format. This data is used to
#' show how to create a cubble object from tibble.
#' @format A tibble object with 732 rows and 9 columns
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{prcp}{precipitation}
#'   \item{tmax}{maximum temperature}
#'   \item{tmin}{minimum temperature}
#' }
#' @seealso climate_small
"climate_flat"

#' Australia climate data (small)
#'
#' Climate data recorded in 55 stations from 2015 - 2020 in Australia.
#' @format A cubble object with 4 spatio-related variables and 3 time-related variables nested in the `ts` column
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{ts}{a list-column with variables `date`, `prcp`, `tmax` and `tmin` nested}
#' }
#' @seealso climate_flat
"climate_small"


#' Australia climate data (dirty)
#'
#' @format A tibble object with 2,069,634 rows and 9 columns
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{prcp}{precipitation}
#'   \item{tmax}{maximum temperature}
#'   \item{tmin}{minimum temperature}
#' }
#' @seealso climate_small
"oz_climate"
