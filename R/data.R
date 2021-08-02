#' Australian climate data in 2020
#' @format A tsibble with 87,639 rows and 13 columns
#' \describe{
#'   \item{date}{date of the record}
#'   \item{station}{weather stations recorded. See \code{station} dataset for metadata of each station}
#'   \item{prcp:tavg}{variables recorded in each station. See \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} for detailed data description)}
#' }
#' @examples
#' library(tidyr)
#' climate %>% pivot_longer(prcp:tavg, names_to = "param", values_to = "value")
"climate"

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
"station"

#' A nested data
#' @format A rowwise dataframe iwth 1,451 rows adn 7 columns
#' \describe{
#'   \item{station}{station id}
#'   \item{data}{a list-column that nests all the time-wise measures}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation station number, if has one}
#' }
"nested"

#' oz_climate
#' @format A rowwise dataframe with 1,451 rows and 7 columns
#' \describe{
#'   \item{station}{station id}
#'   \item{date}{date}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{prcp}{precipitation}
#'   \item{tmax}{max temperature}
#'   \item{tmin}{min temperature}
#' }
"oz_climate"

#' climate_small
#' @format a climate dataset with 55 stations recorded across Australia
#' \describe{
#'   \item{station}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{ts}{a list-column with variables `date`, `prcp`, `tmax` and `tmin` nested}
#' }
"climate_small"

