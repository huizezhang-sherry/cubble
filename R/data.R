#' Australian climate data in 2020
#' @format A tibble with 87,639 rows and 13 columns
#' \describe{
#'   \item{date}{date of the record}
#'   \item{station}{weather stations recorded. See \code{station} dataset for metadata of each station}
#'   \item{prcp:tavg}{variables recorded in each station. See \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} for detailed data description)}
#' }
#' @examples
#' library(tidyr)
#' climate %>% pivot_longer(prcp: tavg, names_to = "param", values_to = "value")
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
