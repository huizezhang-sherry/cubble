#' Australia climate data
#'
#' Daily measure on precipitation (prcp) maximum temperature (tmax), and
#' minimum temperature (tmin) in 2020 for 639 stations. \code{stations} and
#' \code{climate} are the separate spatial and temporal objects while
#' \code{climate_aus} is the combined cubble object.
#' @details
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
#' library(ggplot2)
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = state_map,
#'                    ggplot2::aes(geometry = .data$geometry),
#'                    color = "grey", linetype = "dotted") +
#'   ggplot2::geom_point(data = climate_aus,
#'                       ggplot2::aes(x = long, y = lat)) +
#'   ggplot2::theme_bw()
#' @seealso climate_subset climate_flat
#' @rdname climate_aus
"climate_aus"

#'@rdname climate_aus
"stations"

#'@rdname climate_aus
"climate"


#' Australia climate data
#'
#' Daily measure on precipitation (prcp) maximum temperature (tmax), and
#' minimum temperature (tmin) in 2020 for 30 stations.
#' @format A cubble object
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
#' library(ggplot2)
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = state_map,
#'                    ggplot2::aes(geometry = .data$geometry),
#'                    color = "grey", linetype = "dotted") +
#'   ggplot2::geom_point(data = climate_subset,
#'                       ggplot2::aes(x = long, y = lat)) +
#'   ggplot2::theme_bw()
#' @seealso climate_aus climate_flat
"climate_subset"


#' Australia climate data
#' Daily measure on precipitation (prcp) maximum temperature (tmax), and
#' minimum temperature (tmin) in 2020 for 5 stations.
#'
#' @format A tibble object with 155 rows and 10 columns
#' \describe{
#'   \item{id}{station id}
#'   \item{lat}{latitude of the station}
#'   \item{long}{longitude of the station}
#'   \item{elev}{elevation of the station}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meterological organisation (WMO) station number}
#'   \item{date}{the date that prcp, tmax, and tmin recorded}
#'   \item{prcp}{precipitation}
#'   \item{tmax}{maximum temperature}
#'   \item{tmin}{minimum temperature}
#' }
#' @seealso climate_aus climate_subset
#' @examples
#' library(ggplot2)
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' ggplot2::ggplot() +
#'  ggplot2::geom_sf(data = state_map,
#'                   ggplot2::aes(geometry = .data$geometry),
#'                   color = "grey", linetype = "dotted") +
#'    ggplot2::geom_point(data = climate_aus,
#'                        ggplot2::aes(x = long, y = lat)) +
#'    ggplot2::theme_bw()
"climate_flat"

