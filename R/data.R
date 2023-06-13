#' Australia climate data
#'
#' Daily measure on precipitation (\code{prcp}), maximum temperature (\code{tmax}), and
#' minimum temperature (\code{tmin}) in 2020 for 639 stations.
#' @details
#' \describe{
#'   \item{id}{station ID, "ASN000" are international paddings, the next two digits
#'   (digit 8-9) indicates the states the station is in: Western Australia: 01-13,
#'   Northern Territory: 14-15, South Australia: 16-26, Queensland: 27-45,
#'    New South Wales: 46-75, Victoria: 76-90, Tasmania: 91-99.
#'    See http://www.bom.gov.au/climate/cdo/about/site-num.shtml}
#'   \item{lat}{latitude of the stations, in degree}
#'   \item{long}{longitude of the stations, in degree}
#'   \item{elev}{elevation of the stations}
#'   \item{name}{station name}
#'   \item{wmo_id}{the world meteorological organisation (WMO) station number}
#'   \item{ts}{a list-column that nests all the temporal variables: date, prcp, tmax, and tmin}
#' }
#' @examples
#' climate_aus %>% face_temporal() %>% face_spatial()
"climate_aus"

#' Toy climate data
#'
#' Daily measure (2020-01-01 to 2020-01-10) on precipitation (prcp),
#' maximum temperature (tmax), and minimum temperature (tmin)
#' for 3 melbourne airport stations. \code{stations} is the spatial component,
#' (\code{stations_sf} as an sf object), \code{meteo} has the temporal component
#' (\code{meteo_ts} as a tsibble object), \code{climate_flat} has both
#' in a single joined table, and \code{climate_mel} is the cubble object.
#' See \code{climate_aus} on the full dataset.
#' @seealso climate_aus
#' @rdname test-data
#' @examples
#' cb <- make_cubble(
#'   spatial = stations, temporal = meteo,
#'   key = id, index = date, coords = c(long, lat)
#' )
#' identical(cb, climate_mel)
#' cb2 <- climate_flat %>% as_cubble(key = id, index = date, coords = c(long, lat))
#' identical(cb, climate_mel)
"stations"

#' @rdname test-data
"stations_sf"

#' @rdname test-data
"meteo"

#' @rdname test-data
"meteo_ts"

#' @rdname test-data
"climate_flat"

#' @rdname test-data
"climate_mel"

#' Australia river data
#' @examples
#' river
"river"



#' Daily COVID count data (in `tsibble`) and Victoria LGA (in `sf`)
#'
#' Daily COVID count data (\code{covid}) from 2022-01-01 to 2020-03-23 in a
#' tsibble object (\code{date}, \code{lga}, \code{n}, and \code{avg_7day}).
#' Victoria Local Government Area (LGA) spatial geometry in an sf object
#' (\code{lga_name_2018} and \code{geometry})
#'
#'
#'
#' \describe{
#'   \item{date}{date object, from 2022-01-01 to 2020-03-23}
#'   \item{lga}{Victoria Local Government Area (LGA) in Australia}
#'   \item{n}{COVID-19 case count}
#'   \item{avg_7day}{rolling mean of \code{n} in a 7 day window. Calculate with \code{mutate(avg_7day = slider::slide_dbl(n, mean, .before = 6))}}
#'   \item{lga_name_2018}{LGA encoding by Australia Bureau of Statistics,
#'   slightly differ from the encoding used by the Department of Health in the \code{covid} data}
#'   \item{geometry}{multipolygon geometry of each LGA}
#' }
#' @examples
#' library(sf)
#' library(dplyr)
#' # prompt msg on the key mismatch between the two datasets
#' make_cubble(lga, covid, by = c("lga_name_2018" = "lga"))
#' check_res <- check_key(lga, covid, by = c("lga_name_2018" = "lga"))
#'
#' # fix mismatch
#' lga2 <- lga |>
#'   rename(lga = lga_name_2018) |>
#'   mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
#'          lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga)) |>
#'   filter(!lga %in% check_res$others$spatial)
#' covid2 <- covid |> filter(!lga %in% check_res$others$temporal)
#'
#' make_cubble(spatial = lga2, temporal = covid2)
#' @rdname covid
"covid"

#' @rdname covid
"lga"



