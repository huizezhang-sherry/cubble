#' extract bbox of the coordinate with a buffer
#' @param dt the data object
#' @param lat the latitude column
#' @param long the longitude column
#' @param buffer_ratio the percentage to add/deduct to the max/min lat/long when querying the map
#'
#' @examples
#' aus_map <- get_map(location = bbox(nested), source = "osm")
#' ggmap(aus_map) +
#'   geom_point(data = nested, aes(x = long, y = lat))
#' @importFrom dplyr summarise across ends_with mutate
#' @importFrom tidyr unnest
#' @export
bbox <- function(dt, lat, long, buffer_ratio = 0.01) {

  # check data structure
  # check lat, long present in the data

  if ("rowwise_df" %in% class(dt)) {
    dt <- dt %>% tidyr::unnest()
  }

  out <- dt %>%
    dplyr::summarise(dplyr::across(lat:long, list(min = min, max = max))) %>%
    relocate(long_min, lat_min, long_max, lat_max)

  buffter_unit <- out$long_max * buffer_ratio

  out <- out %>%
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("max"), ~ .x + buffter_unit),
      dplyr::across(dplyr::ends_with("min"), ~ .x - buffter_unit)
    ) %>%
    as_vector()

  names(out) <- NULL

  out
}
