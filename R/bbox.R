#' Extract bbox of the coordinate with a buffer
#' @param data the data object
#' @param lat the latitude column
#' @param long the longitude column
#' @param buffer_ratio the percentage to add/deduct to the max/min lat/long when querying the map
#' @examples
#' library(ggmap)
#' library(ggplot2)
#' aus_map <- get_map(location = bbox(station), source = "osm")
#' ggmap(aus_map) + geom_point(data = station, aes(x = long, y = lat))
#' @importFrom dplyr summarise across ends_with mutate relocate
#' @importFrom tidyr unnest
#' @export
bbox <- function(data, lat, long, buffer_ratio = 0.01) {

  # check data structure
  # check lat, long present in the data

  if ("rowwise_df" %in% class(data)) {
    data <- data %>% ungroup()
  }

  out <- data %>%
    dplyr::summarise(dplyr::across(lat:long, list(min = min, max = max))) %>%
    dplyr::relocate(.data$long_min, .data$lat_min, .data$long_max, .data$lat_max)

  buffter_unit <- out$long_max * buffer_ratio

  out <- out %>%
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("max"), ~ .x + buffter_unit),
      dplyr::across(dplyr::ends_with("min"), ~ .x - buffter_unit)
    ) %>%
    purrr::as_vector()

  names(out) <- NULL

  out
}
