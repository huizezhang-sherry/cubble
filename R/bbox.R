#' Extract bbox of the coordinate with a buffer
#' @param data the data object
#' @param lat the latitude column
#' @param long the longitude column
#' @param buffer_ratio the percentage to add/deduct to the max/min lat/long when querying the map
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggmap)
#' aus_map <- get_map(location = bbox(station), source = "osm")
#' ggmap(aus_map) + geom_point(data = oz_global2, aes(x = long, y = lat))
#' }
#' @importFrom dplyr summarise across mutate
#' @export
bbox <- function(data, lat, long, buffer_ratio = 0.01) {

  # check lat, long poutent in the data

  data <- tibble::as_tibble(data)

  out <- data %>%
    dplyr::summarise(dplyr::across(lat:long, list(min = min, max = max)))

  buffer_unit <- out$long_max * buffer_ratio

  c(out$long_min - buffer_unit,
    out$lat_min - buffer_unit,
    out$long_max + buffer_unit,
    out$lat_max+ buffer_unit
    )

}
