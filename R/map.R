#' A quick plot of map
#'
#' A quick initiate of a ggplot2 object for map with some default aesthetic setting
#' to save some typing for create a map.
#'
#' @param map_data the dataset contains the map to be plotted
#' @param ... other arguments to be passed into `geom_sf()`
#'
#' @examples
#' library(rmapshaper)
#' library(ggplot2)
#' library(dplyr)
#'
#' vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")
#' plot_map(vic_map)
#'
#' # Victoria stations
#' victoria <- aus_climate %>% filter(stringr::str_detect(id, "ASN0008"))
#' plot_map(vic_map) +
#'    geom_point(data = victoria, aes(x = long, y = lat))
#' @export
plot_map <- function(map_data, ...){
  if ("geometry" %in% names(map_data)){
    ggplot2::ggplot()  +
      ggplot2::geom_sf(
        data = map_data,
        ggplot2::aes(geometry = .data$geometry),
        fill = "transparent", color = "grey", linetype = "dotted", ...) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Longitude", y = "Latitude")
  } else{
    cli::cli_abort("{.fn map_data} needs to have a column called {.field geometry}")
  }

# -------------------------------------------------------------------------


}
