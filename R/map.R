#' A quick plot of maps
#'
#' A quick initiate of a ggplot2 object for map with some default aesthetic setting
#' to save some typing for create a map.
#'
#' @param map_data the dataset contains the map to be plotted
#' @param ... other arguments to be passed into `geom_sf()`
#' @examples
#' library(rmapshaper)
#' library(ggplot2)
#' state_map <- ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' plot_map(state_map)
#' plot_map(state_map) +
#'    geom_point(data = climate_small, aes(x = long, y = lat))
#' @export
plot_map <- function(map_data, ...){
  if ("geometry" %in% names(map_data)){
    ggplot2::ggplot()  +
      ggplot2::geom_sf(data = map_data,
                       ggplot2::aes(geometry = geometry),
                       color = "grey80", alpha = 0.4, linetype = 3, ...) +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  } else{
    abort("`map_data` needs to have a column called `geometry`")
  }

}
