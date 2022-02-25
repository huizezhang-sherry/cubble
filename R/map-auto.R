#' A quick plot of map
#'
#' A quick plotting function with some default aesthetic and theme settings for
#' maps. To add more customisation on the plot, set \code{print_code = TRUE},
#' and modify in the relevant places.
#'
#' @param map_data the dataset contains the map object, an sf object
#' @param point_data a cubble object to plot the site
#' @param print_code whether to print out the ggplot2 code
#'
#' @examples
#' library(ggplot2)
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' # a quick plot
#' plot_map(state_map, climate_aus)
#'
#' # print out the ggplot2 code of the map
#' plot_map(state_map, climate_aus, print_code = TRUE)
#' @export
plot_map <- function(map_data, point_data, print_code = FALSE){
  is_sf <- inherits(map_data, "sf")
  c <- deparse(substitute(map_data))
  if (!is_sf) cli::cli_abort("Require an sf object as the base map")
  x <- coord_x(point_data)
  y <- coord_y(point_data)

  if (print_code){
    cli::cli_code(format(fn_body(make_plot)))
  }

  make_plot(map_data, point_data, x, y)
}


make_plot <- function(map_data, point_data, long, lat){

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_data,
                     ggplot2::aes(geometry = .data$geometry),
                     color = "grey", linetype = "dotted") +
    ggplot2::geom_point(data = point_data,
                        ggplot2::aes(x = long, y = lat)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

}
