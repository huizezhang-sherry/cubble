#' A quick plot of sites on an underlying map
#'
#' \code{plot_map} allows you to quickly create a ggplot with your base map
#' and cubble object, with some default aesthetic and theme settings.
#'
#' It should generally be used to quickly create some prototype maps. To make
#' further modification on the map, set \code{print_code = TRUE}. This will print
#' the code in the console as well as write it into the clipboard (so you can
#' directly paste it into your script).
#'
#' @param map_data the dataset contains the map object, an sf object
#' @param point_data a cubble object to plot the site
#' @param print_code whether to print out the ggplot2 code, default to FALSE
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
#' @importFrom clipr clipr_available write_clip
#' @importFrom styler style_text
#' @importFrom whisker whisker.render
#' @return a ggplot object
plot_map <- function(map_data, point_data, print_code = FALSE){

  is_sf <- inherits(map_data, "sf")
  map <- deparse(substitute(map_data))
  point <- deparse(substitute(point_data))
  if (!is_sf) cli::cli_abort("Require an sf object as the base map")
  x <- coord_x(point_data)
  y <- coord_y(point_data)

  if (print_code){
    cli::cli_inform("Press {.kbd Ctrl/Cmd + V} to directly paste the code from the clipboard")
    codes <- whisker::whisker.render(plot_string)
    clip(codes)
  }

  make_plot(map_data, point_data, x, y)
}


make_plot <- function(map_data, point_data, long, lat){

  long <- sym(eval(long))
  lat <- sym(eval(lat))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_data,
                     ggplot2::aes(geometry = .data$geometry),
                     color = "grey", linetype = "dotted") +
    ggplot2::geom_point(data = point_data,
                        ggplot2::aes(x =!!long , y = !!lat)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

}

plot_string <- 'ggplot() +
    geom_sf(data = {{{map}}}, aes(geometry = geometry),
            color = "grey", linetype = "dotted") +
    geom_point(data = {{{point}}}, aes(x = {{x}}, y = {{y}})) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Longitude", y = "Latitude")'


clip <- function(obj){

  obj <- styler::style_text(obj, scope = "tokens")
  available <- clipr::clipr_available()
  if (available) clipr::write_clip(content = obj)
}
