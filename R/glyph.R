#' Function to create a glyph dataset and plot with maps
#' @param data for `glyph_pixel`, a data object; for `glyph_map`,
#' a data of class "glyphplot", created by `glyph_pixel()` or `GGally::glyph()`
#' @param type either a static or interactive plot
#' @param x_major the main x axis
#' @param y_major the main y axis
#' @param var the variable that contains all the variable names, in the long form
#' @param gvar the grouping variable
#' @param width the width adjustment
#' @examples
#' # create a tibble object
#' climate_2020 <- oz_climate %>%
#'                   dplyr::filter(lubridate::year(date) == 2020)
#'
#' # add missing summaries
#' out <- climate_2020 %>%
#'          global(station) %>%
#'          add_missing_prct(prcp:tmin) %>%
#'          add_missing_dscrb() %>%
#'          tidyr::pivot_longer(cols = c(ends_with("missing"), ends_with("dscrb")),
#'                              names_to = c("var", ".value"), names_pattern ="(.*)_(.*)")
#'
#' # create a glyphplot dataset with `glyph_pixel`
#' glyph_df <- glyph_pixel(out,
#'                         x_major = long,
#'                         y_major = lat,
#'                         var = "var",
#'                         gvar = "station")
#'
#' # create the glyph plot
#' quality_map(glyph_df, type = "ggplot")
#' quality_map(glyph_df, type = "interactive")
#' @references `glyph_pixel` is inspired by the `glyph` function in the package GGally:
#' https://github.com/ggobi/ggally/blob/master/R/gglyph.R
#' @rdname glyph
#' @export
glyph_pixel <- function(data, x_major, y_major, var, gvar, width = ggplot2::rel(0.7)){
  var_to_complete <- syms(c(gvar, var))
  x_major <- enquo(x_major)
  y_major <- enquo(y_major)
  data <- data %>% tibble::as_tibble()

  # display 3 pixels per row
  out <- data %>%
    tidyr::complete(!!!var_to_complete) %>%
    dplyr::group_by(!!sym(gvar)) %>%
    dplyr::mutate(minor_id = seq_len(dplyr::n()),
           gid = interaction(!!x_major, !!y_major),
           gx = !!x_major + rescale11((.data$minor_id-1) %% 3) * width / 2,
           gy = !!y_major + (.data$minor_id-1) %/% 3) %>%
    dplyr::ungroup()

  class(out) <- c("glyphplot", class(out))
  out
}

rescale11 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  2 * ((x - rng[1])/(rng[2] = rng[1])) - 1
}

#' @rdname glyph
#' @export
quality_map <- function(data, type = c("ggplot", "interactive")){
 if(!"glyphplot" %in% class(data)) abort("First create a glyph data use `glyph_pixel()` or `GGally::glyph()`")

  aus_map <- ggmap::get_map(location = bbox(data), source = "osm")

  p <- ggmap::ggmap(aus_map) +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = .data$gx, y = .data$gy,
                                     group = .data$gid, color = .data$dscrb,
                                     label = missing, text = glue::glue("var: {var}"))) +
    ggplot2::scale_color_brewer(palette = "Dark2")

  if (type == "ggplot"){
    p + ggplot2::theme(legend.key = ggplot2::element_rect(size = 1)) +
      ggplot2::geom_point(data = data, ggplot2::aes(x = 0, y = 0, fill = .data$var), alpha = 0) +
      ggplot2::guides(fill = ggplot2::guide_legend(label.position = "top",
                                 direction = "horizontal",
                                 title = "order",
                                 title.position = "left",
                                 title.vjust = 0.25,
                                 override.aes = list(alpha=1)))
  } else if (type == "interactive"){
    plotly::ggplotly(p, tooltip = c("label", "text"))

  } else{
    abort("`type` needs to eiter ggplot or interactive")
  }

}
