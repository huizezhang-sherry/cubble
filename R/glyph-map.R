#' Create glyph map with ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param x_major,x_minor,y_major,y_minor The name of the variable (as a
#' string) for the major and minor x and y axes. Together, each unique
#' combination of \code{x_major} and \code{y_major} specifies a grid cell.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param height,width The height and width of each glyph. Defaults to 95% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#'  resolution of the data by using \code{\link[ggplot2]{rel}}.
#' @export
#' @examples
#' library(ggplot2)
#' library(GGally)
#' # basic glyph map with reference line and box---------------
#' ggplot(data = GGally::nasa,
#'        aes(x_major = long, x_minor = day,
#'            y_major = lat, y_minor = surftemp)) +
#'   geom_glyph_box() +
#'   geom_glyph_line() +
#'   geom_glyph() +
#'   theme(aspect.ratio = 1)
#'
#' # with polar coordinate ---------------
#' ggplot() +
#'   geom_glyph(data = GGally::nasa,
#'              aes(x_major = long, x_minor = day,
#'                  y_major = lat, y_minor = surftemp), polar = TRUE) +
#'   theme(aspect.ratio = 1)
#'
#' # adjust width and height with relative & absolute value ---------------
#' ggplot() +
#'   geom_glyph(data = GGally::nasa,
#'              aes(x_major = long, x_minor = day,
#'                  y_major = lat, y_minor = surftemp),
#'                  width = rel(2), height = 3) +
#'   theme(aspect.ratio = 1)
#'
#'
geom_glyph <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., x_major = NULL,
                       x_minor = NULL, y_major = NULL, y_minor = NULL,
                       polar = FALSE, width = ggplot2::rel(1.5),
                       height = ggplot2::rel(1.5),
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      polar = polar,
      width = width,
      height = height,
      ...
    )
  )
}

GeomGlyph <- ggplot2::ggproto(
  "GeomGlyph",
  ggplot2::Geom,
  setup_data = function(data, params) {

    glyph_data_setup(data, params)
  },

  draw_panel = function(data,  panel_params, coord, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...)
  },


  required_aes = c("x_minor", "x_major", "y_minor", "y_major"),
  default_aes = ggplot2::aes(
    colour = "black", size = 0.5, linetype = "solid", alpha = 1,
    polar = FALSE,
    width = ggplot2::rel(1.5),
    height = ggplot2::rel(1.5)
  )
)

#' @export
geom_glyph_line <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., x_major = NULL,
                            x_minor = NULL, y_major = NULL, y_minor = NULL,
                            polar = FALSE, width = ggplot2::rel(1.5),
                            height = ggplot2::rel(1.5),
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlyphLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      polar = polar,
      width = width,
      height = height,
      ...
    )
  )
}

GeomGlyphLine <- ggplot2::ggproto(
  "GeomGlyphLine",
  ggplot2::Geom,
  setup_data = function(data, params) {
    data <- glyph_data_setup(data, params)
    calc_ref_line(data, params)
  },

  draw_panel = function(data,  panel_params, coord, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...)
  },


  required_aes = c("x_minor", "x_major", "y_minor", "y_major"),
  default_aes = ggplot2::aes(
    colour = "grey", size = 1, linetype = "solid", alpha = 1,
    polar = FALSE,
    width = ggplot2::rel(1.5),
    height = ggplot2::rel(1.5),
  )
)

#' @export
geom_glyph_box <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., x_major = NULL,
                           x_minor = NULL, y_major = NULL, y_minor = NULL,
                           polar = FALSE, width = ggplot2::rel(1.5),
                           height = ggplot2::rel(1.5),
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlyphBox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      polar = polar,
      width = width,
      height = height,
      ...
    )
  )
}

GeomGlyphBox <- ggplot2::ggproto(
  "GeomGlyphBox",
  ggplot2::Geom,
  setup_data = function(data, params) {
    data <- glyph_data_setup(data, params)
    calc_ref_box(data, params)
  },

  draw_panel = function(data,  panel_params, coord, ...) {
    ggplot2:::GeomRect$draw_panel(data, panel_params, coord, ...)
  },


  required_aes = c("x_minor", "x_major", "y_minor", "y_major"),
  default_aes = ggplot2::aes(
    colour = "grey", size = 0.5, linetype = "solid", alpha = 1,
    fill = "transparent",
    polar = FALSE,
    width = ggplot2::rel(1.5),
    height = ggplot2::rel(1.5),
  )
)

# Helpers ------------------------------------------
rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale11 <- function(x, xlim = NULL) 2 * rescale01(x, xlim) - 1

glyph_data_setup <- function(data, params){
  if (length(unique(data$group)) == 1){
    data$group <- interaction(data$x_major, data$y_major, drop = TRUE)
  }

  data$polar <- params$polar
  data$width <- ggplot2::resolution(data$x_major, zero = FALSE) * params$width
  data$height <- ggplot2::resolution(data$y_major, zero = FALSE) * params$height

  if (params$polar) {
    theta <- 2 * pi * rescale01(data$x_minor)
    r <- rescale01(data$y_minor)

    data$x <- data$x_major + params$width  / 2 * r * sin(theta)
    data$y <- data$y_major + params$height / 2 * r * cos(theta)
    data <- data[order(data$x_major, data$x_minor), ]

  } else {
    data$x <- data$x_major + rescale11(data$x_minor) * params$width / 2
    data$y <- data$y_major + rescale11(data$y_minor) * params$height / 2

  }

  data
}


calc_ref_line <- function(data, params){
  ref_line <- data

  if (any(data$polar)) {
    theta <- seq(0, 2 * pi, length.out = 30)
    ref_line <- ref_line %>% transform(
      group = group,
      x = x_major + width / 4 * sin(theta),
      y = y_major + height / 4 * cos(theta)
    )
  } else{
    ref_line <- ref_line %>% transform(group = group,
                                       x = x_major + params$width / 2,
                                       y = y_major) %>%
      rbind(ref_line %>% transform(
        group = group,
        x = x_major - params$width / 2,
        y = y_major
      )
      )
  }

  ref_line
}


calc_ref_box <- function(data, params){
  ref_box <- data %>%
    dplyr::mutate(xmin = x_major -  params$width / 2,
                   xmax = x_major + params$width / 2,
                   ymin = y_major - params$height / 2,
                   ymax = y_major + params$height / 2)
  ref_box
}

