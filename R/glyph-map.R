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
#' @param ref_line,ref_box Wether to include the horizontal middle reference
#' line and the reference bounding box.
#' @export
#' @examples
#' library(ggplot2)
#' library(GGally)
#' # basic glyph map with reference line and box---------------
#' ggplot() +
#'   geom_glyph(data = GGally::nasa,
#'              aes(x_major = long, x_minor = day,
#'                  y_major = lat, y_minor = surftemp),
#'              ref_box = TRUE, ref_line = TRUE)+
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
                       ref_line = FALSE, ref_box = FALSE,
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
      ref_line = ref_line,
      ref_box = ref_box,
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
  },

  draw_panel = function(data,  panel_params, coord, ...) {
    grid::gList(if (any(data$ref_box)) {
      ref_box <- calc_ref_box(data)
      ggplot2:::GeomRect$draw_panel(ref_box, panel_params, coord, ...)
    },
    if (any(data$ref_line)) {
      ref_line <- calc_ref_line(data)
      ggplot2:::GeomPath$draw_panel(ref_line, panel_params, coord, ...)
    },
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...))
  },


  required_aes = c("x_minor", "x_major", "y_minor", "y_major"),
  default_aes = ggplot2::aes(
    colour = "black", size = 0.5, linetype = 1, alpha = 1,
    polar = FALSE,
    width = ggplot2::rel(1.5),
    height = ggplot2::rel(1.5),
    ref_line = FALSE,
    ref_box = FALSE
  )
)


#' @export
#' @rdname geom_glyph
stat_glyph <- function(mapping = NULL, data = NULL, geom = "identity",
                       position = "identity", ...,
                       x_major = NULL, x_minor = NULL,
                       y_major = NULL, y_minor = NULL, polar = FALSE,
                       width = ggplot2::rel(1.5),
                       height = ggplot2::rel(1.5),
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatGlyph,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      x_major = NULL,
      x_minor = NULL,
      y_major = NULL,
      y_minor = NULL,
      polar = FALSE,
      width = width,
      height = height,
      ...
    )
  )
}

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

calc_ref_line <- function(data){
  ref_line <- data %>% dplyr::select(-c(x_minor, y_minor, x, y))


  if (any(data$polar)) {
    theta <- seq(0, 2 * pi, length.out = 30)
    ref_line <- ref_line %>% transform(
      group = group,
      x = x_major + width / 4 * sin(theta),
      y = y_major + height / 4 * cos(theta)
    )
  } else{
    ref_line <- ref_line %>% transform(group = group,
                                       x = x_major +  width / 2,
                                       y = y_major) %>%
      rbind(ref_line %>% transform(
        group = group,
        x = x_major - width / 2,
        y = y_major
      )
      )
  }

  ref_line %>% transform(colour = "grey",
                         size = 1)
}


calc_ref_box <- function(data){

  ref_box <- data %>%
    dplyr::select(-c(x_minor, y_minor, x, y)) %>%
    dplyr::mutate(xmin = x_major - width / 2,
                   xmax = x_major + width / 2,
                   ymin = y_major - height / 2,
                   ymax = y_major + height / 2,
                  colour = "grey",
                  fill = "transparent")
  ref_box
}

