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
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell. Defaults to \code{\link{identity}} so
#'  that no scaling is performed.
#' @param global_rescale Whether rescale is performed globally or on each
#' individual glyph.
#' @export
#' @rdname glyph
#' @return a ggplot object
#' @examples
#' print_p <- GGally::print_if_interactive
#'
#' library(ggplot2)
#' # basic glyph map with reference line and box---------------
#' p <- ggplot(data = GGally::nasa,
#'        aes(x_major = long, x_minor = day,
#'            y_major = lat, y_minor = surftemp)) +
#'   geom_glyph_box() +
#'   geom_glyph_line() +
#'   geom_glyph() +
#'   theme_bw()
#' print_p(p)
#'
#' # rescale on each individual glyph ---------------
#' p <- ggplot(data = GGally::nasa,
#'        aes(x_major = long, x_minor = day,
#'            y_major = lat, y_minor = surftemp)) +
#'   geom_glyph(global_rescale = FALSE)
#' print_p(p)
#'
#' # adjust width and height with relative & absolute value ---------------
#' p <- ggplot() +
#'   geom_glyph(data = GGally::nasa,
#'              aes(x_major = long, x_minor = day,
#'                  y_major = lat, y_minor = surftemp),
#'                  width = rel(0.8), height = 1) +
#'    theme_bw()
#' print_p(p)
#'
#' # apply a re-scaling on Y and use polar coordinate
#' p <-
#'   GGally::nasa |> 
#'   ggplot(aes(x_major = long, x_minor = day,
#'              y_major = lat, y_minor = ozone)) +
#'     geom_glyph_box(fill=NA) +
#'     geom_glyph_line() +
#'     geom_glyph(y_scale = GGally::range01, polar = TRUE)
#' print_p(p)
geom_glyph <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., x_major = NULL,
                       x_minor = NULL, y_major = NULL, y_minor = NULL,
                       x_scale = identity, y_scale = identity,
                       polar = FALSE, width = ggplot2::rel(2.1),
                       height = ggplot2::rel(1.8), global_rescale = TRUE,
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
      global_rescale = global_rescale,
      x_scale = make_scale(x_scale),
      y_scale = make_scale(y_scale),
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

  draw_panel = function(data, panel_params, coord, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...)
  },


  required_aes = c("x_minor", "x_major", "y_minor", "y_major"),
  default_aes = ggplot2::aes(
    colour = "black", size = 0.5, linetype = "solid", alpha = 1,
    linewidth = 0.5,
    polar = FALSE,
    width = ggplot2::rel(2.1),
    height = ggplot2::rel(2.1),
    global_rescale = TRUE,
    x_scale = make_scale(identity),
    y_scale = make_scale(identity)
  )
)

#' @export
#' @rdname glyph
geom_glyph_line <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., x_major = NULL,
                            x_minor = NULL, y_major = NULL, y_minor = NULL,
                            polar = FALSE, width = ggplot2::rel(2.1),
                            height = ggplot2::rel(2.1),
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
    colour = "grey", size = 1, linetype = "solid", alpha = 1, linewidth = 0.5,
    polar = FALSE,
    width = ggplot2::rel(2.1),
    height = ggplot2::rel(2.1)
  )
)

#' @export
#' @rdname glyph
geom_glyph_box <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., x_major = NULL,
                           x_minor = NULL, y_major = NULL, y_minor = NULL,
                           polar = FALSE, width = ggplot2::rel(2.1),
                           height = ggplot2::rel(2.1),
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
    colour = "grey", size = 0.5, linetype = "solid", alpha = 1, linewidth = 0.5,
    fill = "transparent",
    polar = FALSE,
    width = ggplot2::rel(2.1),
    height = ggplot2::rel(2.1)
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

is.rel <- function(x) inherits(x, "rel")

has_scale <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  # Unwrap the list structure so that it can be stored in a tbl
  stopifnot(is.list(x))
  x <- x[[1]]
  !is_null(x) &&
    !(
      identical(x, "identity") ||
      identical(x, identity)
    )
}
make_scale <- function(x) {
  # Wrap the list structure so that it can be stored in a tbl
  list(x)
}
get_scale <- function(x) {
  stopifnot(is.list(x))
  # Unwrap the list structure so that it can be stored in a tbl
  fn <- x[[1]]
  if (is.character(fn)) {
    # Legacy support for `"identity"`
    fn <- get(unique(x)[1], envir = globalenv(), mode = "function")
  }
  stopifnot(is.function(fn))
  fn
}

glyph_data_setup <- function(data, params){
  if (length(unique(data$group)) == 1){
    data$group <- as.integer(interaction(data$x_major, data$y_major,
                                         drop = TRUE))
    data <- data |>  dplyr::group_by(.data$group)
  }

  if (has_scale(params$x_scale)) {
    x_scale <- get_scale(params$x_scale)
    data <-
      data |> 
      dplyr::mutate(
        x_minor = x_scale(.data$x_minor)
      )
  }

  if (has_scale(params$y_scale)) {
    y_scale <- get_scale(params$y_scale)
    data <-
      data |> 
      dplyr::mutate(
        y_minor = y_scale(.data$y_minor)
      )
  }

  datetime_class <- c(
    "Date", "yearmonth", "yearweek", "yearquarter","POSIXct", "POSIXlt")
  if (any(class(data$x_minor) %in% datetime_class)){
    data[["x_minor"]] <- as.numeric(data[["x_minor"]])
  }

  data <- data |> 
    dplyr::mutate(
      polar = params$polar,
      width = ifelse(!is.rel(params$width), unclass(params$width),
        ggplot2::resolution(.data$x_major, zero = FALSE) *
          unclass(params$width)),
      height = ifelse(!is.rel(params$height), unclass(params$height),
        ggplot2::resolution(.data$y_major, zero = FALSE) *
          unclass(params$height))
      )

  if (any(data$polar)) {
    data <- data |>  dplyr::ungroup()
    theta <- 2 * pi * rescale01(data$x_minor)
    r <- rescale01(data$y_minor)

    data <- data |> 
      dplyr::mutate(x = .data$x_major + .data$width / 2 * r * sin(theta),
                    y = .data$y_major + .data$height / 2 * r * cos(theta)) |> 
      dplyr::arrange(.data$x_major, .data$x_minor)

  } else {
    if (isTRUE(params$global_rescale)) data <- data |>  dplyr::ungroup()
    data <- data |> 
      dplyr::mutate(
        x = .data$x_major + rescale11(.data$x_minor) * .data$width / 2,
        y = .data$y_major + rescale11(.data$y_minor) * .data$height / 2)
  }

  data |> dplyr::ungroup()
}


calc_ref_line <- function(data, params){
  ref_line <- data

  if (any(data$polar)) {
    theta <- seq(0, 2 * pi, length.out = 30)
    ref_line <- ref_line |>  dplyr::mutate(
      group = .data$group,
      x = .data$x_major + .data$width / 4 * sin(theta),
      y = .data$y_major + .data$height / 4 * cos(theta)
    )
  } else{
    ref_line <- ref_line |> 
      dplyr::mutate(group = .data$group,
                    x = .data$x_major + .data$width/ 2,
                    y = .data$y_major) |> 
      rbind(ref_line |>  dplyr::mutate(group = .data$group,
                                       x = .data$x_major - .data$width / 2,
                                       y = .data$y_major))
  }

  ref_line
}


calc_ref_box <- function(data, params){
  ref_box <- data |> 
    dplyr::mutate(xmin = .data$x_major - .data$width / 2,
                   xmax = .data$x_major + .data$width / 2,
                   ymin = .data$y_major - .data$height / 2,
                   ymax = .data$y_major + .data$height / 2)
  ref_box
}
