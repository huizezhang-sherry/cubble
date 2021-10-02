#' Move item-wise variables into long form
#'
#' @details
#' Some spatio-temporal visualisations, i.e. a glyph map, require both item-wise
#' and time-wise variables to create the plot. `migrate()` moves item-wise variables
#' into the long form before creating the visualisation.
#'
#' @param data a long form tibble object
#' @param ... variables in metadata to included into the long form
#' @examples
#' library(ggplot2)
#' library(GGally)
#' library(dplyr)
#' dt <- aus_climate %>%
#'   stretch() %>%
#'   migrate(lat, long) %>%
#'   filter(lubridate::month(date) == 2 ) %>%
#'   mutate(yday = lubridate::yday(date)) %>%
#'   select(-date)
#'
#' g <- glyphs(dt,
#'             x_major = "long", x_minor = "yday",
#'             y_major = "lat", y_minor = "tmax", height = 1, width = 2)
#'
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' plot_map(state_map) +
#'   ggplot2::geom_path(data = g, aes(gx, gy, group = gid)) +
#'   ggplot2::theme_void() +
#'   ggplot2::labs(x = "", y = "")
#' @export
#' @rdname cubble-verb
migrate <- function(data, ...){

  dots <- enquos(..., .named = TRUE)
  test_cubble(data)

  if (form(data) != "long"){
    abort("data needs to be in long form, convert using `stretch()`")
  }

  in_leaves <- map_lgl(names(dots), ~.x %in% names(leaves(data)))
  if (!all(in_leaves)){
    inform(glue::glue("`{names(dots)[!in_leaves]}` does not present in the spatial stem of the data, hence not migrated. "))
  }

  to_join <- leaves(data) %>% select(key_vars(data)[1], names(dots)[in_leaves])
  data %>% left_join(to_join)

}
