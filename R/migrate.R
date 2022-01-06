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
#' state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
#' plot_map(state_map) +
#'   geom_glyph(data = dt,
#'              aes(x_major = long, x_minor = yday,
#'                  y_major = lat, y_minor = tmax))
#' @export
#' @rdname cubble-verb
migrate <- function(data, ...){
  dots <- enquos(..., .named = TRUE)
  test_cubble(data)
  sp <- spatial(data)

  if (form(data) != "long"){
    cli::cli_abort("{.fn migrate} should be used on the long form.")
  }

  in_spatial <- map_lgl(names(dots), ~.x %in% names(sp))
  if (!all(in_spatial)){
    cli::cli_inform(
      "{.code {names(dots)[!in_spatial]}} does not exist in spaital stem. No migration")
  }

  to_join <- sp %>% select(key_vars(data)[1], names(dots)[in_spatial])
  suppressMessages(data %>% left_join(to_join))


}
