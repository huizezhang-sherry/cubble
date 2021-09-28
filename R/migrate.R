#' Incorporate group-wise metadata into a long form cubble
#'
#' @details
#' Some spatio-temporal visualisations, i.e. a glyph map, require both group-wise variables
#' and time-wise variables to make the plot. While data manipulation can be easier when separating
#' those two types of variables, the last step before plotting the data is to transfer the useful
#' group-wise variables into the long form. `migrate()` is a shortcut for transferring group-wise
#' variables from cubble's metadata. For transfering other group-wise variables, use `left_join`.
#'
#' @param data a long form tibble object
#' @param ... variables in metadata to included into the long form
#' @examples
#' dt <- climate_small %>% stretch() %>% migrate(lat, long)
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
