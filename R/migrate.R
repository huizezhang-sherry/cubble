#' Incorporate meta data into the long form
#'
#'
#' @param data a long form tibble object
#' @param ... variables in metadata to included into the long form
#' @examples
#' \dontrun{
#' out <- oz_zoom2 %>% migrate(lat, long)
#' # Currently, switch back to global model will remove those migrated variables
#' out %>% global()
#' }
#' @importFrom dplyr left_join
#' @export
migrate <- function(data, ...){

  vars <- enquos(...)
  test_cubble(data)

  if (form(data) != "long"){
    abort("data needs to be in long form, convert using `zoom()`")
  }

  out <- map_lgl(vars, ~as_name(.x) %in% names(meta(data)))

  if (!all(out)){
    abort("only variables in the metadata can be migrated into the long form. See metadata with `meta()`")
  }

  key <- group_vars(data)
  key_to_join <- key[key %in% names(meta(data))]
  to_join <- meta(data) %>% dplyr::select(key_to_join, !!!vars)
  data %>% left_join(to_join, by = key_to_join)

}
