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
#' @export
migrate <- function(data, ...){

  test_cubble(data)

  if (form(data) != "long"){
    abort("data needs to be in long form, convert using `zoom()`")
  }

  to_join <- meta(data) %>% select(group_vars(data), ...)
  data %>% left_join(to_join)

}
