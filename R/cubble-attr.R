#' Functions to get attributes from a cubble object
#'
#' @details
#' Apart from inheriting attributes `names`, `row.names`, and `class` from the underlying tibble,
#' a cubble object will also have additional attributes: `groups`, `meta` and `form`.
#'
#' `groups()`, `meta()`, and `form()` provide a more  convenient way to extract
#' these attributes without `%@%` from a cubble.
#' `group_vars()` extracts the variable name of the group, which can often be useful.
#'
#' If a cubble object is also a tsibble, then tsibble attributes (`key`, `index`, `index2`, `interval`)
#' are also preserved and can be accessed via the relevant functions in the tsibble package.
#'
#' @examples
#' # extract attributes of a cubble object
#' form(climate_small)
#' leaves(climate_small) %>% head(5)
#' key_data(climate_small) %>% head(5)
#' key_vars(climate_small)
#'
#' # print out the attribute names of cubble with a tsibble underlying class
#' names(attributes(climate_small %>% stretch()))
#' @param data an cubble object
#'
#' @export
#' @rdname attributes
form <- function(data){
  test_cubble(data)
  data %@% form
}


determine_form <- function(data){
  # determine_form is a lower level detector of the form based on the vector class
  cls <- unlist(map(data, class))

  if ("list" %in% cls){
    "nested"
  } else{
    "long"
  }
}

#' @export
#' @rdname attributes
leaves <- function(data){
  test_cubble(data)
  data %@% "leaves"
}

#' @export
#' @rdname attributes
key_vars <- function(data){
  names <- names2(key_data(data))
  names[names != ".rows"]
}

#' @export
#' @rdname attributes
key_data <- function(data){
  test_cubble(data)
  data %@% "groups"
}

#' @export
#' @rdname attributes
coords <- function(data){
  data %@% "coords"
}

#' @export
#' @rdname attributes
coord_x <- function(data){
  coords(data)[1]
}

#' @export
#' @rdname attributes
coord_y <- function(data){
  coords(data)[2]
}

#' @export
#' @rdname attributes
index <- function(data){
  data %@% "index"
}
