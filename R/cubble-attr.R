#' Functions to extract cubble attributes
#'
#' @details
#' Apart from inheriting attributes `names`, `row.names`, and `class` from the underlying tibble,
#' a cubble has its site identifier: `key`, temporal identifier, `index`, and
#' spatial coordinate reference: `coords`.
#'
#' If a cubble object is also a tsibble, then tsibble attributes (`key`, `index`, `index2`, `interval`)
#' are also preserved and can be accessed via the relevant functions in the tsibble package. (NOT FULLY IMPLEMENTED)
#'
#' @examples
#' # extract attributes of a cubble object
#' form(aus_climate)
#' spatial(aus_climate) %>% head(5)
#' key_data(aus_climate) %>% head(5)
#' key_vars(aus_climate)
#' index(aus_climate)
#' coords(aus_climate)
#' coord_x(aus_climate)
#' coord_y(aus_climate)
#'
#' @param data an cubble object
#'
#' @export
#' @rdname attributes
form <- function(data){
  test_cubble(data)
  data %@% form
}

#' @export
#' @rdname attributes
is_long <- function(data){
  test_cubble(data)
  form(data) == "long"
}

#' @export
#' @rdname attributes
is_nested <- function(data){
  test_cubble(data)
  form(data) == "nested"
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
spatial <- function(data){
  test_cubble(data)
  data %@% "spatial"
}

#' @export
#' @rdname attributes
key_vars <- function(data){
  names <- names2(key_data(data))
  index <- index(data)
  names[!names %in% c(".rows", index)]
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
