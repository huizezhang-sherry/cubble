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


  if (determine_form(data) == "long"){
    sp <- data %@% "spatial"
    if (".val" %in% names(sp)) {
      unnest(sp , .val)
    } else{
      sp
    }

  } else{
    NULL
  }

}

#' Create and manipulate leaves attributes
#'
#' A leaves object records whether a variable in the cubble varies across time or not and
#' make it possible for a cubble to switch between the nested and long form.
#' This set of functions create and manipulate the leaves object.
#'
#' @details
#' * `new_leaves()` creates a leaves object by supplying a flat data and the grouping variable
#' * `is_leaves()` tests whether the object is a leaves object
#' * `invariant()` and `variant()` extracts the time-variant and invariant variables from a leaves object
#' * `tbl_sum()` provides a customised printing header for leaves
#'
#' Notice that users may not need to these functions to directly manipulate the leaves.
#' To extract the leaves object from a cubble, use `leaves()`
#'
#' @param data a flat data without any nesting structure
new_spatial <- function(data){
  test_cubble(data)

  form <- determine_form(data)
  if (form == "nested"){
    spatial <-  NULL
  } else if (form == "long"){
    spatial <- spatial(data)
  } else{
    cli::cli_abort("{.field form} can only be either long or nested in a cubble.")
  }

  spatial
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

#' @export
#' @rdname attributes
row_id <- function(data){
  data %@% "row_id"
}
