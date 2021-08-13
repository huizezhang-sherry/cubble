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
#' meta(climate_small) %>% head(5)
#' groups(climate_small) %>% head(5)
#' group_vars(climate_small)
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
  cls <- unlist(map(data, class))

  if ("list" %in% cls){
    "nested"
  } else{
    "long"
  }
}

#' @export
#' @rdname attributes
meta <- function(data){
  test_cubble(data)
  data %@% meta
}

#' @export
#' @rdname attributes
group_vars <- function(data){
  groups <- groups(data)
  names <- names2(groups)
  names[names != ".rows"]
}

#' @export
#' @rdname attributes
groups <- function(data){
  data %@% groups
}
