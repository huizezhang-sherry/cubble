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
#' climate_small %>% leaves(stem = "time") %>% head(5)
#' climate_small %>% stretch() %>% leaves(stem = "spatial") %>% head(5)
#' groups(climate_small) %>% head(5)
#' group_vars(climate_small)
#'
#' # print out the attribute names of cubble with a tsibble underlying class
#' names(attributes(climate_small %>% stretch()))
#' @param data an cubble object
#' @param stem either "time" or "spatial".
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

as_leaves <- function(data, groups, stem){
  tibble::new_tibble(data, nrow = nrow(data),
                     groups = groups, stem = stem, class = "leaves")
}


#' @export
#' @rdname attributes
leaves <- function(data, stem){
  test_cubble(data)

  if (stem == "time" & form(data) == "nested"){
    leaves_data <- data %>% mutate(ts = list(map(.data$ts, as_tibble))) %>% tidyr::unnest(ts)
    as_leaves(leaves_data, groups = group_vars(data), stem = "time")
  } else if (stem == "spatial"){
    data %@% leaves
  } else{
    abort("Use `leaves` to see the time stem when data is in the nested form, or
          Use `leaves` to see the spatial stem when data is in the long form")
  }
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
