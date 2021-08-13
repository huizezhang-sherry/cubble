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

#' @export
#' @rdname attributes
leaves <- function(data, stem){
  test_cubble(data)

  if (stem == "time" & form(data) == "nested"){
    leaves_data <- data %>% stretch() %>% as_tibble()
    tibble::new_tibble(leaves_data, nrow = nrow(leaves_data), groups = group_vars(data), stem = "time", class = "leaves")
  } else if (stem == "spatial" & form(data) == "long"){
    nested_form <- data %>% tamp()
    listcol <-  nested_form %>% listcol_name()
    leaves_data <- nested_form %>% as_tibble() %>% select(-listcol)
    tibble::new_tibble(leaves_data, nrow = nrow(leaves_data), groups = group_vars(data), stem = "spatial", class = "leaves")
  } else{
    abort("Use `leaves` to see the time stem when data is in the nested form, or
          Use `leaves` to see the spatial stem when data is in the long form")
  }
}

listcol_name <- function(data){
  test_nested(data)

  var_type <- map_chr(data, class)
  names(var_type[var_type == "list"])
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
