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
#' @param key the spatial identifier
#'
#' @examples
#' new_leaves(aus_climate, id)
#' new_leaves(aus_climate, id) %>% is_leaves()
#' invariant(leaves(aus_climate))
#' variant(leaves(aus_climate))
#' @rdname leaves
new_leaves <- function(data, key){
  data <- as_tibble(data)
  data <- data[map_lgl(as_tibble(data), ~all(class(.x) != "list") )]
  key <- enquo(key)
  all_vars <- find_invariant(data, !!key)
  invariant <- data %>% select(all_vars$invariant) %>% map_chr(pillar::type_sum)
  variant <- data %>% select(all_vars$variant) %>% map_chr(pillar::type_sum)
  leaves_data <- unique(data[names(invariant)])

  tibble::new_tibble(leaves_data, nrow = nrow(leaves_data),
                     #key = as_name(key),
                     invariant = invariant, variant = variant,
                     class = "leaves")
}

as_leaves <- function(data, variant){

  invariant <- data %>%  map_chr(pillar::type_sum)

  tibble::new_tibble(data, nrow = nrow(data),
                     #key = as_name(key),
                     invariant = invariant, variant = variant,
                     class = "leaves")
}

#' @export
tbl_sum.leaves <- function(data){
  var_names <- names(variant(data))
  var_type <- variant(data)
  c("Leaves" = glue::glue("{nrow(data)} x {ncol(data)}"))
}

#' @export
#' @rdname leaves
is_leaves <- function(data){
  inherits(data, "leaves")
}

#' @export
#' @rdname leaves
invariant <- function(data){
  test_leaves(data)
  data %@% invariant
}

#' @export
#' @rdname leaves
variant <- function(data){
  test_leaves(data)
  data %@% variant
}
