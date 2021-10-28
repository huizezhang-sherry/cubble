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
