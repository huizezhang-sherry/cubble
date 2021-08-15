#' tsibble related functions
#' @param data the data object to be operated on
#' @param ... other arguments passed onto the function
#' @export
#' @importFrom tsibble index_by
index_by.cubble_df <- function(data, ...) {
  leaves_data <- leaves(data)
  out <- index_by(NextMethod(data),  ...)
  group_vars <- group_vars(out)
  new_cubble(out, group = group_vars, leaves = leaves_data, form = determine_form(out))
}

