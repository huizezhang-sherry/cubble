#' tsibble related functions
#' @param data the data object to be operated on
#' @param ... other arguments passed onto the function
#'
#' @export
#' @importFrom tsibble as_tsibble
# as_tsibble.cubble_df <- function(data) {
#   group_vars <- group_vars(data)
#   meta_data <- meta(data)
#
#   out <- as_tsibble(tibble::as_tibble(data), key = !!group_vars)
#   cubble_df(out, group = group_vars, meta_data = meta_data,  format = determine_format(out))
# }

#' @export
#' @importFrom tsibble index_by
index_by.cubble_df <- function(data, ...) {
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  out <- index_by(NextMethod(data),  ...)
  cubble_df(out, group = group_vars, meta_data = meta_data, format = determine_format(out))
}

