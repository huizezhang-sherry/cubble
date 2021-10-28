#' tsibble related functions
#' @param data the data object to be operated on
#' @param ... other arguments passed onto the function
#' @importFrom tsibble index_by
# index_by.cubble_df <- function(data, ...) {
#   #leaves_data <- leaves(data)
#   out <- index_by(NextMethod(data),  ...)
#   new_cubble(out,
#              key = key_vars(out), index = index(data), coords = coords(data),
#              leaves = leaves_data, form = determine_form(out))
# }
#
