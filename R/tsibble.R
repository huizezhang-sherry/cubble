#' @export
as_tsibble.cubble_df <- function(data, ...) {
  out <- as_tsibble(as_tibble(data), ...)
  group_vars <- group_vars(data)
  meta_data <- meta(data)
  cubble_df(out, group_vars = group_vars, meta_data = meta_data,  format = "long")
}

#' #' @export
#' index_by.cubble_df <- function(data, ...) {
#'   out <- index_by(as_tsibble(data), ...)
#'   group_vars <- group_vars(data)
#'   meta_data <- meta(data)
#'   cubble_df(data, group_vars = group_vars, meta_data = meta_data, format = "long")
#' }
