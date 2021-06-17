#' #' @export
#' as_tsibble.cubble_df <- function(data, ...) {
#'   out <- as_tsibble(as_tibble(data), ...)
#'   group_vars <- names2(data %@% groups)[1]
#'   meta_data <- data %@% meta
#'   attr_data <- attributes(data)
#'   attr_tsibble_all <- attributes(out)
#'   attr_tsibble <- attr_tsibble_all[setdiff(names(attr_tsibble_all), names(attr_data))]
#'   cubble_df(data, group_vars = group_vars, meta_data = meta_data, class = "tbl_ts", others = attr_tsibble, format = "long")
#' }
#'
#' #' @export
#' index_by.cubble_df <- function(data, ...) {
#'   out <- index_by(as_tsibble(data), ...)
#'   group_vars <- names2(data %@% groups)[1]
#'   meta_data <- data %@% meta
#'   cubble_df(data, group_vars = group_vars, meta_data = meta_data, format = "long")
#' }
