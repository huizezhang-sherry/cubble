#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {
  out <- dplyr:::dplyr_col_modify(as_tibble(data), cols)
  group_vars <- names2(data %@% groups)[1]
  meta_data <- data %@% meta

  # if ("tbl_ts" %in% class(data)){
  #   attr_data <- attributes(data)
  #   attr_tsibble_all <- attributes(out)
  #   attr_tsibble <- attr_tsibble_all[setdiff(names(attr_tsibble_all), names(attr_data))]
  #   cubble_df(out, group_vars = group_vars, meta_data = meta_data, class = "tbl_ts", others = attr_tsibble, format = "wide")
  # } else{
  #   cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "wide")
  # }

  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "wide")
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...) {
  out <- vec_slice(data, i)
  group_vars <- names2(data %@% groups)[1]
  meta_data <- data %@% meta
  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "long")
}

#' @importFrom dplyr dplyr_reconstruct dplyr_row_slice
#' @export
dplyr_reconstruct.cubble_df <- function(data, template) {
  group_vars <- names2(data %@% groups)[1]
  meta_data <- data %@% meta

  cubble_df(data, group_vars = group_vars, meta_data = meta_data, format = "long")
}
