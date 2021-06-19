#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
dplyr_col_modify.cubble_df <- function(data, cols) {

  group_vars <- group_vars(data)
  meta_data <- meta(data)

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(as_tsibble(as_tibble(data), key = !!group_vars), cols)
  } else{
    out <- dplyr_col_modify(as_tibble(data), cols)
  }

  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "wide")
}

dplyr_row_slice.cubble_df <- function(data, i, ...) {
  out <- vec_slice(data, i)
  group_vars <- group_vars(data)
  meta_data <- meta(data)
  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "long")
}

dplyr_reconstruct.cubble_df <- function(data, template) {
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  cubble_df(data, group_vars = group_vars, meta_data = meta_data, format = "long")
}

