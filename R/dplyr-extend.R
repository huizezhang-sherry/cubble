#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
dplyr_col_modify.cubble_df <- function(data, cols) {
  #browser()
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = !!group_vars), cols)
  } else{
    out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  }

  cubble_df(out, group = group_vars, meta_data = meta_data, format = determine_format(out))
}

dplyr_row_slice.cubble_df <- function(data, i, ...) {
  out <- vec_slice(data, i)
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  # update meta data
  meta_col <- group_vars[map_lgl(group_vars, ~has_name(meta_data, .x))]
  row <- meta_data[[meta_col]] %in% unique(out[[meta_col]])
  meta_data <- meta_data[row,]

  cubble_df(out, group = group_vars, meta_data = meta_data, format = determine_format(out) )
}

dplyr_reconstruct.cubble_df <- function(data, template) {
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  cubble_df(data, group = group_vars, meta_data = meta_data, format = determine_format(data))
}


#' @export
summarise.cubble_df <- function(data, ...){
  group_vars <- group_vars(data)
  meta_data <- meta(data)
  out <- NextMethod("summarise")

  cubble_df(out, group = group_vars, meta_data = meta_data, format = determine_format(out))
}
