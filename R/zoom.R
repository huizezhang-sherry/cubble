#' Switch a cubble object into a long form
#' @param data a cubble object
#' @param key the spatio identifier. Key can be automatically detected for a cubble object
#' @details
#' `zoom()` switch a cubble object into the long form where the combination of group identifier
#' and timestamp defines a row. The long form cubble is always of class `cubble_df` and `grouped_df`.
#'
#' @examples
#' climate_flat %>% tamp(station) %>% zoom()
#'
#' @export
#' @seealso Other cubble verbs include \code{\link{tamp}} and \code{\link{migrate}}
zoom <- function(data, key) {
  test_cubble(data)
  UseMethod("zoom")
}

#' @export
zoom.cubble_df <- function(data, key){

  col <- sym(names(data)[map_chr(data, class) == "list"])

  if (!is_list(eval_tidy(col, data))) {
    abort("The column to zoom need to be a list-column")
  }

  key <- enquo(key)

  if (quo_is_missing(key)){
    key <- quo(!!group_vars(data))
  } else{
    key <- quo_get_expr(key)
  }

  meta_data <- tibble::as_tibble(data[, find_non_varying_var(data, !!key)])


  list_col <- data %>% dplyr::pull(!!col)

  if ("tbl_ts" %in% class(list_col[[1]])){
    data$ts <- map(data$ts, tibble::as_tibble)
    out <- data[vec_c(as_name(key), as_name(col))]
    out <- out %>%
      tidyr::unnest(!!col) %>%
      tsibble::as_tsibble(key = !!key)
  } else{
    out <- data %>%
      dplyr::select(!!key, !!col) %>%
      tidyr::unnest(!!col)
  }

  cubble_df(out, group = as_name(key), meta_data = meta_data, form = "long")
}
