#' Switch a cubble object from the nested form into the long form
#'
#' `stretch()` switches a cubble object into a long cubble, suitable for temporal operations.
#' The long cubble uses the combination of `key` and `index` to identify each row and
#' arranges each `key` as a separate group.
#' @param data a nested cubble object
#' @param col the list column to be stretched, `col` is required to be specified
#' if there are more than one list column and the list column name is not `ts`
#'
#' @examples
#' climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   stretch()
#' @export
stretch <- function(data, col) {
  test_cubble(data)
  UseMethod("stretch")
}

#' @export
stretch.cubble_df <- function(data, col){
  test_nested(data)

  key <- syms(key_vars(data))
  if (length(key) == 2){
    cur_key <- key[key %in% names(data)]
  } else{
    cur_key <- key[[1]]
  }
  index <- index(data)
  coords <- coords(data)
  data <- as_tibble(data)
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])

  col <- enquo(col)
  if (rlang::quo_is_missing(col)) col <- sym(get_listcol(data))

  # unnest the temporal variables
  if (is_tsibble) data$ts <- map(data$ts, tibble::as_tibble)
  out <- data %>% dplyr::select(!!!cur_key, !!col) %>% tidyr::unnest(c(!!col))

  # organise spatial variables into `spatial`
  spatial <- data %>% select(-!!col)
  if (".val" %in% colnames(spatial)) spatial <- spatial %>% tidyr::unnest(.data$.val)

  if (is_tsibble){
    out <- out %>% tsibble::as_tsibble(key = !!cur_key, index = index)
    tsibble_attr <- attributes(out)
  }

  new_cubble(out,
             key = map_chr(key, as_name), index = index, coords = coords,
             spatial = spatial,
             #tsibble_attr = tsibble_attr,
             form = "long")
}
