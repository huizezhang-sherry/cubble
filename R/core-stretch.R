#' Switch a cubble object into a long form
#'
#' `stretch()` switches a cubble object into a long cubble, suitable for temporal operations.
#' The long cubble uses the combination of `key` and `index` identifies each row and
#' arrange each `key` as a separate group.
#' @param data a cubble object
#' @param col the list column to be stretched
#'
#' @examples
#' climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   stretch()
#'
#' @export
#' @seealso Other cubble verbs include \code{\link{tamp}} and \code{\link{migrate}}
stretch <- function(data, col) {
  test_cubble(data)
  UseMethod("stretch")
}

#' @export
stretch.cubble_df <- function(data, col){
  key <- syms(key_vars(data))
  index <- index(data)
  coords <- coords(data)
  row_id <- row_id(data)
  data <- as_tibble(data)
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])

  col <- enquo(col)
  if (rlang::quo_is_missing(col)) col <- sym(get_listcol(data))

  # unnest the temporal variables
  if (is_tsibble) data$ts <- map(data$ts, tibble::as_tibble)
  out <- data %>% dplyr::select(!!!key, !!col) %>% tidyr::unnest(c(!!col))

  # organise spatial variables into `spatial`
  spatial <- data %>% select(-!!col)
  if (".val" %in% colnames(spatial)) spatial <- spatial %>% tidyr::unnest(.val)

  if (is_tsibble){
    out <- out %>% tsibble::as_tsibble(key = !!key[[1]], index = index)
  }

  new_cubble(out,
             key = map_chr(key, as_name), index = index, coords = coords,
             row_id = row_id, spatial = spatial, form = "long")
}
