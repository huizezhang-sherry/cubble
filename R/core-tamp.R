#' Switch an cubble object from the long form into the nested form
#'
#' `tamp()` turns a long cubble back into a nest cubble and can be seen as
#' the inverse operation of \code{stretch()}.
#' The nested cubble identifies each row by `key` and is suitable
#' for operations whose output doesn't involve a time index.
#'
#' @param data a long cubble object
#' @examples
#' cb_long <- climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   stretch()
#'
#' cb_long %>% tamp()
#' @export
tamp <- function(data) {
  test_cubble(data)
  UseMethod("tamp")
}


#' @export
tamp.cubble_df <- function(data) {
  test_long(data)

  # will only keep the first grouping variable if more than one
  key <- rlang::sym(key_vars(data))
  if (length(key) > 1) key <- key[1]
  spatial <- spatial(data)
  # also sort out double keys
  tvars <- colnames(data)[colnames(data) != rlang::as_name(key)]
  tvars <- tvars[!tvars %in% colnames(spatial)]

  out <- tibble::as_tibble(data) %>%
    dplyr::left_join(spatial, by = rlang::as_name(key))

  if ("tbl_ts" %in% class(data)){
    out <- out %>%
      tsibble::as_tsibble(key = key_vars(data), index = index(data))
  }

  as_cubble(out,key = !!key, index = !!index(data), coords = coords(data))
}

