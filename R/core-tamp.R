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
  key <- rlang::syms(key_vars(data))
  key_name <- map_chr(key, rlang::as_name)
  index <- index(data)
  coords <- coords(data)

  spatial <- spatial(data)
  if (length(key) == 1){
    tvars <- colnames(data)[colnames(data) != key_name]
    tvars <- tvars[!tvars %in% colnames(spatial)]

    migrated_var <- intersect(names(data), names(spatial)) %>%
      setdiff(key_name)

    temporal <- tibble::as_tibble(data) %>%
      dplyr::select(-migrated_var) %>%
      tidyr::nest(ts = -key_name)

    out <- spatial %>% dplyr::left_join(temporal, by = key_name)

  } else if (length(key) == 2){
    spatial <- spatial %>% tidyr::nest(.val = -key_name[1])
    temporal <- as_tibble(data) %>% tidyr::nest(ts = -key_name[1])
    out <- left_join(spatial, temporal, by = key_name[1])
  }


  if ("tbl_ts" %in% class(data)){
    out <- out %>% mutate(ts = map(ts, ~tsibble::as_tsibble(.x, index = index)))
  }

  new_cubble(out,
             key = key_name, index = index, coords = coords,
             spatial = spatial, form = "nested")
}

