#' Switch a cubble object into the long form
#'
#' `face_temporal()` switches a cubble object into a long cubble, suitable for temporal operations.
#' The long cubble uses the combination of `key` and `index` to identify each row and
#' arranges each `key` as a separate group.
#' @param data a nested cubble object
#' @param col the list column to be expanded, `col` is required to be specified
#' if there are more than one list column and the list column name is not `ts`
#' @return a cubble object in the nested form
#' @rdname face_temporal
#' @export
#' @examples
#' climate_mel %>% face_temporal()
face_temporal <- function(data, col) {
  UseMethod("face_temporal")
}

#' @rdname face_temporal
#' @export
face_temporal.cubble_df <- function(data, col){
  NextMethod()
}

#' @rdname face_temporal
#' @export
face_temporal.temporal_cubble_df <- function(data, col){
  cli::cli_alert_info("The cubble is already in the long form")
  data
}

#' @rdname face_temporal
#' @export
face_temporal.spatial_cubble_df <- function(data, col){

  key <- syms(key_vars(data))
  if (length(key) == 2){
    cur_key <- key[key %in% names(data)][[1]]
  } else{
    cur_key <- key[[1]]
  }
  index <- index(data)
  coords <- coords(data)

  class_l <- length(class(data))
  class(data) <- class(data)[2:class_l]
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])

  col <- enquo(col)
  if (rlang::quo_is_missing(col)) col <- sym(get_listcol(data))

  # unnest the temporal variables
  if (is_tsibble) data$ts <- map(data$ts, tibble::as_tibble)
  out <- as_tibble(data) %>%  dplyr::select(!!cur_key, !!col) %>%  tidyr::unnest(c(!!col))

  # organise spatial variables into `spatial`
  class(data) <- class(data)[class(data) != "cubble_df"]
  if (".val" %in% colnames(data)) data <- as_tibble(data)
  spatial <- data %>%  select(-!!col)
  if (".val" %in% colnames(spatial)) spatial <- spatial %>%  tidyr::unnest(.data$.val)

  if (is_tsibble){
    out <- out %>% tsibble::as_tsibble(key = !!cur_key, index = index)
    tsibble_attr <- attributes(out)
  }

  new_temporal_cubble(
    out, key = map_chr(key, as_name), index = index, coords = coords, spatial = spatial
    )
}
