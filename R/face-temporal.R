#' @rdname face
#' @export
face_temporal <- function(data, col) {
  UseMethod("face_temporal")
}

#' @rdname face
#' @export
face_temporal.cubble_df <- function(data, col){
  NextMethod()
}

#' @rdname face
#' @export
face_temporal.temporal_cubble_df <- function(data, col){
  cli::cli_alert_info("The cubble is already in the long form")
  data
}

#' @rdname face
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
