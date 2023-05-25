#' Switch a cubble object into the nested form
#'
#' `face_spatial()` turns a long cubble back into a nest cubble and can be seen as
#' the inverse operation of \code{face_temporal()}.
#' The nested cubble identifies each row by `key` and is suitable
#' for operations whose output doesn't involve a time index.
#'
#' @param data a long cubble object
#' @return a cubble object in the nested form
#' @rdname face_spatial
#' @export
#' @examples
#' cb_long <- climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   face_temporal()
#'
#' cb_long %>%  face_spatial()
face_spatial <- function(data) {
  UseMethod("face_spatial")
}


#' @rdname face_spatial
#' @export
face_spatial.cubble_df <- function(data) {
  NextMethod()
}

#' @rdname face_spatial
#' @export
face_spatial.spatial_cubble_df <- function(data) {
  cli::cli_alert_info("The cubble is already in the nested form")
  data

}

#' @rdname face_spatial
#' @export
face_spatial.temporal_cubble_df <- function(data) {
  # will only keep the first grouping variable if more than one
  key <- rlang::syms(key_vars(data))
  key_name <- map_chr(key, rlang::as_name)
  index <- index(data)
  coords <- coords(data)

  spatial <- spatial(data)
  if (length(key) == 1){
    tvars <- colnames(data)[colnames(data) != key_name]
    tvars <- tvars[!tvars %in% colnames(spatial)]

    unfoldd_var <- intersect(names(data), names(spatial)) %>%
      setdiff(key_name)

    temporal <- as_tibble(data) %>% tidyr::nest(ts = -key_name)

    out <- spatial %>%  dplyr::left_join(temporal, by = key_name)

  } else if (length(key) == 2){
    spatial <- spatial %>%  tidyr::nest(.val = -key_name[1])
    temporal <-data %>%  tidyr::nest(ts = -key_name[1])
    out <- left_join(spatial, temporal, by = key_name[1])
  }

  new_spatial_cubble(
    out, key = key_name, index = index, coords = coords
    )
}

