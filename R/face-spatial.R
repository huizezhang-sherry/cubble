#' Switch a cubble object between the nested (spatial) form and the long (temporal) form
#'
#' While `face_temporal()` switches a cubble object into a long cubble,
#' suitable for temporal operations, `face_spatial()` turns a long cubble back
#' into a nest cubble for spatial operations. The two operations are exact inverse.
#' @param data a cubble object
#' @param col the list column to be expanded, `col` is required to be specified
#' if there are more than one list column and the list column name is not `ts`
#' @return a cubble object
#' @rdname face
#' @export
#' @examples
#' cb_long <- climate_mel %>% face_temporal()
#' cb_back <- cb_long %>% face_spatial()
#' identical(climate_mel, cb_back)
face_spatial <- function(data) {
  UseMethod("face_spatial")
}

#' @rdname face
#' @export
face_spatial.cubble_df <- function(data) {
  NextMethod()
}

#' @rdname face
#' @export
face_spatial.spatial_cubble_df <- function(data) {
  cli::cli_alert_info("The cubble is already in the nested form")
  data

}

#' @rdname face
#' @export
face_spatial.temporal_cubble_df <- function(data) {
  # will only keep the first grouping variable if more than one
  key <- rlang::syms(key_vars(data))
  key_name <- map_chr(key, rlang::as_name)
  index <- data %@% "index"
  coords <- coords(data)

  spatial <- spatial(data)
  if (length(key) == 1){
    tvars <- colnames(data)[colnames(data) != key_name]
    tvars <- tvars[!tvars %in% colnames(spatial)]

    unfoldd_var <- intersect(names(data), names(spatial)) %>%
      setdiff(key_name)

    class(data) <- setdiff(class(data), cb_temporal_cls)
    temporal <- data %>% tidyr::nest(ts = -key_name)

    out <- spatial %>% dplyr::left_join(temporal, by = key_name) %>% rowwise()

  } else if (length(key) == 2){
    spatial <- spatial %>%  tidyr::nest(.val = -key_name[1])
    temporal <-data %>%  tidyr::nest(ts = -key_name[1])
    out <- left_join(spatial, temporal, by = key_name[1])
  }

  new_spatial_cubble(
    out, key = key_name, index = index, coords = coords
    )
}

remove_attrs <- function(data){
  attr(data, "key") <- NULL
  attr(data, "index") <- NULL
  attr(data, "coords") <- NULL
  attr(data, "spatial") <- NULL
  attr(data, "groups") <- NULL
  data
}
