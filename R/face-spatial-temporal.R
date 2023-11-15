#' Pivot a cubble object between the nested/long (spatial/temporal) form
#'
#' While `face_temporal()` switches a cubble object into a long cubble,
#' suitable for temporal operations, `face_spatial()` turns a long cubble back
#' into a nest cubble for spatial operations. The two operations are exact
#' inverse.
#' @param data a cubble object
#' @param col a character (or a symbol), the list column to be expanded,
#' `col` is required to be specified if there are more than one list column
#' and the list column name is not `ts`.
#' @return a cubble object
#' @rdname face
#' @export
#' @examples
#' cb_long <- climate_mel |> face_temporal()
#' cb_back <- cb_long |> face_spatial()
#' identical(climate_mel, cb_back)
face_temporal <- function(data, col) {
  UseMethod("face_temporal")
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
  index <- index_var(data)
  coords <- coords(data)
  spatial <- spatial(data)

  class_l <- length(class(data))
  class(data) <- class(data)[2:class_l]
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])

  col <- enquo(col)
  if (rlang::quo_is_missing(col)) col <- sym(get_listcol(data))

  # unnest the temporal variables
  if (is_tsibble) data$ts <- map(data$ts, tibble::as_tibble)
  out <- as_tibble(data) %>%
    dplyr::select(!!cur_key, !!col) %>%
    tidyr::unnest(c(!!col))

  # organise spatial variables into `spatial`
  class(data) <- class(data)[class(data) != "cubble_df"]

  if (is_tsibble){
    out <- out |> tsibble::as_tsibble(key = !!cur_key, index = index)
    tsibble_attr <- attributes(out)
    index <- out %@% "index"
  }

  new_temporal_cubble(
    out, key = map_chr(key, as_name), index = index,
    coords = coords, spatial = spatial
  )
}

#' @rdname face
#' @export
face_spatial <- function(data) {
  UseMethod("face_spatial")
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

  tvars <- colnames(data)[colnames(data) != key_name]
  tvars <- tvars[!tvars %in% colnames(spatial)]
  unfoldd_var <- intersect(names(data), names(spatial)) %>%
    setdiff(key_name)

  class(data) <- setdiff(class(data), cb_temporal_cls)
  temporal <- data |> remove_attrs() |> tidyr::nest(ts = -key_name)
  out <- spatial |> dplyr::left_join(temporal, by = key_name)

  new_spatial_cubble(
    out, key = key_name, index = index, coords = coords
    )
}

remove_attrs <- function(data){
  UseMethod("remove_attrs")
}

remove_attrs.default <- function(data){
  as_tibble(data)
}

remove_attrs.grouped_df <- function(data){
  attr(data, "key") <- NULL
  attr(data, "index") <- NULL
  attr(data, "coords") <- NULL
  attr(data, "spatial") <- NULL
  data
}

remove_attrs.rowwise_df <- remove_attrs.grouped_df

remove_attrs.tbl_df <- function(data){
  attr(data, "key") <- NULL
  attr(data, "index") <- NULL
  attr(data, "coords") <- NULL
  attr(data, "spatial") <- NULL
  attr(data, "groups") <- NULL
  data
}


remove_attrs.tbl_ts <- function(data){
  attr(data, "coords") <- NULL
  attr(data, "spatial") <- NULL
  attr(data, "groups") <- NULL
  data
}
