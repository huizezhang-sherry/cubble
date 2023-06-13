#' Augment spatial component into the long (temporal) form
#'
#' Some spatio-temporal transformation, i.e. glyph maps, uses both spatial
#' and temporal variables. `unfold()` allows you to temporarily moves spatial
#' variables into the long form for these transformations.
#'
#' @param data a long cubble object
#' @param ... spatial variables to move into the long form,
#' support tidyselect syntax
#' @return a cubble object in the long form
#' @examples
#' climate_mel %>% face_temporal() %>% unfold(long, lat)
#' climate_mel %>% face_temporal() %>% unfold(dplyr::starts_with("l"))
#' @rdname unfold
#' @export
unfold <- function(data, ...) {
  UseMethod("unfold")
}

#' @rdname unfold
#' @export
unfold.spatial_cubble_df <- function(data, ...){
  cli::cli_abort("{.fn unfold} currently can only be used on a long form cubble
                 (class {.code spatial_cubble_df})")

}

#' @rdname unfold
#' @export
unfold.temporal_cubble_df <- function(data, ...){
  sp <- spatial(data)
  key <- key_vars(data)
  index <- index(data)
  coords <- coords(data)

  to_join <- sp %>% as_tibble() |> select(c(key_vars(data), ...))
  out <- as_tibble(data) |> left_join(to_join, by = key)

  if (nrow(out) != nrow(data)){
    var <- names(dots)
    cli::cli_alert_warning(
      "The key and unfoldd variable{?s} {.field {var}} are not one-to-one."
    )
  }

  if (is_tsibble(data)){
    index <- data %@% index
  } else{
    index <- as_name(index)
  }

  new_temporal_cubble(
    out, key = key, index = index, coords = coords, spatial = sp
  )
}
