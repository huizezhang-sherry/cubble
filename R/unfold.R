#' Move spatial variables into the long form
#'
#' Some spatio-temporal transformation, i.e. glyph maps, uses both spatial
#' and temporal variables. `unfold()` allows you to temporarily moves spatial
#' variables into the long form for these transformations.
#'
#' @param data a long cubble object
#' @param ... spatial variables to move into the long form
#' @return a cubble object in the long form
#' @examples
#' climate_mel %>% face_temporal() %>%  unfold(long, lat)
#'
#' @rdname unfold
#' @export
unfold <- function(data, ...) {
  UseMethod("unfold")
}

#' @rdname unfold
#' @export
unfold.cubble_df <- function(data, ...){
  NextMethod()
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
  #browser()
  dots <- enquos(..., .named = TRUE)
  sp <- spatial(data)
  key <- key_vars(data)
  index <- index(data)
  coords <- coords(data)
  in_spatial <- map_lgl(names(dots), ~.x %in% names(sp))
  if (!all(in_spatial)){
    cli::cli_inform(
      "{.code {names(dots)[!in_spatial]}} does not exist as a spaital variable. No migration")
  }

  to_join <- sp %>% as_tibble() |> select(key_vars(data), names(dots)[in_spatial])
  out <- as_tibble(data) |> left_join(to_join, by = key)

  if (nrow(out) != nrow(data)){
    var <- names(dots)
    cli::cli_alert_warning(
      "The key and unfoldd variable{?s} {.field {var}} are not one-to-one."
    )
  }

  if (inherits(data, "tbl_ts")){
    index <- data %@% index
  } else{
    index <- as_name(index)
  }

  new_temporal_cubble(
    out, key = key, index = index, coords = coords, spatial = sp
  )
}
