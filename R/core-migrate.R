#' Move spatial variables into the long form
#'
#' Some spatio-temporal transformation, i.e. glyph maps, uses both spatial
#' and temporal variables. `unfold()` allows you to temporarily moves spatial
#' variables into the long form for these transformations.
#'
#' @param data a long cubble object
#' @param ... spatial variables to move into the long form
#' @examples
#' cb <- climate_flat |>
#'   as_cubble(key = id, index = date, coords = c(long, lat)) |>
#'   face_temporal()
#'
#' # unfold long and lat
#' cb_mig <- cb |> unfold(long, lat)
#'
#' # migration is not memorised by cubble:
#' # if you switch to the nested cubble and then switch back,
#' # long and lat will not be preserved
#' cb_mig |> face_spatial() |> face_temporal()
#'
#' @export
#' @rdname cubble-verb
unfold <- function(data, ...){
  dots <- enquos(..., .named = TRUE)
  test_cubble(data)
  test_long(data)
  sp <- spatial(data)
  key <- key_vars(data)

  in_spatial <- map_lgl(names(dots), ~.x %in% names(sp))
  if (!all(in_spatial)){
    cli::cli_inform(
      "{.code {names(dots)[!in_spatial]}} does not exist as a spaital variable. No migration")
  }

  to_join <- sp |> select(key_vars(data), names(dots)[in_spatial]) |> dplyr::distinct()
  out <- data |> left_join(to_join, by = key)

  if (nrow(out) != nrow(data)){
    var <- names(dots)
    cli::cli_alert_warning(
      "The key and unfoldd variable{?s} {.field {var}} are not one-to-one."
    )
  }
  out
}
