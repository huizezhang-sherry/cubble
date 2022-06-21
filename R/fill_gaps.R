#' `tsibble` methods implemented in cubble
#'
#' See \code{\link[tsibble]{fill_gaps}}
#' @param .data,...,.full,.start,.end see tsibble documentation
#' @return a cubble object
#' @export
fill_gaps.cubble_df <- function(.data, ..., .full = FALSE,
                                .start = NULL, .end = NULL) {

  temporal <- NextMethod("fill_gaps")
  spatial <- spatial(.data)

  new_cubble(temporal,
             key = key_vars(.data), index = index(.data),
             coords = coords(.data), spatial = spatial, form = "long")
}
