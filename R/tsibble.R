#' Promote the temporal component in cubble to an tsibble object after creation
#' @param x object of class \code{temporal_cubble_df}
#' @param ... not used
#' @export
#' @seealso [make_spatial_sf]
#' @examples
#' climate_mel %>% face_temporal() %>% make_temporal_tsibble()
make_temporal_tsibble <- function(x, ...){

  stopifnot(is_cubble_temporal(x))
  key <- key_vars(x)
  index <- index_var(x)
  coords <- coords(x)
  spatial <- spatial(x)
  x <- as_tibble(x) %>% remove_attrs()
  out <- tsibble::as_tsibble(x, key = key, index = index)
  new_temporal_cubble(
    out, key = key_vars(out), index = index_var(out), coords = coords, spatial = spatial)

}


#' `tsibble` functions implemented in cubble
#' @param .data,...,.full,.start,.end see \code{tsibble} documentation
#' @importFrom tsibble fill_gaps scan_gaps
#' @return a cubble object
#' @rdname tsibble
#' @export
#' @examples
#' library(tsibble)
#' climate_aus %>% face_temporal() %>% fill_gaps()
#' climate_aus %>% face_temporal() %>% scan_gaps()
fill_gaps.temporal_cubble_df <- function(.data, ..., .full = FALSE,
                                .start = NULL, .end = NULL) {

  stopifnot(is_cubble_temporal(.data))
  key <- key_vars(.data)
  index <-  index_var(.data)
  coords <- coords(.data)
  spatial <- spatial(.data)
  .data <- as_tsibble(.data, key = key , index = index)
  res <- fill_gaps(.data, ...)

  new_temporal_cubble(
    res, key = key, index = index, coords = coords(.data), spatial = spatial)
}

#' @rdname tsibble
#' @export
scan_gaps.temporal_cubble_df <- function(.data, ...){
  stopifnot(is_cubble_temporal(.data))
  key <- key_vars(.data)
  index <-  index_var(.data)
  coords <- coords(.data)
  spatial <- spatial(.data)
  .data <- as_tsibble(.data, key = key , index = index)
  res <- scan_gaps(.data, ...)

  # why it makes more sense to output the result into the original class,
  # it is better with a plain tibble summary?
  new_temporal_cubble(
    res, key = key, index = index, coords = coords(.data), spatial = spatial)


}
