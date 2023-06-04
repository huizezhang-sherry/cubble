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


