#' Promote the spatial/ temporal component in cubble to an sf/ tsibble object after creation
#' @param data A cubble object
#' @param ... not used
#' @rdname make
#' @export
#' @examples
#' climate_mel %>% make_spatial_sf()
#' climate_mel %>% face_temporal() %>% make_temporal_tsibble()
make_temporal_tsibble <- function(data, ...){

  stopifnot(is_cubble_temporal(data))
  key <- key_vars(data)
  index <- index_var(data)
  coords <- coords(data)
  spatial <- spatial(data)
  data <- as_tibble(data) %>% remove_attrs()
  out <- tsibble::as_tsibble(data, key = key, index = index)
  new_temporal_cubble(
    out, key = key_vars(out), index = index_var(out), coords = coords, spatial = spatial)

}
