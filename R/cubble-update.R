#' update
#' when the data is already a cubble object but need update on attributes
#' @param data,key,index,coords,spatial,... skdflsjkd
#' @rdname update
#' @export
update_cubble <- function(data, key, index, coords, ...){

   UseMethod("update_cubble")
}

#' @rdname update
#' @export
update_cubble.spatial_cubble_df <- function(data, key = NULL, index = NULL, coords = NULL, ...){
  is_cubble(data)
  key <- key_vars(data)
  index <- index_var(data)
  coords <- coords(data)

  data %>% new_spatial_cubble(key = key, index = index, coords = coords)

}

#' @rdname update
#' @export
update_cubble.temporal_cubble_df <- function(data, key = NULL, index = NULL, coords = NULL, spatial = NULL, ...){

  key <- key_vars(data)
  index <- index_var(data)
  coords <- coords(data)

  spatial <- spatial(data)
  data %>% new_temporal_cubble(key = key, index = index, coords = coords, spatial = spatial)
}
