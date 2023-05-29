#' extract key related components in a cubble
#' @param x a cubble object
#' @rdname key
#' @importFrom tsibble key_vars key key_data
#' @export
#' @examples
#' library(tsibble)
#' key(climate_mel)
#' key_vars(climate_mel)
#' key_data(climate_mel)
key_vars.cubble_df <- function(x){
  x %@% "key"
}

#' @rdname key
#' @export
key.cubble_df <- function(x){
  syms(key_vars(x))
}

#' @rdname key
#' @export
key_data.cubble_df <- function(.data){
  # a tsibble object has the key attributes defined as the groups attribute
  # in a cubble
  .data %@% "groups"
}

#' extract the coords attribute from a cubble
#' @param data a cubble object
#' @export
#' @examples
#' coords(climate_mel)
coords <- function(data){
  is_cubble(data)
  data %@% "coords"
}


coords2strvec <- function(coords){

  # from
  # <quosure>
  # expr: ^c(long, lat)
  # env:  global
  # to
  # [1] "long" "lat"
  inherits(coords, "quosure")
  coords <- as.list(quo_get_expr(coords))[-1]
  unlist(map(coords, as_string))
}


#' extract the spatial component from a cubble
#' @param data a cubble object
#' @rdname spatial
#' @export
#' @examples
#' spatial(climate_mel)
spatial <- function(data){
  UseMethod("spatial")
}

#' @rdname spatial
#' @export
spatial.spatial_cubble_df <- function(data){
  class(data) <- setdiff(class(data), c("spatial_cubble_df","cubble_df"))
  data %>% select(-"ts")
}

#' @rdname spatial
#' @export
spatial.temporal_cubble_df <- function(data){
  data %@% "spatial"
}


#' extract index related components in a cubble
#' @param data a cubble object
#' @rdname index
#' @export
#' @examples
#' index(climate_mel)
#' index_var(climate_mel)
index <- function(data) {
  # not sure why tsibble doesn't have index() as an S3 method as key does
  is_cubble(data)
  sym(index_var(data))
}

#' @rdname index
#' @export
index_var <- function(data) {
  is_cubble(data)
  data %@% "index"
}
