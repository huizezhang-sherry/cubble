#' extract key related components in a cubble
#' @param data a cubble object
#' @rdname key
#' @importFrom tsibble key_vars key key_data
#' @export
#' @examples
#' key(climate_mel)
#' key_vars(climate_mel)
#' key_data(climate_mel)
key_vars.cubble_df <- function(data){
  data %@% "key"
}

#' @rdname key
#' @export
key.cubble_df <- function(data){
  syms(key_vars(data))
}

#' @rdname key
#' @export
key_data.cubble_df <- function(data){
  # a tsibble object has the key attributes defined as the groups attribute
  # in a cubble
  data %@% "groups"
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
