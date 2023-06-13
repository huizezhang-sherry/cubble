#' Extract cubble attributes
#' @param x,.data,data a cubble object
#' @rdname attr
#' @importFrom tsibble key_vars key key_data
#' @export
#' @examples
#' library(tsibble)
#' key(climate_mel)
#' key_vars(climate_mel)
#' key_data(climate_mel)
#' cubble::index(climate_mel)
#' cubble::index_var(climate_mel)
#' coords(climate_mel)
#' spatial(climate_mel)
key_vars.cubble_df <- function(x){
  keys <- key_data(x)
  head(names(keys), -1L)
}

#' @rdname attr
#' @export
key.cubble_df <- function(x){
  syms(key_vars(x))
}

#' @rdname attr
#' @export
key_data.cubble_df <- function(.data){
  .data %@% "key"
}

#' @rdname attr
#' @export
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

#' @rdname attr
#' @export
spatial <- function(data){
  UseMethod("spatial")
}

#' @rdname attr
#' @export
spatial.spatial_cubble_df <- function(data){
  class(data) <- setdiff(class(data), c("spatial_cubble_df","cubble_df"))
  data %>% select(-"ts") %>% remove_attrs()
}

#' @rdname attr
#' @export
spatial.temporal_cubble_df <- function(data){
  remove_attrs(data %@% "spatial")
}

#' @rdname attr
#' @export
index <- function(data) {
  # not sure why tsibble doesn't have index() as an S3 method as key does
  is_cubble(data)
  sym(index_var(data))
}

#' @rdname attr
#' @export
index_var <- function(data) {
  is_cubble(data)
  data %@% "index"
}
