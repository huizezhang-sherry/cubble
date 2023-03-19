cubble_cast <- function(x, to, ..., x_arg = "", to_arg = ""){
  out <- vctrs::tib_cast(x, to, ..., x_arg = "", to_arg = "")

  if (!compatible_to_combine(x, to)){
    vctrs::stop_incompatible_cast(
      x, to,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Incompatible objects: check your key, index, and coords"
    )
  }

  key <- key_vars(x) %||% key_vars(to)
  index <- index(x) %||% index(to)
  coords <- coords(x) %||% coords(to)
  spatial <- spatial(x) %||% spatial(to)
  form <- form(x) %||% form(to)
  new_cubble(out,
             key = key, index = index, coords = coords,
             spatial = spatial, form = form)

}

cubble_ptype2 <- function(x, y, ..., x_arg = "", to_arg = ""){
  out <- vctrs::tib_ptype2(x, y, ..., x_arg = "", to_arg = "")

  if (!compatible_to_combine(x, y)){
    vctrs::stop_incompatible_cast(
      x, y,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Incompatible objects: check your key, index, and coords"
    )
  }

  key <- key_vars(x) %||% key_vars(y)
  index <- index(x) %||% index(y)
  coords <- coords(x) %||% coords(y)
  spatial <- spatial(x) %||% spatial(y)
  form <- form(x) %||% form(y)
  new_cubble(out,
             key = key, index = index, coords = coords,
             spatial = spatial, form = form)
}

compatible_to_combine <- function(x,y){
  has_compatible_key && has_compatible_coords && has_compatible_index
}

# can write these as function factory
has_compatible_key <- function(x, y){
  key_x <- if (is_cubble(x)) key_vars(x) else NULL
  key_y <- if (is_cubble(y)) key_vars(y) else NULL

  identical(key_x, key_y)
}

has_compatible_coords <- function(x, y){
  coords_x <- if (is_cubble(x)) coords(x) else NULL
  coords_y <- if (is_cubble(y)) coords(y) else NULL

  identical(coords_x, coords_y)
}

has_compatible_index <- function(x, y){
  index_x <- if (is_cubble(x)) index(x) else NULL
  index_y <- if (is_cubble(y)) index(y) else NULL

  identical(index_x, index_y)
}

#' @export
vec_ptype2.cubble_df.cubble_df <- function(x, y, ...) cubble_ptype2(x, y, ...)
#' @export
vec_ptype2.cubble_df.tbl_df  <- function(x, y, ...) cubble_ptype2(x, y, ...)
#' @export
vec_ptype2.tbl_df.cubble_df <- function(x, y, ...) cubble_ptype2(x, y, ...)
#' @export
vec_ptype2.data.frame.cubble_df <- function(x, y, ...) cubble_ptype2(x, y, ...)
#' @export
vec_ptype2.cubble_df.data.frame <- function(x, y, ...) cubble_ptype2(x, y, ...)

#' @export
vec_cast.cubble_df.cubble_df <- function(x, to, ...) cubble_cast(x, to, ...)
#' @export
vec_cast.cubble_df.tbl_df <- function(x, to, ...) cubble_cast(x, to, ...)
#' @export
vec_cast.tbl_df.cubble_df <- function(x, to, ...) vctrs::tib_cast(x, to, ...)
#' @export
vec_cast.data.frame.cubble_df <- function(x, to, ...) vctrs::df_cast(x, to, ...)
#' @export
vec_cast.cubble_df.data.frame <- function(x, to, ..., x_arg, to_arg, call) cubble_cast(x, to, ...)

#' @export
vec_proxy.cubble_df <- function(x, ...) x
