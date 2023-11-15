#' Accessors to a cubble object
#' @details
#' For nested cubbles, `[` will return a cubble object if the \code{key}
#' variable, the\code{coords} variables, and the \code{ts} column all present.
#' If the cubble object is also an sf object, the sticky select behavior on
#' the sf column will preserve. For long cubbles, `[` will return a cubble
#' object if the \code{key} and \code{index} variable both present.
#' When a cubble can't be created and the data is not an sf class,
#' `[` will always return a tibble, even with single index selection.
#'
#' @param data an object of class \code{spatial_cubble_df} or
#' \code{temporal_cubble_df}
#' @param i,j row and column selector
#' @inheritParams base::`[.data.frame`
#' @examples
#' climate_mel[c(1:3, 7)] # a nested cubble
#' make_spatial_sf(climate_mel)[1:3] # an sf
#'
#' long <- climate_mel |> face_temporal()
#' long[1:3] # a long cubble
#'
#' climate_mel[1:3] # tibble
#' long[2:5] # tibble
#' climate_mel[1] # still tibble
#' long[1] # and still tibble
#' @rdname accessors
#' @export
`[.spatial_cubble_df` <- function(data, i, j, drop = FALSE){

  # https://github.com/tidyverts/tsibble/blob/main/R/subset.R
  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
  }

  out <- data
  if(!is.null(i)) out <- vec_slice(data, i)
  if(!is.null(j)){
    class(out) <- setdiff(class(out), cb_spatial_cls)
    out <- out[,unlist(j)]
  }

  cb_cols <- c(key_vars(data), coords(data), "ts")

  if (all(cb_cols %in% names(out))){
    dplyr_reconstruct(out, data)
  } else{
    out
  }

}

#' @rdname accessors
#' @export
`[.temporal_cubble_df` <- function(data, i, j, drop = FALSE){

  #browser()
  # https://github.com/tidyverts/tsibble/blob/main/R/subset.R
  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
  }

  out <- data
  if(!is.null(i)) out <- vec_slice(data, i)
  if(!is.null(j)){
    class(out) <- setdiff(class(out), cb_temporal_cls)
    out <- out[,unlist(j)]
  }

  cb_cols <- c(key_vars(data), index_var(data))
  if (all(cb_cols %in% names(out))){
    dplyr_reconstruct(out, data)
  } else{
    out
  }

}

#' @rdname accessors
#' @export
`names<-.spatial_cubble_df`<- function(x, value){
  out <- `names<-`(as_tibble(x), value)
  new_spatial_cubble(
    out, key = value[which(names(x) == key_vars(x))],
    index = index_var(x),
    coords = value[which(names(x) %in% coords(x))]
  )
}

#' @rdname accessors
#' @export
`names<-.temporal_cubble_df`<- function(x, value){
  out <- `names<-`(as_tibble(x), value)
  spatial <- spatial(x)
  names(spatial)[which(key_vars(x)==names(spatial))] <-
    value[which(names(x) == key_vars(x))]

  new_temporal_cubble(
    out, key = value[which(names(x) == key_vars(x))],
    index = value[which(names(x) == index_var(x))],
    coords = coords(x), spatial = spatial
  )
}


#' @rdname accessors
#' @export
`[[<-.cubble_df` <- function(x, i, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}
