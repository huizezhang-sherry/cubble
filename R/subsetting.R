#' @export
`[.cubble_df` <- function(data, i, j, drop = FALSE){

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

  if(!is.null(i)){
    out <- vec_slice(data, i)
  }

  if(!is.null(j)){
    out <- vctrs::vec_data(out)
    # check column includes key, index, and coords

    out <- out[,unlist(j)]
  }

 dplyr_reconstruct(out, data)

}

#' @export
`names<-.cubble_df` <- function(x, value){

  out <- data <- x
  out %@% "names" <- value

  key_idx <- which(names(data) == key_vars(data)[1])
  new_key <- sym(names(out)[key_idx])
  long_idx <- which(names(data) == coord_x(data))
  new_long <- names(out)[long_idx]
  lat_idx <- which(names(data) == coord_y(data))
  new_lat <- names(out)[lat_idx]

  #leaves_data <- new_leaves(out, !!new_key)
  spatial = spatial(out)

  new_cubble(out,
             key = as_name(new_key), index = index(data), coords = c(new_long, new_lat),
             row_id = row_id(data), spatial = spatial, form = determine_form(out))

}
