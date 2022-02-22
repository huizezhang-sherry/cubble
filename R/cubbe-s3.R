#' Constructor for the cubble class
#'
#' @param ... a list object to create new cubble
#' @param data the object to be created or tested as cubble
#' @param key the spatial identifier
#' @param index the time identifier
#' @param coords the coordinates that characterise the spatial dimension
#' @rdname cubble-class
#' @export
cubble <- function(..., key, index, coords) {
  data <- tibble::tibble(!!!list2(...))
  key <- enquo(key)
  new_cubble(data,
             key = as_name(key), index = as_name(index), coords = coords,
             row_id = row_id, spatial = NULL, form = "nested")

}

new_cubble <- function(data, key, index, coords, spatial, form, row_id, tsibble_attr = NULL) {
  #browser()
  key_data <- group_data(dplyr::grouped_df(data, vars = unlist(map(key, as_name))))

  all_cols <- names(data)

  if (form == "nested"){
    unique_key <- unique(data[[key]]) %>% length()
    if (unique_key != nrow(data) & !is.null(row_id(data))){
      data <- data %>% ungroup() %>% rearrange_index(key = key, old_key = row_id)
    }
  }

  if (length(coords) == 1){
      others <- all_cols[!all_cols %in% c(key, coords, "ts")]
      ordered <- c(key, coords, others, "ts")
      data <- data %>% select(ordered)
  }

  attr <- list(x = data,
               groups = key_data, index = index, row_id = row_id,
               spatial = spatial, coords = coords, form = form,
               class = "cubble_df") %>%
    Filter(f = length)

  # check column ts present,
  if (form == "nested"){
    # also need to check
    # * ts is a list column
    # * ts column contain index
    if (!"ts" %in% names(data)){
      cli::cli_abort("data need to contain a {.code ts} column to construct a cubble")
    }

  }

  #tsibble_attr <- NULL
  if ("tbl_ts" %in% class(data)){
    attr$class <- c(attr$class, "tbl_ts")
    attr <- c(attr, tsibble_attr)
  }


  if (form == "nested"){
    names(attr)[1] <- "data"
    out <- rlang::exec("new_rowwise_df", !!!attr)
  } else if (form == "long"){
    out <- rlang::exec("new_grouped_df", !!!attr)
  }

  out
}


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
