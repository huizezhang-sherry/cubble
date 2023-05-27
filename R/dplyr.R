#' Access to dplyr verbs
#' @param data a cubble object
#' @param cols,i,template,... see [dplyr::dplyr_col_modify()], [dplyr::dplyr_row_slice()],
#' and [dplyr;:dplyr_reconstruct()]
#'
#' @references \link{https://dplyr.tidyverse.org/reference/dplyr_extending.html}
#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @rdname dplyr
#' @export
dplyr_col_modify.spatial_cubble_df <- function(data, cols) {


}


#' @rdname dplyr
#' @export
dplyr_col_modify.temporal_cubble_df <- function(data, cols) {

  # if ("tbl_ts" %in% class(data)){
  #   out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = key_vars(data)), cols)
  # } else{
  #   out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  # }

  # TODO: deal with ts??
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
dplyr_row_slice.spatial_cubble_df <- function(data, i, ...){
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
dplyr_row_slice.temporal_cubble_df <- function(data, i, ...){

  out <- NextMethod()

  dplyr_reconstruct(out, data)

  # out <- vec_slice(data, i)
  # key <- key_vars(data)
  # # update spatial as subsetting will change the number of row
  # if (is_long(data) && length(key) == 1){
  #   keep <- unique(out[[key]])
  #   spatial <- spatial(data) %>%  filter(!!sym(key) %in% keep)
  # }
  #
  # # TODO: update coords
  # # TODO: update index - same reason
  # dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
dplyr_reconstruct.spatial_cubble_df <- function(data, template) {

  new_spatial_cubble(
    as_tibble(data), key = key_vars(template),
    index = index_var(template), coords = coords(template)
  )
}


#' @rdname dplyr
#' @export
dplyr_reconstruct.temporal_cubble_df <- function(data, template) {

  key <- key_vars(template)
  index <-  index_var(template)
  spatial <- spatial(template)

  if (inherits(template, "tbl_ts")){

    data <- tsibble::as_tsibble(data, key = key, index = index)
  }


  new_temporal_cubble(
    data, key = key, index = index, coords = coords(template), spatial = spatial
  )
  # if (cubble_can_reconstruct(data, template)){
  #
  #   if(inherits(template, "sf")) {
  #
  #     attr(data, "sf_column") <- attr(template, "sf_column")
  #     attr(data, "agr") <- attr(template, "agr")
  #     class(data) <- c("sf", class(data))
  #   }
  #   if(inherits(template, "tbl_ts")){
  #     class(data) <- c("tbl_ts", class(data))
  #     data <- tsibble::build_tsibble(data, key = key_vars(template)[1])
  #   }
  #
  #   new_cubble(data,
  #              key = key_vars(template), index = index(template),
  #              coords = coords(template), spatial = spatial(template),
  #              form = form(template))
  # } else{
  #   # otherwise become a tibble
  #   x <- vctrs::new_data_frame(data)
  #   x <- tibble::new_tibble(x, nrow = nrow(x))
  #
  #   if ("tbl_ts" %in% class(template)){
  #     x <- tsibble::build_tsibble(x, key = key_vars(template)[1])
  #   }
  #
  #
  #   if ("sf" %in% class(template)) {
  #     x <- sf::st_as_sf(x, sf_column_name = attr(template, "sf_column"))
  #   }
  #
  #   return(x)
  #
  # }

}

cubble_can_reconstruct <- function(data, to){
  has_key <- has_index <- FALSE

  if (nrow(data) == 0) return(FALSE)
  # have key
  has_key <- any(key_vars(to) %in% names(data))

  # have index
  if (is_cubble_temporal(to)){
    has_index <- any(index(to) %in% names(data))
  } else if (is_cubble_spatial(to)){
    has_index <- any(index(to) %in% names(data[["ts"]][[1]]))
  }

  has_key && has_index

}

#' @export
summarise.cubble_df <- function(.data, ..., .by, .groups){
  data <- .data
  key <- key_vars(data)
  spatial <- spatial(data)
  origin <- data
  class(data) <- class(data)[class(data) != "cubble_df"]
  out <- NextMethod()

  dplyr_reconstruct(out, origin)

}


#' @export
# select.cubble_df <- function(data, ...){
#   out <- NextMethod()
#
#   dplyr_reconstruct(out, data)
# }


#' @export
#' @importFrom dplyr group_by_prepare
group_by.cubble_df <- function(.data, ..., .add, .drop){
  data <- .data
  key <- key_vars(data)
  groups <- dplyr::group_by_prepare(data, ..., .add = TRUE)
  group_var <- groups$group_names
  index <- setdiff(group_var,key)
  out <- groups$data

  new_cubble(out,
             key = c(key, index), index = index, coords = coords(data),
             spatial = spatial(data))
}

#' @export
ungroup.cubble_df <- function(x, ...){
  data <- x
  ungroup_var <- names(enquos(..., .named = TRUE))

  if (!all(ungroup_var %in% names(data))){
    problem <- ungroup_var[!ungroup_var %in% names(data)]
    cli::cli_abort("the ungroup variable: {problem} is not found in the data")
  }

  if (key_vars(data)[1] %in% ungroup_var){
    cli::cli_abort("Can't ungroup the spatio identifier!")
  }


  updated_group_var <- setdiff(key_vars(data), ungroup_var)

  new_cubble(data,
             key = updated_group_var, index = index(data), coords = coords(data),
             spatial = spatial(data))
}

#' @export
rename.cubble_df <- function(.data, ...){
  out <- .data %>%  as_tibble() %>%  rename(...)
  dplyr_reconstruct(out, .data)
}
