#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = key_vars(data)), cols)
  } else{
    out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  }

  # TODO: deal with ts??
  dplyr_reconstruct(out, data)
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...){

  out <- vec_slice(data, i)
  key <- key_vars(data)
  # update spatial as subsetting will change the number of row
  if (is_long(data)){
    keep <- unique(out[[key]])
    spatial <- spatial(data) %>%  filter(!!sym(key) %in% keep)
  }

  # TODO: update coords

  # TODO: update index - same reason



  if ("tbl_ts" %in% class(data)){
    out <- tsibble::build_tsibble(out, key = key_vars(data)[1])
  }

  dplyr_reconstruct(out, data)
}

#' @export
dplyr_reconstruct.cubble_df <- function(data, template) {


  if (cubble_can_reconstrcut(data, template)){
    new_cubble(data,
               key = key_vars(template), index = index(template),
               coords = coords(template), spatial = spatial(template),
               form = form(template))
  } else{
    # otherwise become a tibble
    x <- vctrs::new_data_frame(data)
    tibble::new_tibble(x, nrow = nrow(x))
  }

}

cubble_can_reconstrcut <- function(data, to){
  has_key <- has_index <- FALSE

  # have key
  has_key <- any(key_vars(to) %in% names(data))

  # have index
  if (is_long(to)){
    has_index <- any(index(to) %in% names(data))
  } else if (is_nested(to)){
    has_index <- any(index(to) %in% names(data[["ts"]][[1]]))
  }

  has_key && has_index

}

#' @export
summarise.cubble_df <- function(data, ...){
  key <- key_vars(data)
  spatial <- spatial(data)
  out <- NextMethod("summarise")

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             spatial = spatial, form = determine_form(out))
}


#' @export
select.cubble_df <- function(data, ...){
  out <- NextMethod("select")

  dplyr_reconstruct(out, data)
}


#' @export
#' @importFrom dplyr group_by_prepare
group_by.cubble_df <- function(data, ...){
  key <- key_vars(data)
  groups <- dplyr::group_by_prepare(data, ..., .add = TRUE, caller_env = caller_env())
  group_var <- groups$group_names
  index <- setdiff(group_var,key)
  out <- groups$data

  new_cubble(out,
             key = c(key, index), index = index, coords = coords(data),
             spatial = spatial(data), form = determine_form(data))
}

#' @export
ungroup.cubble_df <- function(data, ...){
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
             spatial = spatial(data), form = determine_form(data))
}

#' @export
rename.cubble_df <- function(.data, ...){
  out <- .data %>%  as_tibble() %>%  rename(...)
  dplyr_reconstruct(out, .data)
}


