#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {


  key <- key_vars(data)

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = !!key), cols)
  } else{
    out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  }

  # if ("tbl_ts" %in% class(out$ts[[1]])){
  #   class(out) <- c(class(out), "tbl_ts")
  #   dt <- out$ts[[1]]
  #   tsibble_attr <- list(index = dt %@% "index",
  #                        index2 = dt %@% "index2",
  #                        interval = dt %@% "interval",
  #                        key = dt %@% "key")
  #
  # }
  # update leaves
  # long form shouldn't make change on the meta but list-col form may (i.e. mutate)
  spatial <- new_spatial(data)

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             row_id = row_id(data), spatial = spatial, form = determine_form(out),
             tsibble_attr = tsibble_attr)
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...){

  out <- vec_slice(data, i)
  key <- key_vars(data)

  spatial <- new_spatial(data)

  if (determine_form(data) == "long"){
    keep <- unique(out[[key]])
    spatial <- spatial %>% filter(!!sym(key) %in% keep)

  }

  if ("tbl_ts" %in% class(data)){
    out <- tsibble::build_tsibble(out, key = key_vars(data)[1])
  }
  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             row_id = row_id(data), spatial = spatial, form = determine_form(out))
}

#' @export
dplyr_reconstruct.cubble_df <- function(data, template) {

  form <- determine_form(template)
  key <- key_vars(template)[1]

  # if (form == "long"){
  #   leaves_data <-  leaves(template)
  # } else{
  #   data_var <- data[, find_invariant(data, !!key)$invariant] %>% names()
  #   old_leaves <- leaves(template) %>% names()
  #   new_leaves <- data_var[data_var %in% old_leaves]
  #   cols <- unique(c(key, new_leaves))
  #   leaves_data <- leaves(template)[cols]
  # }
  spatial <- new_spatial(template)


  new_cubble(data,
             key = key, index = index(template), coords = coords(template),
             row_id = row_id(template), spatial = spatial,
             form = determine_form(template))
}

#' @export
summarise.cubble_df <- function(data, ...){
  key <- key_vars(data)
  spatial <- new_spatial(data)
  out <- NextMethod("summarise")

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             row_id = row_id(data),
             spatial = spatial, form = determine_form(out))
}

#' @export
left_join.cubble_df <- function(data1, data2, by = NULL, ...){

  if (!is_cubble(data1)){
    abort("data1 needs to be a cubble object")
  }

  if (form(data1) == "long" && any(by == key_vars(data1))){
    inform("Joining variable(s) being invariant to the group variable ...")
  }

  if (is_null(by)){
    by <- intersect(names(data1), names(data2))
  }
  out <- NextMethod("left_join")


  if (is_cubble(data2)){
    # key <- c(key_vars(data1), key_vars(data2))
    # index <- index(data1)
    # coords <- list(coords(data1), coords(data2))
    # leaves <- list(leaves(data1), leaves(data2))
    #
    # out <- new_cubble(out,
    #            key = key, index = index, coords = coords,
    #            leaves = leaves, form = determine_form(out))
  } else{
    out <- dplyr_reconstruct(out, data1)
  }

  out
}

#' @export
select.cubble_df <- function(data, ...){
  out <- NextMethod("select")

  dplyr_reconstruct(out, data)
}


#' @export
group_by.cubble_df <- function(data, ...){
  key <- key_vars(data)
  groups <- dplyr:::group_by_prepare(data, ..., .add = TRUE, caller_env = caller_env())
  group_var <- groups$group_names
  index <- setdiff(group_var,key)
  out <- groups$data

  new_cubble(out,
             key = key, index = index, coords = coords(data),
             row_id = row_id(data), spatial = spatial(data), form = determine_form(data))
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
             row_id = row_id(data), spatial = spatial(data), form = determine_form(data))
}

#' @export
rename.cubble_df <- function(.data, ...){
  out <- .data %>% as_tibble() %>% rename(...)
  dplyr_reconstruct(out, .data)
}


