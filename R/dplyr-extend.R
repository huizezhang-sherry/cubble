#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {
  group_vars <- group_vars(data)

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = !!group_vars), cols)
  } else{
    out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  }

  # update meta data:
  # long form shouldn't make change on the meta but list-col form may (i.e. mutate)
  form <- determine_form(data)
  if (form == "list-col"){
    key <- group_vars(data)
    meta_data <- out[, find_non_varying_var(out, !!key)]
  } else if (form == "long"){
    meta_data = meta(data)
  } else{
    abort("{form} meeds to be either long or list-col")
  }


  cubble_df(out, group = group_vars, meta_data = meta_data, form = determine_form(out))
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...) {

  out <- vec_slice(data, i)
  group_vars <- group_vars(data)
  meta_data <- meta(data)

  # update meta data
  meta_col <- group_vars[map_lgl(group_vars, ~has_name(meta_data, .x))]
  row <- meta_data[[meta_col]] %in% unique(out[[meta_col]])
  meta_data <- meta_data[row,]

  if ("tbl_ts" %in% class(data)){
    out <- tsibble::build_tsibble(out, key = group_vars(data)[1])
  }
  cubble_df(out, group = group_vars, meta_data = meta_data, form = determine_form(out) )
}

#' @export
dplyr_reconstruct.cubble_df <- function(data, template) {

  group_vars <- group_vars(template)
  form <- determine_form(template)

  if (form == "list-col"){
    key <- group_vars(template)
    meta_data <- data[, find_non_varying_var(data, !!key)]
  } else if (form == "long"){
    meta_data = meta(template)
  } else{
    abort("{form} meeds to be either long or list-col")
  }

  cubble_df(data, group = group_vars, meta_data = meta_data, form = determine_form(template))
}

#' @export
summarise.cubble_df <- function(data, ...){
  group_vars <- group_vars(data)
  meta_data <- meta(data)
  out <- NextMethod("summarise")

  cubble_df(out, group = group_vars, meta_data = meta_data, form = determine_form(out))
}

#' @export
left_join.cubble_df <- function(data1, data2, by = NULL, ...){

  if (!is_cubble(data1)){
    abort("data1 needs to be a cubble object")
  }

  if (form(data1) == "long" && any(by == group_vars(data1))){
    inform("Joining variable(s) being invariant to the group variable ...")
  }

  out <- NextMethod("left_join")
  dplyr_reconstruct(out, data1)

}

#' @export
select.cubble_df <- function(data, ...){
  out <- NextMethod("select")

  dplyr_reconstruct(out, data)
}


#' @export
group_by.cubble_df <- function(data, ...){
  new_group_var <- enquos(..., .named = TRUE)
  group_var <- union(group_vars(data), names(new_group_var))

  cubble_df(data, group = group_var, meta_data = meta(data), form = determine_form(data))
}

#' @export
ungroup.cubble_df <- function(data, ...){
  ungroup_var <- names(enquos(..., .named = TRUE))

  if (!all(ungroup_var %in% names(data))){
    problem <- ungroup_var[!ungroup_var %in% names(data)]
    abort(glue::glue("the ungroup variable: {problem} is not found in the data"))
  }

  if (group_vars(data)[1] %in% ungroup_var){
    abort("Can't ungroup the spatio identifier!")
  }


  updated_group_var <- setdiff(group_vars(data), ungroup_var)

  cubble_df(data, group = updated_group_var, meta_data = meta(data), form = determine_form(data))
}
