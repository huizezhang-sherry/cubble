#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {
  key <- key_vars(data)

  if ("tbl_ts" %in% class(data)){
    out <- dplyr_col_modify(tsibble::as_tsibble(tibble::as_tibble(data), key = !!key), cols)
  } else{
    out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  }

  # update leaves
  # long form shouldn't make change on the meta but list-col form may (i.e. mutate)
  form <- determine_form(data)
  if (form == "nested"){
    leaves_data <- out %>%
      mutate(ts = map(.data$ts, tibble::as_tibble)) %>%
      tidyr::unnest(.data$ts) %>%
      new_leaves(!!key)
  } else if (form == "long"){
    leaves_data <-  leaves(data)
  } else{
    abort("{form} meeds to be either long or nested")
  }

  new_cubble(out, key = key, leaves = leaves_data, form = determine_form(out))
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...){

  out <- vec_slice(data, i)
  key <- key_vars(data)

  form <- determine_form(data)
  if (form == "nested"){
    leaves_data <- as_tibble(out) %>%
      mutate(ts = map(.data$ts, tibble::as_tibble)) %>%
      tidyr::unnest(.data$ts) %>%
      new_leaves(!!key)
  } else if (form == "long"){
    leaves_data <- leaves(data)
  } else{
    abort("{form} meeds to be either long or nested")
  }


  if ("tbl_ts" %in% class(data)){
    out <- tsibble::build_tsibble(out, key = key_vars(data)[1])
  }
  new_cubble(out, key = key, leaves = leaves_data, form = determine_form(out) )
}

#' @export
dplyr_reconstruct.cubble_df <- function(data, template) {

  form <- determine_form(template)
  key <- key_vars(template)[1]

  data_var <- data[, find_invariant(data, !!key)$invariant] %>% names()
  old_leaves <- leaves(template) %>% names()
  new_leaves <- data_var[data_var %in% old_leaves]
  leaves_data <- leaves(template)[c(key, new_leaves)]


  new_cubble(data, key = key, leaves = leaves_data, form = determine_form(template))
}

#' @export
summarise.cubble_df <- function(data, ...){
  key <- key_vars(data)
  leaves_data <- leaves(data)
  out <- NextMethod("summarise")

  new_cubble(out, key = key, leaves = leaves_data, form = determine_form(out))
}

#' @export
left_join.cubble_df <- function(data1, data2, by = NULL, ...){

  if (!is_cubble(data1)){
    abort("data1 needs to be a cubble object")
  }

  if (form(data1) == "long" && any(by == key_vars(data1))){
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
  key <- union(key_vars(data), names(new_group_var))

  new_cubble(data, key = key, leaves = leaves(data), form = determine_form(data))
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


  updated_group_var <- setdiff(key_vars(data), ungroup_var)

  new_cubble(data, key = updated_group_var,leaves = leaves(data), form = determine_form(data))
}

#' @export
rename.cubble_df <- function(data, ...){
  out <- data %>% as_tibble() %>% rename(...)
  dplyr_reconstruct(out, data)
}

#' @export
unnest.cubble_df <- function(data, ...){
  out <- data %>% as_tibble() %>% unnest(...)
  dplyr_reconstruct(out, data)
}

