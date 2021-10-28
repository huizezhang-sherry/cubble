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
  form <- determine_form(data)
  if (form == "nested"){
    spatial <- NULL
  } else if (form == "long"){
    leaves_data <-  spatial(data)
  } else{
    abort("{form} meeds to be either long or nested")
  }

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             spatial = spatial, form = determine_form(out),
             tsibble_attr = tsibble_attr)
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...){

  out <- vec_slice(data, i)
  key <- key_vars(data)

  form <- determine_form(data)
  if (form == "nested"){
    spatial = NULL
  } else if (form == "long"){
    spatial <- spatial(data)
  } else{
    abort("{form} meeds to be either long or nested")
  }


  if ("tbl_ts" %in% class(data)){
    out <- tsibble::build_tsibble(out, key = key_vars(data)[1])
  }
  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             spatial = spatial, form = determine_form(out))
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

  if (form == "nested"){
    spatial = NULL
  } else if (form == "long"){
    spatial <- spatial(data)
  } else{
    abort("{form} meeds to be either long or nested")
  }


  new_cubble(data,
             key = key, index = index(template), coords = coords(template),
             spatial = spatial, form = determine_form(template))
}

#' @export
summarise.cubble_df <- function(data, ...){
  key <- key_vars(data)
  leaves_data <- leaves(data)
  out <- NextMethod("summarise")

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             leaves = leaves_data, form = determine_form(out))
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


  if (is_cubble(data2)){
    key <- c(key_vars(data1), key_vars(data2))
    index <- index(data1)
    coords <- list(coords(data1), coords(data2))
    leaves <- list(leaves(data1), leaves(data2))
    form <- determine_form(out)

    out <- new_cubble(out,
               key = key, index = index, coords = coords,
               leaves = leaves, form = form)
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
  new_group_var <- enquos(..., .named = TRUE)
  key <- union(key_vars(data), names(new_group_var))

  new_cubble(data,
             key = key, index = index(data), coords = coords(data),
             leaves = leaves(data), form = determine_form(data))
}

#' @export
ungroup.cubble_df <- function(data, ...){
  ungroup_var <- names(enquos(..., .named = TRUE))

  if (!all(ungroup_var %in% names(data))){
    problem <- ungroup_var[!ungroup_var %in% names(data)]
    abort(glue::glue("the ungroup variable: {problem} is not found in the data"))
  }

  if (key_vars(data)[1] %in% ungroup_var){
    abort("Can't ungroup the spatio identifier!")
  }


  updated_group_var <- setdiff(key_vars(data), ungroup_var)

  new_cubble(data,
             key = updated_group_var, index = index(data), coords = coords(data),
             leaves = leaves(data), form = determine_form(data))
}

#' @export
rename.cubble_df <- function(.data, ...){
  out <- .data %>% as_tibble() %>% rename(...)
  dplyr_reconstruct(out, .data)
}

unnest_cubble <- function(data, ..., tsibble_key = NULL){

  if ("tbl_ts" %in% class(data$ts[[1]])){

    tsibble_index <- index(data$ts[[1]])
    tsibble_key <- enquo(tsibble_key)
    if (quo_is_null(tsibble_key)) tsibble_key <- sym(key_vars(data))
    data <- data %>%
      mutate(ts = list(as_tibble(ts))) %>%
      as_tibble()

    unnested <- data %>% unnest(...)

    out <- unnested %>% as_tsibble(index = tsibble_index, key = !!tsibble_key)

  } else{
    out <- data %>% as_tibble() %>% unnest(...)
  }

  dplyr_reconstruct(out, data)
}

