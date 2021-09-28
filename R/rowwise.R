#' @export
strip_rowwise <- function(data){
  attr(data, "groups") <- attr(data, "groups")[NULL, ]
  attr(data, "class") <- attr(data, "class")[attr(data, "class") != "rowwise_df"]

  data
}

#' @export
switch_key <- function(data, new_key){
  new_key <- enquo(new_key)
  test_cubble(data)
  out <- data %>% as_tibble()
  new_cubble(out,
             key = as_name(new_key), index = index(data), coords = coords(data),
             leaves = new_leaves(out %>% unnest(), !!new_key), form = determine_form(data))
}

#' @export
rename_key <- function(data, ...){
  out <- data %>% as_tibble() %>% rename(...)

  new_cubble(out,
             key = names(list(...)), index = index(data), coords = coords(data),
             leaves = leaves(data), form = determine_form(data))
}
