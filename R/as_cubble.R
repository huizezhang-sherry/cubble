#' @export
as_cubble <- function(data, key){
  UseMethod("as_cubble")
}


#' @export
as_cubble.tbl_df <- function(data, key) {
  key <- enquo(key)

  if (quo_is_missing(key)){
    abort("Please specify the key variable for grouping")

  }
  all_vars <- find_invariant(data, !!key)

  out <- data %>%
    tidyr::nest(ts = c(!!!all_vars$variant)) %>%
    dplyr::rowwise()

  leaves_data <- new_leaves(data, !!key)

  new_cubble(out, group = as_name(key), leaves = leaves_data, form = "nested")
}


#' @export
as_cubble.rowwise_df <- function(data, key){
  key <- enquo(key)

  if (any(duplicated(data[[as_name(key)]]))){
    abort("Make sure each row identifies a key!")
  }

  type <- map_chr(data, class)
  leaves <- as_tibble(data) %>% tidyr::unnest() %>% new_leaves(!!key)

  list_col <- names(type)[type %in% "list"]

  if (length(list_col) == 0){
    abort("Can't identify the list-column, prepare the data as a rowwise_df with a list column")
  } else if (length (list_col) > 1){
    abort("Cubble currently can only deal with one list column")
  } else{
    nested_names <- Reduce(union, map(data[[as_name(list_col)]], names))
    if (any(nested_names == as_name(key))){
      data <- data %>%
        mutate(!!list_col := list(!!ensym(list_col) %>% select(-!!key)))
    }
  }

  new_cubble(data, group = as_name(key), leaves = leaves, form = "nested")
}
