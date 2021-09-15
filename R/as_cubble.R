#' @rdname cubble-class
#' @export
as_cubble <- function(data, key, index, coords) {
  UseMethod("as_cubble")
}

#' @rdname cubble-class
#' @export
as_cubble.tbl_df <- function(data, key, index, coords) {

  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  test_missing(quo = key, var = "key")
  test_missing(quo = index, var = "index")
  test_missing(quo = coords, var = "coords")

  # check presents in the data
  # checks for key
  # checks for index
  # checks for coords
  coords <- names(data)[eval_select(coords, data)]
  # - check lat between -90 to 90
  # - check long between -180 to 180?
  # - give it an attribution on the range? 0 to 360 or -180 to 180

  # compute leaves
  all_vars <- find_invariant(data, !!key)

  out <- data %>%
    tidyr::nest(ts = c(!!!all_vars$variant)) %>%
    dplyr::rowwise()

  leaves_data <- new_leaves(data, !!key)

  new_cubble(out,
             key = as_name(key), index = as_name(index), coords = coords,
             leaves = leaves_data, form = "nested")
}

#' @rdname cubble-class
#' @export
as_cubble.rowwise_df <- function(data, key, index, coords) {
  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  test_missing(quo = key, var = "key")
  test_missing(quo = index, var = "index")
  test_missing(quo = coords, var = "coords")

  # check presents in the data
  # checks for key
  # checks for index
  # checks for coords
  coords <- names(data)[eval_select(coords, data)]

  if (any(duplicated(data[[as_name(key)]]))){
    abort("Make sure each row identifies a key!")
  }

  # compute leaves
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

  new_cubble(data,
             key = as_name(key), index = as_name(index), coords = coords,
             leaves = leaves, form = "nested")
}
