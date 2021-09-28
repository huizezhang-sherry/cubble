#' @rdname cubble-class
#' @importFrom tidyr unchop
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

  # check if date is already nested in the list-column
  col_type <- map(data, class)
  listcol_var <- names(col_type)[col_type == "list"]

  if (length(listcol_var) == 0){
    all_vars <- find_invariant(data, !!key)

    out <- data %>%
      tidyr::nest(ts = c(!!!all_vars$variant)) %>%
      dplyr::rowwise()

    leaves_data <- new_leaves(data, !!key)
  } else{
    listcol_var <- listcol_var[1]
    invariant_var <- names(col_type)[col_type != "list"]
    chopped <- data %>% tidyr::unchop(listcol_var)
    already <- as_name(index) %in% names(chopped$ts)

    out <- data
    variant <- chopped$ts %>% map_chr(pillar::type_sum)
    leaves_data <- as_leaves(data[,invariant_var], variant)
  }

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

  # if (any(duplicated(data[[as_name(key)]]))){
  #   abort("Make sure each row identifies a key!")
  # }

  # compute leaves
  leaves <- as_tibble(data) %>% tidyr::unnest() %>% new_leaves(!!key)
  list_col <- get_listcol(data)

  if (length(list_col) == 0){
    abort("Can't identify the list-column, prepare the data as a rowwise_df with a list column")
  } else if (length (list_col) > 2){
    abort("Cubble currently can only deal with at most two list columns")
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
