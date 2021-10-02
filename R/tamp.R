#' Formulate or switch an object into a cubble in the nested form
#'
#' Create a cubble object or switch an existing cubble object into the nested form
#'
#' @details
#' * For an object that is not `cubble_df`, `tamp()` will initialise a cubble object upon
#' providing a `group` variable.
#'
#' * For an existing `cubble` in the long form, `tamp()`
#' switches the object back to the nested form. This form is easier to computing
#' group-specific variables.
#'
#' @param data the data to be converted into a cubble object
#' @param key the spatio identifier. Key can be automatically detected for a cubble object
#' @examples
#' # switch to the nested form from the long form
#' aus_climate %>%
#'   stretch() %>%
#'   filter(lubridate::year(date) == 2020) %>%
#'   tamp()
#' @export
#' @seealso Other cubble verbs include \code{\link{stretch}} and \code{\link{migrate}}
tamp <- function(data, key) {
  UseMethod("tamp")
}

#' @importFrom tsibble index as_tsibble
#' @export
tamp.cubble_df <- function(data, key) {
  test_cubble(data)

  # will only keep the first grouping variable if more than one
  key <- enquo(key)
  if (quo_is_missing(key)){
    key <- key_vars(data)
    if (length(key) > 1) key <- key[1]
    key <- quo(!!sym(key))
  }

  # compute metadata again if change to a different key
  all_vars <- find_invariant(data, !!key)
  if (as_name(key) %in% key_vars(data)){
    leaves_data <- leaves(data)
  } else{
    leaves_data <- new_leaves(data, !!key)
  }

  if (form(data) == "long"){
    out <- tibble::as_tibble(data) %>%
      left_join(leaves_data) %>%
      tidyr::nest(ts = c(!!!all_vars$variant)) %>%
      dplyr::rowwise()
  } else{
    abort("Currently `tamp.cubble_df` is only for switching form long form to nested form")
  }

  if ("tbl_ts" %in% class(data)){
    out <- out %>% mutate(ts = list(as_tsibble(.data$ts, index = tsibble::index(data))))
  }

  new_cubble(out,
             key = as_name(key), index = index(data), coords = coords(data),
             leaves = leaves_data, form = "nested")
}

