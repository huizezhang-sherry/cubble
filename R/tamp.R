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
  # all_vars <- find_invariant(data, !!key)
  # if (as_name(key) %in% key_vars(data)){
  #
  # } else{
  #   leaves_data <- new_leaves(data, !!key)
  # }
  spatial <- spatial(data)
  tvars <- colnames(data)[colnames(data) != as_name(key)]
  tvars <- tvars[!tvars %in% colnames(spatial)]

  if (form(data) != "long"){
    cli::cli_abort("{.fn tamp} requires data to be in long form.")
  }

  suppressMessages(
  out <- tibble::as_tibble(data) %>%
    left_join(spatial) %>%
    tidyr::nest(ts = c(tvars)) %>%
    dplyr::rowwise()
  )

  if ("tbl_ts" %in% class(data)){
    out <- out %>% mutate(ts = list(as_tsibble(.data$ts, index = tsibble::index(data))))
  }

  new_cubble(out,
             key = as_name(key), index = index(data), coords = coords(data),
             spatial = NULL, form = "nested")
}

