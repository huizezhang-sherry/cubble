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
#' library(lubridate)
#' library(dplyr)
#'
#' # create a cubble object from a tibble
#' climate_flat %>% tamp(station)
#'
#' # switch to the nested form from the long form
#' climate_small %>%
#'   stretch() %>%
#'   filter(year(date) == 2020) %>%
#'   tamp()
#' @export
#' @seealso Other cubble verbs include \code{\link{stretch}} and \code{\link{migrate}}
tamp <- function(data, key) {
  UseMethod("tamp")
}

#' @importFrom tsibble index
#' @export
tamp.cubble_df <- function(data, key) {
  test_cubble(data)

  # will only keep the first grouping variable if more than one
  key <- enquo(key)
  if (quo_is_missing(key)){
    grp <- group_vars(data)
    if (length(grp) > 1) grp <- grp[1]
    key <- quo(!!sym(grp))
  }

  # compute metadata again if change to a different key
  nest_var <- find_nest_var(data, !!key)
  if (as_name(key) %in% group_vars(data)){
    meta_data <- meta(data)
  } else{
    meta_data <- tibble::as_tibble(data[, find_non_varying_var(data, !!key)])
  }

  if (form(data) == "long"){
    out <- tibble::as_tibble(data) %>%
      left_join(meta_data) %>%
      tidyr::nest(ts = c(!!!nest_var$nest_var)) %>%
      dplyr::rowwise()
  } else{
    abort("Currently `tamp.cubble_df` is only for switching form long form to nested form")
  }

  if ("tbl_ts" %in% class(data)){
    out <- out %>% mutate(ts = list(as_tsibble(.data$ts, index = tsibble::index(data))))
  }

  cubble_df(out, group = as_name(key), meta_data = meta_data, form = "nested")
}

#' @export
tamp.tbl_df <- function(data, key) {
  key <- enquo(key)

  if (quo_is_missing(key)){
    abort("Please specify the key variable for grouping")

  }
  nest_var <- find_nest_var(data, !!key)

  out <- data %>%
    tidyr::nest(ts = c(!!!nest_var$nest_var)) %>%
    dplyr::rowwise()
  meta_data <- tibble::as_tibble(out[nest_var$non_varying_var])
  cubble_df(out, group = as_name(key), meta_data = meta_data, form = "nested")
}
