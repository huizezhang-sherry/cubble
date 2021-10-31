#' Switch a cubble object into a long form
#' @param data a cubble object
#' @param col the list column to be stretched
#' @param ... variables to be stretched into a single column
#' @details
#' `stretch()` switch a cubble object into the long form where the combination of group identifier
#' and timestamp defines a row. The long form cubble is always of class `cubble_df` and `grouped_df`.
#'
#' @examples
#' climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   stretch()
#'
#' # another example for stretching two cubbles
#'
#' @export
#' @seealso Other cubble verbs include \code{\link{tamp}} and \code{\link{migrate}}
stretch <- function(data, col, ...) {
  test_cubble(data)
  UseMethod("stretch")
}

#' @export
stretch.cubble_df <- function(data, col, ...){

  col <- enquo(col)

  key <- syms(key_vars(data))
  index <- index(data)
  coords <- coords(data)

  data <- as_tibble(data)
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])
  #cols <- syms(tidyselect::vars_select(names(data), !!enquo(cols)))

  if (quo_is_missing(col)){
    col <- sym(get_listcol(data))
    if (length(col) > 1){
      cli::cli_abort("Can't detect the column to stretch. Please specify the list column to stretch. ")
    }
  }

  test_list(data, !!col)

  if (is_tsibble){
    data$ts <- map(data$ts, tibble::as_tibble)
  }


  out <- data %>% select(!!!key, !!col) %>% unnest(c(!!col))
  # out <- map2_dfr(cols, key,
  #                 ~unnest_with_rename(data = data,
  #                                     .x, .y,
  #                                     first_key = first_key,
  #                                     col_tojoin1, col_tojoin2))

  spatial <- data %>% select(-!!col)

  if (is_tsibble) {
    out <- out %>% tsibble::as_tsibble(key = !!key[[1]])
  }

  new_cubble(out,
             key = key, index = index, coords = coords,
             spatial = spatial, form = "long")
}




