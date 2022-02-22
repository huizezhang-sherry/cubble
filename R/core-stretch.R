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
  row_id <- row_id(data)

  data <- as_tibble(data)
  is_tsibble <- "tbl_ts" %in% map_chr(data$ts, ~class(.x)[1])

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

  spatial <- data %>% select(-!!col)
  if (".val" %in% colnames(spatial)) spatial <- spatial %>% unnest(.val
                                                                   )

  if (is_tsibble) {
    out <- out %>% tsibble::as_tsibble(key = !!key[[1]], index = index)
  }

  new_cubble(out,
             key = map_chr(key, as_name), index = index, coords = coords,
             row_id = row_id, spatial = spatial, form = "long")
}




