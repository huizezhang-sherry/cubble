#' Switch a cubble object into a long form
#' @param data a cubble object
#' @param cols the list column to be stretched
#' @param ... variables to be stretched into a single column
#' @details
#' `stretch()` switch a cubble object into the long form where the combination of group identifier
#' and timestamp defines a row. The long form cubble is always of class `cubble_df` and `grouped_df`.
#'
#' @examples
#' climate_flat %>%
#'   as_cubble(key = station, index = date, coords = c(long, lat)) %>%
#'   stretch()
#'
#' # another example for stretching two cubbles
#'
#' @export
#' @seealso Other cubble verbs include \code{\link{tamp}} and \code{\link{migrate}}
stretch <- function(data, cols, ...) {
  test_cubble(data)
  UseMethod("stretch")
}

#' @export
stretch.cubble_df <- function(data, cols, ...){

  dots <- list2(...)
  col_tojoin1 <- map(dots, ~names(.x))
  col_tojoin2 <- map(dots, ~.x)

  cols <- syms(tidyselect::vars_select(names(data), !!enquo(cols)))

  if (is_empty(cols)){
    cols <- get_listcol(data)
    if (length(cols) > 1){
      abort("Please specify the list column to stretch. ")
    }
    cols <- list(sym(cols))
  }

  test_list <- map_lgl(cols, ~eval_tidy(.x,  data) %>% is_list())

  if (!all(test_list)) {
    abort("The column to stretch need to be a list-column")
  }

  key <- syms(key_vars(data))
  key_names <-  map_chr(key, as_name)
  first_key <- key_names[1]

  leaves_data <- leaves(data)

  list_col <- data[,cols[[1]]][[1]]

  if ("tbl_ts" %in% class(list_col[[1]])){
    data$ts <- map(data$ts, tibble::as_tibble)
  }

  out <- map2_dfr(cols, key,
                  ~unnest_with_rename(data = data,
                                      .x, .y,
                                      first_key = first_key,
                                      col_tojoin1, col_tojoin2))


  if ("tbl_ts" %in% class(list_col[[1]])) {
    out <- out %>% tsibble::as_tsibble(key = !!key[[1]])
  }

  new_cubble(out,
             key = first_key, index = index(data), coords = coords(data),
             leaves = leaves_data, form = "long")
}


# helper
unnest_with_rename <- function(data, cols, key, first_key, col_tojoin1, col_tojoin2){

  dt <- data %>% as_tibble() %>% select(key, cols)

  cur_key <- as_name(key)
  if (cur_key != first_key){

    dt <- dt %>% rename(!!sym(first_key) := cur_key) %>% unnest(cols)
    if (!is_null(col_tojoin1) & !is_null(col_tojoin2)) {
      dt <- map2_dfr(col_tojoin1, col_tojoin2, ~dt %>% rename(!!sym(.x) := !!sym(.y)))
    }
  } else {
    dt <- dt %>% unnest(cols)
  }

  dt

}
