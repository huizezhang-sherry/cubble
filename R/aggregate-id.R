#' Aggregate the id axis based on an id-invariant variable
#'
#' @param dt the data cube
#' @param val the value column to aggregate
#' @param along the id-invariant variable to aggregate upon
#' @param ... additional operations on the `along` variable
#' @param FUN a function to aggregate by
#' @return a cube with id being aggregated
#'
#' @examples
#' \dontrun{
#' library(stringr)
#' water_small_cube <- water_small %>%
#'   mutate(river_name = str_extract(name, "^[^\\@]+ "),
#'          river_name = as.factor(str_trim(river_name)),
#'          river_name = tolower(word(river_name, 1, 2, sep = " "))) %>%
#'   as_tsibble(key = c(parameter, station), index = date) %>%
#'   as_cube(param = parameter)
#' aggregate_id(water_small_cube, value, long, ~ggplot2::cut_interval(.x, 3))
#' aggregate_id(water_small_cube, val = value, along = river_name)
#' }
#' @export
aggregate_id <- function(dt, val, along, ..., FUN = mean){

  # issue: cut_interval would approximate a decimal number to integer before cutting: 143.xxx -> 144 -> [144, 145]

  along <- ensym(along)
  val <- ensym(val)
  if (!is_missing(...)) expr <- purrr::as_mapper(...)

  param_col <- param(dt)
  id_col <- id(dt)
  index_col <- index(dt)


  if (!vec_in(as_label(along),find_non_varying(dt, !!id_col))){
    abort(glue::glue("{along} must be invariant to {id_col}!"))
  }

  if (is.numeric(eval_tidy(along, dt))){
    dt <- dt %>%
      as_tibble() %>%
      mutate({{along}} := expr(eval_tidy(along, dt))) %>%
      as_tsibble(index = !!index_col, key = c(!!id_col, !!param_col)) %>%
      build_cube(param = !!param_col)
  }

  dt %>%
    as_tibble() %>%
    group_by(!!along,!!param_col, !!index_col) %>%
    dplyr::summarise({{val}} := exec(FUN, {{val}}, na.rm = TRUE)) %>%
    ungroup() %>%
    as_tsibble(index = !!index_col, key = c(!!along, !!param_col)) %>%
    build_cube(param = !!param_col)
}

