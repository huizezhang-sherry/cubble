#' Aggregate the id axis based on an id-invariant variable
#'
#' @param dt the data cube
#' @param weight a weighting matrix for multivariate parameters
#' @return a cube with parameters being aggregated
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
#' v <- c(0.1, 0.2, 0.25, 0.3, 0.1, 0.05)
#' aggregate_param(water_small_cube, v)
#' }
#' @export
aggregate_param <- function(dt, weight){

  # linear combinations would make more sense if values are standardised first
  # think: how to supply more than one linear combination
  # in the meta it would be nice to show: param: comb_1: Electrical_conductivity * 0.1 + ...

  param_col <- param(dt)
  id_col <- id(dt)
  index_col <- index(dt)
  id_non_varying <- find_non_varying(dt, !!id_col) %>% map(as.symbol)

  unique_param <- unique(dt %>% pull(!!param_col))

  multiplies <- map2(syms(unique_param), weight, ~expr(!!.x * !!.y))
  single <- purrr::reduce(multiplies, ~expr(!!.x + !!.y))

  dt %>%
    as_tibble() %>%
    tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
    group_by(!!!id_non_varying, !!index_col) %>%
    transmute(comb_1 = eval(single)) %>%
    ungroup() %>%
    as_tsibble(index = !!index_col, key = c(!!id_col)) %>%
    build_cube(param = comb_1)

}

