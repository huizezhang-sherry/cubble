#' Slice the data cube into lower dimension
#' @param dt the raw dataframe to slice, in long form - does it has to be long form?
#' @param key the dimension to slice
#'
#' @return a nested tibble/ tsibble - think through here
#' @examples
#' \dontrun{
#' raw %>% slice_by(station)
#' raw %>% slice_by(param)
#' raw %>% slice_by(time)
#' }
#' @export
slice_by <- function(dt, key){
  # implement multiple keys
  # key <- enquo(key)
  #
  # distinct_class <- dt %>% dplyr::distinct(!!key) %>% nrow()
  #
  # count_table <- dt %>%
  #   dplyr::group_by(!!key) %>%
  #   # summarise_all is superseded - change to use across
  #   dplyr::summarise_all(n_distinct) %>%
  #   dplyr::ungroup() %>%
  #   # summarise_if is superseded - change to use across
  #   dplyr::summarise_if(is.numeric, sum)
  #
  # var_non_varying <- names(count_table)[count_table==distinct_class]
  # var_varying <- names(count_table)[count_table!=distinct_class]
  #
  # dt %>%
  #   tidyr::nest(-c(!!key, var_non_varying))
}


