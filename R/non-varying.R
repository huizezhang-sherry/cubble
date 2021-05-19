#' Slice the data cube into lower dimension
#' @param dt the raw dataframe to slice, in long form - does it has to be long form?
#' @param key the dimension to slice
#'
#' @return a nested tibble/ tsibble - think through here
#' @examples
#' \dontrun{
#' water_raw %>% find_non_varying(station, parameter, time)
#' }
#' @export
find_non_varying <- function(dt, ...){
  key <- ensyms(...)

  distinct_class <- map_dbl(key, ~dt %>% dplyr::distinct(!!.x) %>% nrow())

  var_length <- map_dbl(colnames(dt), ~nrow(unique(dt[.x])))

  out <- unlist(map(distinct_class, ~colnames(dt)[var_length == .x]))
  names(out) <- NULL
  out

}


