#' #' Slice the data cube into lower dimension
#' #' @param dt the raw dataframe to slice, in long form - does it has to be long form?
#' #' @param key the dimension to slice
#' #'
#' #' @return a nested tibble/ tsibble - think through here
#' #' @examples
#' #' \dontrun{
#' #' water_raw %>% find_non_varying(station, parameter, time)
#' #' }
#' #' @export
#' find_non_varying <- function(dt, ...){
#'   key <- ensyms(...)
#'   # currently don't process the non-varying variables for the index axis
#'   include_index <- any(key == index(dt))
#'   key <- key[key != index(dt)]
#'
#'   out <- unique(unlist(map(key, find_one, dt = dt)))
#'   if(include_index) out <- c(out, as_label(index(dt)))
#'   names(out) <- NULL
#'   out
#'
#' }
#'
#'
#' find_one <- function(dt, key){
#'   subset <- dt %>% as_tibble() %>% tidyr::nest(data = -(!!key)) %>% pull(data) %>% .[[1]]
#'   var_length <- map_dbl(colnames(subset), ~nrow(unique(subset[.x])))
#'   c(as_name(key), colnames(subset)[var_length == 1])
#'
#' }
