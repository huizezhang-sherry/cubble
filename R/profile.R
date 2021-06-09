#' #' Profile the data cube
#' #' @param dt the data cube to profile
#' #'
#' #' @return  here should be a customised proint of the profile
#' #' @export
#' profile_cube <- function(dt){
#'
#'   #first step is to detect the three axis
#'   # produce a vector of axis (axis <- c(station, time, param))
#'
#'   # map(axis, profile_single)
#'
#'   # then customise a print for the profile
#'   # - id <station>
#'   #   - name
#'   #   - lat
#'   #   - long
#'   # - param <parameter>
#'   # - time <time: interval>
#'   #
#' }
#'
#'
#' profile_single <- function(dt, axis){
#'
#'   # axis <- enquo(axis)
#'   # distinct_class <- dt %>% dplyr::distinct(!!axis) %>% nrow()
#'   #
#'   # count_table <- dt %>%
#'   #   dplyr::group_by(!!axis) %>%
#'   #   dplyr::summarise_all(dplyr::n_distinct) %>%
#'   #   dplyr::ungroup() %>%
#'   #   dplyr::summarise_if(is.numeric, sum)
#'   #
#'   # var_non_varying <- names(count_table)[count_table==distinct_class]
#'   # var_varying <- names(count_table)[count_table!=distinct_class]
#'   #
#'   # # extend the output for the case where all the variables are var_varying
#'   # tibble::tibble(!!axis := var_non_varying)
#' }
#'
