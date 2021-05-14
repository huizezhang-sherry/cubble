#' Aggregate the time axis
#'
#' @param dt the data cube
#' @param period the period to be aggregated into
#' @param summarised_by the summary function applied to aggregation
#' @export
#'
#' @examples
#' raw %>% aggregate_time(period = "month", summarised_by = mean)
aggregate_time <- function(dt, period, summarised_by){

  # the argument name summarised_by is a bit clumsy

  dt %>%
    # here the column should be the one detected as the axis, rather than time
    # period should translate into a function: "month" -> yearmonth(), "day" -> as_date()
    # also need to check if the period make sense with the current time
    dplyr::mutate(time = as.Date(time)) %>%
    # also you want all the other columns to also be preserved after summarise
    dplyr::group_by(time, parameter, station) %>%
    # the summary function should be dictated by summarised_by rather than fixed as mean
    # think about the case where different summary functions for different parameter
    dplyr::summarise(value = mean(value))

}
