#' Switch to a different key of a cubble
#' @param data a cubble
#' @param key the new key
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' # create a data with hierarchical structure:
#' # id (station) is nested in country, country is nested in continent
#' long <- world_climate %>%
#'   stretch() %>%
#'   migrate(country, continent) %>%
#'   filter(lubridate::year(date) == 2020)
#'
#' # switch the key from id to country to aggregate on country
#' country_wise <- long %>%
#'   switch_key(country) %>%
#'   group_by(date) %>%
#'   summarise(tmax = mean(tmax, na.rm = TRUE))
#'
#' # plot the aggregated maximum temperature for each country
#' country_wise %>%
#'   ggplot(aes(x = date, y = tmax, group = country)) +
#'   geom_line() +
#'   facet_wrap(vars(country)) +
#'   scale_x_date(date_labels = "%b")
#' @importFrom tidyr unpack unchop
#' @export
switch_key <- function(data, key){
  new_key <- enquo(key)
  test_cubble(data)

  if (!as_name(new_key) %in% names(data)){
    abort(glue::glue("{as_name(key)} does not exist in {data}!"))
  }

  data_form <- form(data)
  if ( data_form == "long") data <- data %>% tamp()

  ts_df <- data %>% dplyr::select(!!new_key, .data$ts) %>% tibble::as_tibble()
  out_ts <- vctrs::vec_split(ts_df[,which(names(ts_df) != as_name(new_key))],
                      ts_df[,as_name(new_key)]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::mutate(ts = map(val, ~tidyr::unchop(.x, .data$ts) %>% tidyr::unpack(.data$ts)))

  data <- tibble::as_tibble(data)
  inv <- find_invariant(data, !!new_key)

  out <- vctrs::vec_split(data[,which(!names(data) %in% c(inv$invariant, "ts"))],
                   data[,inv$invariant]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::left_join(out_ts %>% dplyr::select(!!new_key, .data$ts), by = as_name(new_key)) %>%
    dplyr::arrange(!!new_key)

  leaves <- as_leaves(data, variant = inv$variant)

  out_cubble <- new_cubble(out,
                           key = as_name(new_key), index = index(data), coords = coords(data),
                           leaves = leaves, form = determine_form(data))

  if (data_form == "long") out_cubble <- out_cubble %>% stretch(ts)

  out_cubble
}


#' Rename the key variable
#' @param data a cubble
#' @param ... argument passed to \code{rename}: NEW = OLD
#'
#' @export
rename_key <- function(data, ...){
  test_cubble(data)
  out <- data %>% as_tibble() %>% rename(...)

  new_cubble(out,
             key = names(list(...)), index = index(data), coords = coords(data),
             leaves = leaves(data), form = determine_form(data))
}
