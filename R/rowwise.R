#' Remove the rowwise grouping of a cubble
#'
#' @param data a cubble object
#' @examples
#' library(dplyr)
#' # row number is not properly added since each row is a separate group
#' aus_climate %>% mutate(.id = row_number())
#'
#' # proper id after removing the grouping structure
#' aus_climate %>% strip_rowwise() %>% mutate(.id = row_number())
#'
#' @export
strip_rowwise <- function(data){

  test_cubble(data)
  attr(data, "groups") <- attr(data, "groups")[NULL, ]
  attr(data, "class") <- attr(data, "class")[attr(data, "class") != "rowwise_df"]

  data
}

#' Switch to a different key of a cubble
#' @param data a cubble
#' @param new_key the new key
#' @param back whether it is a switch back to the individual level
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
#' @export
switch_key <- function(data, new_key, back = FALSE){
  new_key <- enquo(new_key)
  test_cubble(data)

  # if (back){
  #   leaves <- new_leaves(data %>% unnest_cubble(tsibble_key = !!new_key), !!new_key)
  # } else{
  #   leaves <- new_leaves(data %>% unnest_cubble(), !!new_key)
  # }

  leaves <- leaves(data)

  new_cubble(data,
             key = as_name(new_key), index = index(data), coords = coords(data),
             leaves = leaves, form = determine_form(data))
}

#' @export
remove_key <- function(data, key){
  key <- enquo(key)
  test_cubble(data)

  old_key <- key_vars(data)
  leaves <- leaves(data)

  if (!as_name(key) %in% old_key){
    abort(glue::glue("{as_name(key)} is not a cubble key!"))
  }

  new_key <- old_key[old_key != as_name(key)]

  ts_df <- data %>% select(new_key, ts) %>% as_tibble()
  out_ts <- vec_split(ts_df[,which(names(ts_df) != new_key)],
                          ts_df[,new_key]) %>%
    as_tibble() %>%
    unpack(key) %>%
    mutate(ts = map(val, ~unchop(.x, ts) %>% unpack(ts)))

  data <- as_tibble(data)
  inv <- find_invariant(data, !!new_key)

  out <- vec_split(data[,which(!names(data) %in% c(inv$invariant, "ts"))],
                   data[,inv$invariant]) %>%
    as_tibble() %>%
    unpack(key) %>%
    left_join(out_ts %>% select(new_key, ts), by = new_key) %>%
    arrange(!!sym(new_key))

  leaves <- as_leaves(data, variant = inv$variant)

  new_cubble(out,
             key = new_key, index = index(data), coords = coords(data),
             leaves = leaves, form = determine_form(data))
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
