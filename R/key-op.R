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
#' # country_wise <- long %>%
#' #   switch_key(country) %>%
#' #   group_by(date) %>%
#' #   summarise(tmax = mean(tmax, na.rm = TRUE))
#'
#' # plot the aggregated maximum temperature for each country
#' # country_wise %>%
#' #   ggplot(aes(x = date, y = tmax, group = country)) +
#' #   geom_line() +
#' #   facet_wrap(vars(country)) +
#' #   scale_x_date(date_labels = "%b")
#' @importFrom tidyr unpack unchop
#' @export
switch_key <- function(data, key){
  new_key <- enquo(key)
  new_key_var <- as_name(new_key)
  old_key <- key_vars(data)
  test_cubble(data)

  if (!as_name(new_key) %in% names(data)){
    cli::cli_abort("{.field as_name(key)} does not exist in {data}!")
  }

  orig_form <- form(data)
  if (orig_form == "long") data <- data %>% tamp()

  data <- tibble::as_tibble(data)
  ts_df <- data %>% dplyr::select(!!new_key, !!old_key, .data$ts) %>% tibble::as_tibble()
  out_ts <- vctrs::vec_split(ts_df %>% select(-new_key_var), ts_df[,new_key_var]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::mutate(ts = map(.data$val, ~tidyr::unchop(.x, .data$ts) %>% tidyr::unpack(.data$ts))) %>%
    dplyr::select(-.data$val)

  inv <- find_invariant(data, !!new_key)
  other_cols <- names(data)[!names(data) %in% c(inv$invariant, "ts")]
  out <- vctrs::vec_split(data[,other_cols], data[,inv$invariant]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::left_join(out_ts %>% dplyr::select(!!new_key, .data$ts), by = new_key_var) %>%
    dplyr::arrange(!!new_key)

  out_cubble <- new_cubble(out,
                           key = new_key_var, index = index(data), coords = coords(data),
                           spatial = NULL, form = "nested")

  if (orig_form == "long") out_cubble <- out_cubble %>% stretch(.data$ts)

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
             spatial = spatial(data), form = determine_form(data))
}

#' Matching
#'
#' @param major
#' @param minor
#' @param n_slice
#' @param dist_max
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
match_key <- function(major, minor, n_slice = 1, dist_max = 10, ...){
  test_cubble(major)
  test_cubble(minor)

  coords_mj <- syms(coords(major))
  coords_mn <- syms(coords(minor))

  if (identical(coords_mj, coords_mn)){
    major <- major %>% rename(long_ref = coords_mj[[1]],
                              lat_ref = coords_mj[[2]])
    coords_mj <- syms(c("long_ref", "lat_ref"))
  }

  key_mj <- key_vars(major)
  key_mn <- key_vars(minor)

  if (identical(key_mj, key_mn)){
    major <- major %>% rename_key(key_mj = key_mj)
    key_mj <- "key_mj"
    minor <- minor %>% rename_key(key_mn = key_mn)
    key_mn <- "key_mn"
  }

  out <- major %>%
    as_tibble() %>%
    mutate(minor = list(as_tibble(minor) %>% select(!!key_mn, !!!coords_mn))) %>%
    unnest(minor) %>%
    mutate(dist = rnoaa::meteo_spherical_distance(long1 = !!coords_mj[[1]],
                                                  long2 = !!coords_mn[[1]],
                                                  lat1 = !!coords_mj[[2]],
                                                  lat2 = !!coords_mn[[2]])) %>%
    group_by(!!sym(key_mj)) %>% slice_min(dist, n = n_slice) %>%
    select(!!sym(key_mj), !!sym(key_mn), dist) %>%
    ungroup() %>%
    filter(dist <= dist_max) %>%
    mutate(.group = row_number())

  out
}
