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
  new_key_var <- as_name(new_key)
  old_key <- key_vars(data)
  coords <- coords(data)
  index <- index(data)
  test_cubble(data)

  orig_form <- form(data)
  if (orig_form == "long") data <- data %>% tamp()

  nested_already <- ".val" %in% names(data)
  if (nested_already){
    data <- data %>% tidyr::unnest(.val)
  }

  if (!as_name(new_key) %in% names(data)){
    cli::cli_abort("{.field {new_key_var}} does not exist in the data!")
  }

  data <- tibble::as_tibble(data)

  if (!nested_already){
    out <- rearrange_index(data, key = new_key, old_key = old_key)
  } else{
    out <- rearrange_index2(data, key = new_key, old_key = old_key)
  }



  out_cubble <- new_cubble(out,
                           key = new_key_var, index = index, coords = coords,
                           row_id = old_key, spatial = NULL, form = "nested")

  if (orig_form == "long") out_cubble <- out_cubble %>% stretch(.data$ts)

  out_cubble
}

rearrange_index <- function(data, key, old_key = NULL){
  new_key_var <- as_name(key)
  ts_df <- data %>% dplyr::select(!!key, !!old_key, .data$ts) %>% tibble::as_tibble()
  out_ts <- vctrs::vec_split(ts_df %>% select(-new_key_var), ts_df[,new_key_var]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::mutate(ts = map(.data$val, ~tidyr::unchop(.x, .data$ts) %>% tidyr::unpack(.data$ts))) %>%
    dplyr::select(-.data$val)

  inv <- find_invariant(data, !!key)
  other_cols <- names(data)[!names(data) %in% c(inv$invariant, "ts")]
  out <- vctrs::vec_split(data[,other_cols], data[,inv$invariant]) %>%
    tibble::as_tibble() %>%
    rename(.val = val) %>%
    tidyr::unpack(key) %>%
    dplyr::left_join(out_ts %>% dplyr::select(!!key, .data$ts), by = new_key_var) %>%
    dplyr::arrange(!!key)

  if (unique(map_dbl(out$.val, ncol)) == 0){
    out <- out %>% dplyr::select(-.val)
  }

  out

}

rearrange_index2 <- function(data, key, old_key = NULL){
  new_key_var <- as_name(key)
  ts_df <- map_dfr(data$ts, rbind)
  out_ts <- vctrs::vec_split(ts_df %>% select(-new_key_var) , ts_df[,new_key_var]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key) %>%
    dplyr::rename(ts = val)

  inv <- find_invariant(data, !!key)
  other_cols <- names(data)[!names(data) %in% c(inv$invariant, "ts")]
  out <- data %>% dplyr::select(-ts) %>%
    dplyr::left_join(out_ts, by = new_key_var) %>%
    dplyr::arrange(!!key)

  out

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
             row_id = row_id(data), spatial = spatial(data), form = determine_form(data))
}


#' Title
#'
#' find the convex hull that wraps around the cluster, make it a polygon, find the centroid of the polygon and finally, extract the x and y coordinate of each centroid:
#' @param data a cubble data object
#'
#' @importFrom geosphere centroid
#' @export
#'
get_centroid <- function(data){
  test_cubble(data)

  if (form(data) != "nested"){
    abort("Require nested form of a cubble to calculate centroid")
  }

  if (!".val" %in% colnames(data)){
    abort("Something is wrong - there should be a .val column")
  }

  key <- key_vars(data)
  coords <- coords(data)

  out <- data %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(chull = list(chull(.val[[coords[1]]],
                              .val[[coords[2]]])),
           hull = list(.val[chull,]),
           cent = list(geosphere::centroid(.val[chull,coords])),
           cent_long = as.numeric(cent[,1]),
           cent_lat = as.numeric(cent[,2])) %>%
    select(-chull, -cent)


  new_cubble(
    out,
    key = key,
    index = index(data),
    coords = coords,
    row_id = row_id(data),
    spatial = NULL,
    form = "nested"
  )
}


