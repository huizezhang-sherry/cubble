#' Switch to a different key of a cubble
#'
#' `switch_key()` allows you select a new variable in the data to become the key.
#' This can be used to create hierarchical data where one variable is nested in another.
#' @param data a cubble object, can be either long or nested cubble
#' @param key the new key
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' # create an artificial cluster for stations
#' set.seed(1234)
#' cb <- climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat)) %>%
#'   mutate(cluster = sample(1:3, 1))
#'
#' # switch the key to cluster
#' cb_hier <- cb %>% switch_key(cluster)
#' @importFrom tidyr unpack unchop
#' @export
switch_key <- function(data, key){
  test_cubble(data)
  new_key <- as_name(enquo(key))
  cur_key <-  key_vars(data)
  key <- unique(c(new_key, cur_key))
  index <- index(data)
  coords <- coords(data)

  orig_form <- form(data)
  if (orig_form == "long") data <- data %>% tamp()

  if (".val" %in% names(data)) data <- data %>% tidyr::unnest(.data$.val)

  if (!new_key %in% names(data)){
    cli::cli_abort("{.field {key_upper}} does not exist in the data!")
  }

  out <- arrange_temporal(data, key) %>% arrange_spatial()

  out_cubble <- new_cubble(
    out, key = key, index = index, coords = coords,
    spatial = NULL, form = "nested")

  if (orig_form == "long") out_cubble <- out_cubble %>% stretch(.data$ts)

  out_cubble
}

arrange_temporal <- function(data, key){
  # organising keys
  data <- as_tibble(data)
  new_key <- key[[1]]
  cur_key <- key[[2]]
  key_lvl <- key_level(data, key)
  upper_key <- key[key_lvl == min(key_lvl)]
  lower_key<-  key[key_lvl == max(key_lvl)]

  # prep ts column
  if (new_key == upper_key){
    ts_df <- data %>% dplyr::select(!!key,.data$ts) %>% tibble::as_tibble()
  } else if (new_key == lower_key){
    ts_df <- data %>% dplyr::select(new_key, .data$ts) %>% tidyr::unnest(.data$ts)
  }

  # split ts column as per the new group
  out_ts <- vctrs::vec_split(ts_df %>% select(-new_key), ts_df[,new_key]) %>%
    tibble::as_tibble() %>%
    tidyr::unpack(key)

  ## finalising
  if (new_key == upper_key){
    out_ts <- out_ts %>%
      dplyr::mutate(ts = map(.data$val, ~tidyr::unchop(.x, .data$ts) %>% tidyr::unpack(.data$ts))) %>%
      dplyr::select(-.data$val)
  } else if (new_key ==lower_key){
    out_ts <- out_ts %>% dplyr::rename(ts = .data$val)
  }

  list(ts = out_ts, sp = data %>% select(-.data$ts), key_vec =key)
}

arrange_spatial <- function(temporal_res){

  ts <- temporal_res$ts
  data <- temporal_res$sp
  key <- temporal_res$key_vec

  # organising keys
  new_key <- key[[1]]
  cur_key <- key[[2]]

  inv <- find_invariant(data, !!new_key)
  other_cols <- names(data)[!names(data) %in% c(inv$invariant, "ts")]

  # organising spatial variables into the new group
  if(nrow(data) > nrow(ts)){
    out <- vctrs::vec_split(data[,other_cols], data[,inv$invariant]) %>%
      tibble::as_tibble() %>%
      rename(.val = .data$val) %>%
      tidyr::unpack(key) # vec_split names the split column as "key"
  } else {
    out <- data
  }

  out <- out %>%
    dplyr::left_join(ts, by = new_key) %>%
    dplyr::arrange(!!new_key)
  out <- out[c(new_key, setdiff(names(out), new_key))]

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
             spatial = spatial(data), form = determine_form(data))
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
    spatial = NULL,
    form = "nested"
  )
}


