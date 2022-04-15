
#' @rdname cubble-class
#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {

  key <- key_vars(data)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(data)[[.x]])))

  check <- check_coords(data)
  bbox <- check$bbox

  bbox_msg <- glue::glue("[{bbox}]")

  if (is_nested(data)){
    ts_size <- map_dbl(data$ts, vec_size) != 0
    var_names <- names(data$ts[ts_size][[1]])
    ts <- data$ts[ts_size][[1]]
    var_type <- map(ts, tibble::type_sum)

  } else if (is_long(data)){
    sp <- spatial(data)
    all <- map(sp, tibble::type_sum)
    all <- all[names(all) != key]
    var_names <- names(all)
    var_type <- all

  }
  var_msg <- glue::glue_collapse(glue::glue("{var_names} [{var_type}]"), sep = ", ")

  size <- tibble::size_sum(data)
  if(is_nested(data)){
    msg <- glue::glue("{key} [{key_n}]: nested form")
  } else if(is_long(data)){
    index <- index(data) %>% paste0(collapse = ", ")
    msg <- glue::glue("{index}, {key} [{key_n}]: long form")
  }

  if ("tbl_ts" %in% class(data)){
    msg <- glue::glue("{msg} [tsibble]")
  }

  if (is_nested(data)) {
    c("cubble" = msg, "bbox" = bbox_msg, "temporal" = var_msg)
  } else if (is_long(data)) {
    c("cubble" = msg, "bbox" = bbox_msg, "spatial" = var_msg)
  }


}

#' @rdname cubble-class
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}


check_coords <- function(data, long_tol = 10, lat_tol = 10){
  test_cubble(data)

  if (form(data) == "nested"){
    dt <- as_tibble(data)
    if (".val" %in% names(dt)) dt <- dt %>% unnest(.data$.val)


  } else if (form(data) == "long"){
    dt <- spatial(data)
  }

  long <- sort(dt[[coords(data)[1]]])
  lat <- sort(dt[[coords(data)[2]]])

  long_diff <- long - dplyr::lag(long)
  lat_diff <- lat - dplyr::lag(lat)

  detect_long_gap <- any(long_diff > long_tol, na.rm = TRUE)
  detect_lat_gap <- any(lat_diff > lat_tol, na.rm = TRUE)


  bbox_string <- check_bbox_digits(range(long), range(lat))
  bbox <- glue::glue_collapse(bbox_string, sep = ", ")

  if (detect_long_gap & detect_lat_gap){
    signal <- glue::glue("- check gap on {coords(data)[1]} and {coords(data)[2]}")
  } else if (detect_long_gap){
    signal <- glue::glue("- check gap on {coords(data)[1]}")
  } else if (detect_lat_gap){
    signal <- glue::glue("- check gap on {coords(data)[2]}")
  } else{
    signal <- ""
  }

  list(bbox = bbox, msg = signal)
}


round_towards_inf <- function(x){
  sign(x) * ceiling(abs(x) * 100)/ 100
}

round_towards_zero <- function(x){
  sign(x) * floor(abs(x)  * 100)/ 100
}

round_upper <- function(x) {
  if (x > 0){
    round_towards_inf(x)
  } else{
    round_towards_zero(x)
  }
}

round_lower <- function(x){
  if (x > 0){
    round_towards_zero(x)
  } else{
    round_towards_inf(x)
  }
}


check_bbox_digits <- function(long_rg, lat_rg){
  if (any(nchar(sub(".*\\.", "", x = long_rg)) >= 2)){
    long_l <- round_lower(long_rg[1])
    long_h <- round_upper(long_rg[2])
  }

  if (any(nchar(sub(".*\\.", "", x = lat_rg)) >= 2)){
    lat_l <- round_lower(lat_rg[1])
    lat_h <- round_upper(lat_rg[2])
  }

  c(long_l, lat_l, long_h, lat_h)

}
