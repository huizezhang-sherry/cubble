#' Constructor for the cubble class
#'
#' @param ... a list object to create new cubble
#' @param data the object to be created or tested as cubble
#' @param key the spatial identifier
#' @param index the time identifier
#' @param coords the coordinates that characterise the spatial dimension
#' @rdname cubble-class
#' @examples
#' climate_flat %>%
#'   as_cubble(key = id, index = date, coords = c(long, lat))
#' @export
cubble <- function(..., key, index, coords) {
  data <- tibble::tibble(!!!list2(...))
  key <- enquo(key)
  new_cubble(data,
             key = as_name(key), index = as_name(index), coords = coords,
             row_id = row_id, spatial = NULL, form = "nested")

}

new_cubble <- function(data, key, index, coords, spatial, form, row_id, tsibble_attr = NULL) {
  key_data <- group_data(dplyr::grouped_df(data, vars = unlist(map(key, as_name))))

  all_cols <- names(data)

  if (form == "nested"){
    unique_key <- unique(data[[key]]) %>% length()
    if (unique_key != nrow(data) & !is.null(row_id(data))){
      data <- data %>% ungroup() %>% rearrange_index(key = key, old_key = row_id)
    }
  }


  if (length(coords) == 1){
    if ("sfc" %in% class(data[[coords]])){
        converted <- convert_sfc_to_ll(data, coords)
        data <- converted$data
        coords <- converted$coords

        others <- all_cols[!all_cols %in% c(key, ".val", "ts", "geom")]
        ordered <- c(key, ".val", others, "ts")
        data <- data %>% select(ordered)

    } else{
      others <- all_cols[!all_cols %in% c(key, coords, "ts")]
      ordered <- c(key, coords, others, "ts")
      data <- data %>% select(ordered)
    }

  }


  attr <- list(x = data,
               groups = key_data, index = index, row_id = row_id,
               spatial = spatial, coords = coords, form = form,
               class = "cubble_df") %>%
    Filter(f = length)

  # check column ts present,
  if (form == "nested"){
    # also need to check
    # * ts is a list column
    # * ts column contain index
    if (!"ts" %in% names(data)){
      cli::cli_abort("data need to contain a {.code ts} column to construct a cubble")
    }

  }

  #tsibble_attr <- NULL
  if ("tbl_ts" %in% class(data)){
    attr$class <- c(attr$class, "tbl_ts")
    attr <- c(attr, tsibble_attr)
  }


  if (form == "nested"){
    names(attr)[1] <- "data"
    out <- rlang::exec("new_rowwise_df", !!!attr)
  } else if (form == "long"){
    out <- rlang::exec("new_grouped_df", !!!attr)
  }

  out
}

#' @rdname cubble-class
#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {

  key <- key_vars(data)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(data)[[.x]])))

  check <- check_coords(data)
  signal <- check$msg
  bbox <- check$bbox

  bbox_msg <- glue::glue("[{bbox}]{signal}")

  if (form(data) == "nested"){
    var_names <- Reduce(unique, map(data$ts, names))
    ts <- data$ts
    var_type <- Reduce(unique, map(1:length(ts), ~map(ts[[.x]], tibble::type_sum)))

  } else if (form(data) == "long"){

    sp <- spatial(data)
    all <- map(sp, tibble::type_sum)
    all <- all[names(all) != key]
    var_names <- names(all)
    var_type <- all

  }
  var_msg <- glue::glue_collapse(glue::glue("{var_names} [{var_type}]"), sep = ", ")

  size <- tibble::size_sum(data)
  if(form(data) == "nested"){
    msg <- glue::glue("{key} [{key_n}]: nested form")
  } else if(form(data) == "long"){
    index <- index(data)
    msg <- glue::glue("{index}, {key} [{key_n}]: long form")
  }

  if ("tbl_ts" %in% class(data)){
    msg <- glue::glue("{msg} [tsibble]")
  }

  if (form(data) == "nested"){
    c("cubble" = msg, "bbox" = bbox_msg, "temporal" = var_msg)
  } else{
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
    if (".val" %in% names(dt)) dt <- dt %>% unnest(.val)


  } else if (form(data) == "long"){
    dt <- spatial(data)
  }

  long <- sort(dt[[coords(data)[1]]])
  lat <- sort(dt[[coords(data)[2]]])

  long_diff <- long - lag(long)
  lat_diff <- lat - lag(lat)

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
