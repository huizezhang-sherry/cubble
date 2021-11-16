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
             spatial = NULL, form = "nested")

}

new_cubble <- function(data, key, index, coords, spatial, form, tsibble_attr = NULL) {
  key_data <- group_data(dplyr::grouped_df(data, vars = unlist(map(key, as_name))))

  attr <- list(x = data,
               groups = key_data, index = index,
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

    # `key` attribute is not included since it is already there
    # tsibble_attr <- list(data %@% "index",
    #                      data %@% "index2",
    #                      data %@% "interval")
    # attr$class <- c(attr$class, "tbl_ts")
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


check_coords <- function(data, long_tol = 10, lat_tol = 5){

  test_cubble(data)


  if (form(data) == "nested"){
    dt <- data
  } else if (form(data) == "long"){
    dt <- spatial(data)
  }

  long <- sort(dt[[coords(data)[1]]])
  lat <- sort(dt[[coords(data)[2]]])

  long_diff <- long - lag(long)
  lat_diff <- lat - lag(lat)

  detect_long_gap <- any(long_diff > long_tol, na.rm = TRUE)
  detect_lat_gap <- any(lat_diff > lat_tol, na.rm = TRUE)

  bbox <- glue::glue_collapse(c(range(long)[1], range(lat)[1],
                                range(long)[2], range(lat)[2]), sep = ", ")

  if (detect_long_gap & detect_lat_gap){
    signal <- glue::glue("- check gap on {coords(data)[1]} and {coords(data)[2]}")
  } else if (detect_long_gap){
    signal <- glue::glue("- check gap on {coords(data)[2]}")
  } else if (detect_lat_gap){
    signal <- glue::glue("- check gap on {coords(data)[1]}")
  } else{
    signal <- ""
  }

  list(bbox = bbox, msg = signal)
}
