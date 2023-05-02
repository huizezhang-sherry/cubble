#' @rdname cubble-class
#' @param x,width,n_extra,n,max_extra_cols,max_footer_lines see pillar tbl-format.R
#' @importFrom  tibble tbl_sum
#' @return a cubble object
#' @export
print.cubble_df <- function(x, width = NULL, ...,
                            n_extra = NULL,
                            n = NULL, max_extra_cols = NULL, max_footer_lines = NULL){
  # ref: https://github.com/r-lib/pillar/blob/main/R/tbl-format.R
  writeLines(format(
    x,
    width = width, ...,
    n = n, max_extra_cols = max_extra_cols, max_footer_lines = max_footer_lines
  ))
}

#' @rdname cubble-class
#' @importFrom  tibble tbl_sum
#' @return a cubble object
#' @export
tbl_sum.cubble_df <- function(x) {

  data <- x
  key <- key_vars(data)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(data)[[.x]])))

  #check <- check_coords(data)
  #bbox <- check$bbox
  if (form(data) == "long") nested <- spatial(data) else nested <- data

  if (inherits(data, "sf")){
    bbox <- sf::st_bbox(nested)
  } else{
    coord_vars <- coords(data)
    if (all(!coord_vars %in% names(nested))) nested <- nested %>% unnest(.val)
    bbox <- as_tibble(nested) %>%
      sf::st_as_sf(coords = coord_vars) %>%
      sf::st_bbox()
  }

  bbox_msg <- glue::glue("[", paste0(bbox, collapse = ", "), "]")

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
    index <- index(data) |> paste0(collapse = ", ")
    msg <- glue::glue("{index}, {key} [{key_n}]: long form")
  }

  if (inherits(data, "tbl_ts")){
    msg <- glue::glue("{msg} [tsibble]")
  } else if (inherits(data, "sf")){
    msg <- glue::glue("{msg} [sf]")
  }

  if (is_nested(data)) {
    c("cubble" = msg, "bbox" = bbox_msg, "temporal" = var_msg)
  } else if (is_long(data)) {
    c("cubble" = msg, "bbox" = bbox_msg, "spatial" = var_msg)
  }


}

#' @rdname cubble-class
#' @return a TRUE/FALSE predicate
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}

