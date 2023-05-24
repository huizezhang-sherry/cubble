#' print a cubble object
#' @rdname cubble-print
#' @param x,width,n_extra,n,max_extra_cols,max_footer_lines see pillar tbl-format.R
#' @param ... other argument to pass into `format()`
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

#' @rdname cubble-print
#' @importFrom  tibble tbl_sum
#' @return a cubble object
#' @export
tbl_sum.cubble_df <- function(x) {

  data <- x
  key <- key_vars(data)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(data)[[.x]])))

  # header line 1
  index <- index(data) |> paste0(collapse = ", ")
  msg <- glue::glue("key: {key} [{key_n}], index: {index}, {form(data)} form")
  if (inherits(data, "tbl_ts")){
    msg <- glue::glue("{msg} [tsibble]")
  } else if (inherits(data, "sf")){
    msg <- glue::glue("{msg} [sf]")
  }

  # header line 2
  # bbox if in the nested form
  if (is_nested(data)){
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
    extent_msg <- glue::glue("[", paste0(bbox, collapse = ", "), "]")
  } else{
    if (form(data) == "nested") long <- face_temporal(data) else long <- data
    # temporal extent if in the long form
    if (!inherits(data, "tbl_ts")){
      long <- as_tsibble(long, key = key_vars(data), index = index(data)[1])
    }
    from_to <- range(as_tibble(long)[[cubble::index(long)]])
    by <- tsibble::interval(long)
    gaps <- tsibble::scan_gaps(long)
    if (nrow(gaps) == 0) gap_msg <- "no gaps" else gap_msg <- "has gaps!"
    extent_msg <- glue::glue(
      paste0(from_to, collapse = " -- "), " [", {format(by)}, "], ", {gap_msg})
  }

  # header line 3: other variables
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

  if (is_nested(data)){
    c("cubble" = msg, "extent" = extent_msg, "temporal" = var_msg)
  } else if (is_long(data)) {
    c("cubble" = msg, "extent" = extent_msg, "spatial" = var_msg)
  }


}

#' @rdname cubble-class
#' @return a TRUE/FALSE predicate
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}

