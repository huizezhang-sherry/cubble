#' print a cubble object
#' @param x,width,n_extra,n,max_extra_cols,max_footer_lines see pillar tbl-format.R
#' @param ... other argument to pass into `format()`
#' @importFrom  tibble tbl_sum
#' @rdname cubble-print
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


#' @importFrom  tibble tbl_sum
#' @rdname cubble-print
#' @export
tbl_sum.cubble_df <- function(x) {
  NextMethod()
}

#' @rdname cubble-print
#' @export
tbl_sum.spatial_cubble_df <- function(x){
  key <- key_vars(x)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(x)[[.x]])))
  index <- index(x)

  # header line 1
  line1 <- glue::glue("key: {key} [{key_n}], index: {index}, nested form")
  if (inherits(x, "sf")) line1 <- glue::glue("{line1} [sf]")


  # header line 2 - print bbox
  if (!inherits(x, "sf")) {
    coord_vars <- coords(x)
    # when there are two keys
    if (all(!coord_vars %in% names(x))) x <- x %>% unnest(.val)
    x <- as_tibble(x) %>% sf::st_as_sf(coords = coord_vars)
  }

  line2 <- glue::glue("[", paste0(sf::st_bbox(x), collapse = ", "), "]")

  # header line 3: temporal variables
  all <- map(x$ts[[1]], tibble::type_sum)
  temporal_vars <- all[names(all) != index]
  line3 <- glue::glue_collapse(
    glue::glue("{names(temporal_vars)} [{temporal_vars}]"), sep = ", ")

  c("cubble" = line1, "extent" = line2, "temporal" = line3)

}

#' @rdname cubble-print
#' @export
tbl_sum.temporal_cubble_df <- function(x){

  key <- key_vars(x)[1]
  key_n <- map_dbl(key, ~length(unique(key_data(x)[[.x]])))
  index <- index(x)

  # header line 1
  line1 <- glue::glue("key: {key} [{key_n}], index: {index}, long form")
  if (inherits(x, "tbl_ts")) line1 <- glue::glue("{line1} [tsibble]")

  # line 2: FROM -- TO [BY] HAS_GAP
  if (!inherits(x, "tbl_ts")) {
    x_tsibble <- as_tsibble(as_tibble(x), key = key_vars(x), index = index(x))
  } else{
    x_tsibble <- x
  }
  from_to <- range(as_tibble(x)[[cubble::index(x)]])
  by <- tsibble::interval(x_tsibble)
  gaps <- tsibble::scan_gaps(x_tsibble)
  if (nrow(gaps) == 0) gap_msg <- "no gaps" else gap_msg <- "has gaps!"
  line2 <- glue::glue(
    paste0(from_to, collapse = " -- "), " [", {format(by)}, "], ", {gap_msg})


  # line 3: spatial variables
  all <- map(spatial(x), tibble::type_sum)
  spatial_vars <- all[names(all) != key]
  line3 <- glue::glue_collapse(
    glue::glue("{names(spatial_vars)} [{spatial_vars}]"), sep = ", ")

  c("cubble" = line1, "extent" = line2, "spatial" = line3)

}


#' Check if the object is a cubble or its subclass
#' @param data the object to test
#' @return a TRUE/FALSE predicate
#' @rdname check-cubble-class
#' @export
#' @examples
#' is_cubble(stations)
#' is_cubble(meteo)
#' is_cubble(climate_flat)
#' is_cubble(climate_mel)
#' is_cubble(climate_aus)
#' is_cubble_spatial(climate_aus)
#' is_cubble_temporal(climate_aus)
is_cubble <- function(data) inherits(data, "cubble_df")

#' @rdname check-cubble-class
#' @export
is_cubble_spatial <- function(data) inherits(data, "spatial_cubble_df")

#' @rdname check-cubble-class
#' @export
is_cubble_temporal <- function(data) inherits(data, "temporal_cubble_df")
