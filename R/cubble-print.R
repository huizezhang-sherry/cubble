#' Print methods
#' @inheritParams base::format
#' @inheritParams base::print
#' @importFrom  tibble tbl_sum
#' @rdname cubble-print
#' @export
#' @examples
#' climate_mel # a nested/spatial cubble
#' face_temporal(climate_mel) # a long/temporal cubble
print.cubble_df <- function(x, width = NULL, ...){
  # ref: https://github.com/r-lib/pillar/blob/main/R/tbl-format.R
  writeLines(format(x, width = width, ...))
}

#' @rdname cubble-print
#' @export
tbl_sum.spatial_cubble_df <- function(x){
  key <- key_vars(x)
  key_n <- nrow(key_data(x))
  index <- index(x)

  # header line 1
  line1 <- glue::glue("key: {key} [{key_n}], index: {index}, nested form")
  if ("rowwise_df" %in% class(x)){
    line1 <- glue::glue("{line1}, groups: rowwise")
  } else if ("groups" %in% names(attributes(x))){
    group_var <- head(names(x %@% groups), -1)
    group_n <- nrow(x %@% groups)
    if (all(group_var != key)) line1 <- glue::glue("{line1}, groups: {group_var} [{group_n}]")
  }
  if (is_sf(x)) line1 <- glue::glue("{line1}, [sf]")


  # header line 2 - print bbox
  x_is_sf <- is_sf(x)
  if (!x_is_sf) {
    coord_vars <- coords(x)
    # when there are two keys
    if (all(!coord_vars %in% names(x))) x <- x %>% unnest(.val)
    x <- as_tibble(x) %>% sf::st_as_sf(coords = coord_vars)
  }

  line2 <- glue::glue("[", paste0(sf::st_bbox(x), collapse = ", "), "]")
  if (!x_is_sf) {
    line2 <- glue::glue(line2, ", Missing CRS!")
  } else{
    line2 <- glue::glue(line2, ", {sf::st_crs(x, parameters = TRUE)$Name}")
  }

  # header line 3: temporal variables
  all <- map(x$ts[[1]], tibble::type_sum)
  line3 <- glue::glue_collapse(
    glue::glue("{names(all)} [{all}]"), sep = ", ")

  c("cubble" = line1, "spatial" = line2, "temporal" = line3)

}

#' @rdname cubble-print
#' @export
tbl_sum.temporal_cubble_df <- function(x){

  key <- key_vars(x)[1]
  key_n <- nrow(spatial(x))
  index <- index(x)

  # header line 1
  line1 <- glue::glue("key: {key} [{key_n}], index: {index}, long form")
  if ("rowwise_df" %in% class(x)){
    line1 <- glue::glue("{line1}, groups: rowwise")
  } else if ("groups" %in% names(attributes(x))){
    group_var <- head(names(x %@% groups), -1)
    group_n <- nrow(x %@% groups)
    if (!key %in% group_var) {
      group_var <- paste0(group_var, collapse = ", ")
      line1 <- glue::glue("{line1}, groups: {group_var} [{group_n}]")
    }
  }
  if (is_tsibble(x)) line1 <- glue::glue("{line1}, [tsibble]")

  # line 2: FROM -- TO [BY] HAS_GAP
  if (!is_tsibble(x)) {
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

  c("cubble" = line1, "temporal" = line2, "spatial" = line3)

}


#' Predicate functions on the object class
#' @param data an object to test for the class
#' @return a logical value of TRUE/FALSE
#' @rdname check-class
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

#' @rdname check-class
#' @export
is_cubble_spatial <- function(data) inherits(data, "spatial_cubble_df")

#' @rdname check-class
#' @export
is_cubble_temporal <- function(data) inherits(data, "temporal_cubble_df")

#' @rdname check-class
#' @export
is_sf <- function(data) inherits(data, "sf")

#' @rdname check-class
#' @export
is_tsibble <- function(data) inherits(data, "tbl_ts")



