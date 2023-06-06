#' Create a cubble object
#'
#' @param ... a list object to create new cubble
#' @param spatial a tibble of spatial variables, contain key and the two coords columns
#' @param temporal a tibble of temporal variables, contain key and index
#' @param key the variable(s) that identifies the spatial location.
#' @param index the single variable that identifies time, currently support
#' base R classes \code{Date}, \code{POSIXlt}, \code{POSIXct} and
#' tsibble's \code{yearmonth}, \code{yearweek}, and \code{yearquarter} class,
#' supply in non-standard evaluation
#' @param coords the coordinate columns, in the form of \code{c(LONGITUDE, LATITUDE)}
#' the argument can be omitted if created from an sf and its subclasses.
#' In case the sf geometry column is not POINT, cubble will use the centroid
#' coordinates as LONGITUDE and LATITUDE
#' @param by used in `make_cubble` when the key variable has different names in the
#' spatial and temporal data, in the syntax of the \code{by} argument in \code{left_join}  (see examples)
#' @rdname cubble-class
#' @return a cubble object
#' @export
#' @examples
#' cubble(
#'   id = rep(c("perth", "melbourne", "sydney"), each = 3),
#'   date = rep(as.Date("2020-01-01") + 0:2, times = 3),
#'   long = rep(c(115.86, 144.96, 151.21), each = 3),
#'   lat = rep(c(-31.95, -37.81, -33.87), each = 3),
#'   value = rnorm(n = 9),
#'   key = id, index = date, coords = c(long, lat)
#'   )
#'
#' # stations and climate are in-built data in cubble
#' make_cubble(spatial = stations, temporal = meteo,
#'             key = id, index = date, coords = c(long, lat))
#'
cubble <- function(..., key, index, coords) {
  data <- tibble::tibble(!!!list2(...))
  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)
  coords <- names(data)[tidyselect::eval_select(coords, data)]

  all_vars <- find_invariant(data, !!key)
  data <- data %>%
    tidyr::nest(ts = c(!!index, !!!all_vars$variant)) %>%
    dplyr::rowwise()

  #validate_cubble(data, key = as_name(key), index = as_name(index), coords = coords , ...)
  new_spatial_cubble(
    data, key = as_name(key), index = as_name(index), coords = coords)

}

#' @rdname cubble-class
#' @export
make_cubble <- function(spatial, temporal, by = NULL, key, index, coords){

  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  if (quo_is_missing(key)){
    if (is_tsibble(temporal)){
      key <- key_vars(temporal)
    } else{
      cli::cli_abort("Please supply the {.code key} argument,
                     or a {.code tsibble} object as the temporal compoenent")
    }
  }

  if (quo_is_missing(index)){
    if (is_tsibble(temporal)){
      index <- index(temporal)
    } else{
      cli::cli_abort("Please supply the {.code index} argument,
                     or a {.code tsibble} object as the temporal compoenent")
    }
  }

  if (quo_is_missing(coords)){
    if (is_sf(spatial)){
      cc <- sf::st_coordinates(sf::st_centroid(spatial))
      colnames(cc) = if (sf::st_is_longlat(spatial)) c("long", "lat") else c("x", "y")
      spatial <- cbind(spatial, cc)
      coords <- colnames(cc)
    } else{
      cli::cli_abort("Please supply the {.code coords} argument in the syntax of
      {.code coords = c(LONG, LAT)}, or use an {.code sf} object as the spatial compoenent")
    }
  } else{
    coords <- coords2strvec(coords)
  }


  # find the common "key" column between spatial and temporal
  # if no shared, parse the `by` argument
  common_cols <- intersect(names(spatial), names(temporal))
  if (!is_null(by)) {
    if (by %in% names(temporal) && names(by) %in% names(spatial)) {
      # rename the common column to have the same name
      names(spatial)[names(spatial) == names(by)] <- by
    }
  } else if (length(common_cols) != 0) {
    # use the first common column
    by <- intersect(names(spatial), names(temporal))[1]
  } else{
    cli::cli_abort("No shared column found.
    Please supply the shared key using the {.code by} argument")
  }

  # find whether there are unmatched spatial and temporal key level
  slvl <- spatial[[by]]
  tlvl <- temporal[[by]]
  only_spatial <- setdiff(slvl, tlvl)
  only_temporal <- setdiff(tlvl, slvl)
  has_unmatch <- length(only_temporal) != 0 | length(only_spatial) != 0


  if (length(only_spatial) != 0) cli::cli_alert_warning(
    "Some sites in the spatial table don't have temporal information"
  )

  if (length(only_temporal) != 0) cli::cli_alert_warning(
    "Some sites in the temporal table don't have spatial information"
  )

  if (has_unmatch) cli::cli_alert_warning(
    'Use {.fn check_key} to check on the unmatched key
    The cubble is created only with sites having both spatial and temporal information'
  )

  # only create when have both spatial & temporal info
  spatial <- spatial %>% filter(!by %in% only_spatial)

  if (is_sf(spatial)){
    # from discussion: https://github.com/r-spatial/sf/issues/951
    # to ensure the sf is built from a tibble
    spatial <- spatial %>% as_tibble() %>% sf::st_as_sf()
  }

  if (is_tsibble(temporal)){
    index <- temporal %@% "index"
  } else{
    index <- as_name(index)
  }

  temporal <- temporal %>% filter(!by %in% only_temporal) %>%
    select(as_name(index), setdiff(colnames(temporal), as_name(index)))

  out <- suppressMessages(
    dplyr::inner_join(spatial, temporal %>% nest(ts = -by)) %>% rowwise()
  )

  new_spatial_cubble(
    out, key = by, index = index, coords = coords
    )
}

cb_spatial_cls <- c("spatial_cubble_df", "cubble_df")
cb_temporal_cls <- c("temporal_cubble_df", "cubble_df")

new_spatial_cubble <- function(data,  ..., validate = TRUE, class = NULL){

  args <- list2(...)
  if (validate) validate_spatial_cubble(data, args)
  attr_vars <- c(args$key, args$coords)
  data <- data %>% select(unname(attr_vars), setdiff(colnames(data), c(attr_vars, "ts")), "ts")
  if (is.null(args$groups)){
    groups <- dplyr::grouped_df(data, args$key) %>% dplyr::group_data()
    out <- new_rowwise_df(data, groups = groups, ...)
  } else{
    groups <- args$groups
    out <- new_rowwise_df(data, ...)
  }
  class(out) <- c(cb_spatial_cls, setdiff(class(data), cb_spatial_cls))
  out
}


new_temporal_cubble <- function(data, ..., validate = TRUE, class = NULL){
  args <- list2(...)
  if (validate) validate_temporal_cubble(data, args)

  attr_vars <- c(args$key, args$index)
  suppressWarnings(data <- data[,c(attr_vars, setdiff(colnames(data), attr_vars))])
  if (is.null(args$groups)){
    groups <- dplyr::grouped_df(data, args$key) %>% dplyr::group_data()
    out <- new_grouped_df(data, groups = groups, ...)
  } else{
    groups <- args$groups
    out <- new_grouped_df(data, ...)
  }

  class(out) <- c(cb_temporal_cls, setdiff(class(data), cb_temporal_cls))
  out
}

validate_spatial_cubble <- function(data, args){

  # all the key values will be unique after nest()

  # check key present
  if (any(!args$key %in% colnames(data))){
    cli::cli_abort("The key variable is not present, please fix")
  }

  # check on index
  x <- as_tibble(data)
  dup_index <- map_lgl(x[["ts"]], ~vec_duplicate_any(.x[[args$index]]))
  index_na <- map_lgl(x[["ts"]], ~any(is.na(.x[[args$index]])))

  if (any(dup_index)){
    where_dup <- which(dup_index, TRUE)
    cli::cli_abort("Duplicated index values found with {args$key} = {x[[args$key]][where_dup]}, please fix.")
  }

  if (any(index_na)){
    where_dup <- which(index_na, TRUE)
    cli::cli_abort("The index variable contains {.code NA}, please fix.")
  }

  # check coords present
  if (any(!args$coords %in% colnames(data))){
    cli::cli_abort("At least one of the coordinate columns not present, please fix")
  }


}

validate_temporal_cubble <- function(data, args){

  if (any(!args$key %in% colnames(data))){
    cli::cli_abort("The key variable is not present, please fix")
  }

  x <- as_tibble(data)
  dup_index <- split(x, x[[args$key]]) %>% map_lgl(~vec_duplicate_any(.x[[args$index]]))
  index_na <- any(is.na(x[[args$index]]))

  if (any(dup_index)){
    where_dup <- which(dup_index, TRUE)
    cli::cli_abort("Duplicated index values found with {args$key} = {x[[args$key]][where_dup]}, please fix.")
  }

  if (any(index_na)){
    where_dup <- which(index_na, TRUE)
    cli::cli_abort("The index variable contains {.code NA}, please fix.")
  }

}

#' @export
`[.cubble_df` <- function(data, i, j, drop = FALSE){

  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
  }

  out <- data
  if(!is.null(i)) out <- vec_slice(data, i)
  if(!is.null(j)){
    out <- tibble::as_tibble(out)
    out <- out[,unlist(j)]
  }

  dplyr_reconstruct(out, data)

}

#' @export
`names<-.cubble_df`<- function(x, value){
  out <- `names<-`(as_tibble(x), value)
  dplyr_reconstruct(out, x)
}

`[[.cubble_df` <- function(x, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}

#' @export
`[[<-.cubble_df` <- function(x, i, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}
