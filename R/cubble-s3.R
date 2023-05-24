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
#' spatial and temporal data, in the syntax of the \code{by} argument in \code{left_join}  (see examples
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
#' make_cubble(spatial = stations, temporal = climate,
#'             key = id, index = date, coords = c(long, lat))
#'
#' # when the key variable is named differently, use the `by` argument,
#' # cubble will take the name from TODO
#' climate2 <- climate %>% rename(station = id)
#' make_cubble(spatial = stations, temporal = climate2,
#'           by = c("id" = "station"), key = id,
#'           index = date, coords = c(long, lat))
cubble <- function(..., key, index, coords) {
  data <- tibble::tibble(!!!list2(...))
  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)
  coords <- names(data)[tidyselect::eval_select(coords, data)]

  all_vars <- find_invariant(data, !!key)
  data <- data %>%
    tidyr::nest(ts = c(!!!all_vars$variant)) %>%
    dplyr::rowwise()

  #validate_cubble(data, key = as_name(key), index = as_name(index), coords = coords , ...)
  new_spatial_cubble(data,
             key = as_name(key), index = as_name(index), coords = coords,
             spatial = NULL, form = "nested")

}

#' @rdname cubble-class
#' @export
make_cubble <- function(spatial, temporal, by = NULL, key, index, coords){

  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  test_missing(quo = key, var = "key")
  key_nm <- as_name(key)
  test_missing(quo = index, var = "index")
  # parse coords from a quosure to a string vector
  coords <- as.list(quo_get_expr(coords))[-1]
  coords <- unlist(map(coords, as_string))

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
  spatial <- spatial %>% filter(!by %in% only_spatial) %>%
    # from discussion: https://github.com/r-spatial/sf/issues/951
    # to ensure the sf is built from a tibble
    as_tibble() %>% sf::st_as_sf()
  temporal <- temporal %>% filter(!by %in% only_temporal)

  out <- suppressMessages(
    dplyr::inner_join(spatial, temporal %>% nest(ts = -by))
  )

  new_spatial_cubble(out,
             key = by, index = as_name(index), coords = coords,
             spatial = NULL, form = "nested")
}



# new_cubble <- function(data, ..., class = NULL){
#
#
#
# }

new_spatial_cubble <- function(data,  ..., class = NULL){

  args <- list2(...)
  groups <- grouped_df(data, args$key) %>% group_data()
  out <- new_rowwise_df(data, groups = groups, ...)
  class(out) <- c("spatial_cubble_df", "cubble_df", class(data))
  out
}


new_temporal_cubble <- function(data, ..., class = NULL){

  args <- list2(...)
  groups <- grouped_df(data, args$key) %>% group_data()
  data <- new_grouped_df(data, groups = groups, ...)
  class(data) <- c("temporal_cubble_df", "cubble_df", class(data))
  data
}
#
# validate_cubble <- function(data, key, index, coords, ...){
#
#  # all the checks go here
# }


#' Cubble constructor
#' @param data data
#' @param key key
#' @param index key
#' @param coords key
#' @param spatial key
#' @param form key
#' @param tsibble_attr key
#' @export
new_cubble <- function(data, key, index, coords, spatial, form, tsibble_attr = NULL){
  data <- arrange(data, !!sym(key[1]))

  # take ordered as TRUE, for now
  attr(index, "ordered") <- TRUE
  if (form == "nested" & ".val" %in% names(data)){
    group_dt <- data |> tidyr::unnest(.data$.val)
  } else{
    group_dt <- data
  }

  key_data <- group_dt |> dplyr::grouped_df(key) |> group_data()

  all_cols <- names(data)

  # if (form == "nested"){
  #   unique_key <- unique(data[[key]]) |> length()
  #   if (unique_key != nrow(data) & !is.null(row_id(data))){
  #     data <- data |> ungroup() |> rearrange_index(key = key, old_key = row_id)
  #   }
  # }

  if (length(coords) == 1){
      others <- all_cols[!all_cols %in% c(key, coords, "ts")]
      ordered <- c(key, coords, others, "ts")
      data <- data |> select(ordered)
  }

  attr <- list(x = data,
               groups = key_data, index = index,
               spatial = spatial, coords = coords, form = form,
               class = "cubble_df") |>
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
  if (inherits(data, "tbl_ts")){
    attr$class <- c(attr$class, "tbl_ts")
    attr <- c(attr, tsibble_attr)
  }

  if (inherits(data, "sf")){
    sf_attr <- attributes(data)
    attr$class <- c(attr$class, "sf")
    attr <- c(attr, sf_column = list(sf_attr$sf_column), agr = list(sf_attr$agr))
  }


  if (form == "nested"){
    names(attr)[1] <- "data"
    out <- rlang::exec("new_rowwise_df", !!!attr)
  } else if (form == "long"){
    out <- rlang::exec("new_grouped_df", !!!attr)
  }

  out
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
`names<-.cubble_df` <- function(x, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}


`[[.cubble_df` <- function(x, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}

`[[<-.cubble_df` <- function(x, value){
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}
