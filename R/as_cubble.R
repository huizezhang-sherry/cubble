#' @rdname cubble-class
#' @importFrom tidyr unchop
#' @importFrom tsibble key_vars index
#' @export
#' @examples
#' # Declaimer: to make the examples easier, here we first `climate_flat` into
#' # different classes and show how they can be casted into a cubble. This is to
#' # demonstrate if your data come in one of the classes, it can be directly cast
#' # into a cubble. By no mean you need to first transform your data into any of
#' # the following class and then cast it to cubble.
#'
#' # If the data is in a tibble:
#' climate_flat %>% as_cubble(key = id, index = date, coords = c(long, lat))
#'
#' # If the spatial and temporal information are in two separate tables:
#' library(dplyr)
#' spatial <- climate_flat %>% select(id:wmo_id) %>% distinct()
#' temporal <- climate_flat %>% select(id, date: tmin) %>% filter(id != "ASN00009021")
#' as_cubble(data = list(spatial = spatial, temporal = temporal),
#'           key = id, index = date, coords = c(long, lat))
#'
#' # If the data is already in a rowwise_df:
#' dt <- climate_flat %>%
#'   tidyr::nest(ts = date:tmin) %>%
#'   dplyr::rowwise()
#' dt %>% as_cubble(key = id, index = date, coords = c(long, lat))
#'
#' # If the data is already in a tsibble, only need to supply `coords`
#' dt <- climate_flat %>% tsibble::as_tsibble(key = id, index = date)
#' dt %>% as_cubble(coords = c(long, lat))
#'
#' # If the data is in netcdf:
#' path <- system.file("ncdf/era5-pressure.nc", package = "weatherdata")
#' raw <- ncdf4::nc_open(path)
#' dt <- as_cubble(raw, vars = c("q", "z"))
as_cubble <- function(data, key, index, coords, ...) {
  UseMethod("as_cubble")
}

#' @rdname cubble-class
#' @export
as_cubble.list <- function(data, key, index, coords, ...){
  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  test_missing(quo = key, var = "key")
  test_missing(quo = index, var = "index")
  test_missing(quo = coords, var = "coords")

  if (length(data) > 2){
    cli::cli_abort("Currently cubble can only take two element for the list input.")
  }
  # now specific list(spatial = ..., temporal = ...)
  spatial <- data$spatial
  temporal <- data$temporal


  var_names <- map(data, colnames) %>% unlist()
  common <- var_names %>% duplicated()
  shared <- unname(var_names[common])

  if (length(shared) == 0){
    cli::cli_abort("Inputs data in the list need to have at least one shared column.")
  }

  #temporal <- temporal %>% tidyr::nest(ts = -shared)

  spatial_key_lvl <- spatial[[as_name(key)]]
  temporal_key_lvl <- temporal[[as_name(key)]]
  only_spatial <- setdiff(spatial_key_lvl, temporal_key_lvl)
  only_temporal <- setdiff(temporal_key_lvl, spatial_key_lvl)

  if (length(only_temporal) != 0){
    cli::cli_alert_warning("Some sites in the temporal table don't have corresponding spatial information")
  }

  if (length(only_spatial) != 0){
    cli::cli_alert_warning("Some sites in the spatial table don't have corresponding temporal information")
  }

  ts <- temporal
  out <- spatial %>% dplyr::nest_join(ts, by = shared)
  coords <- names(out)[tidyselect::eval_select(coords, out)]

  new_cubble(out,
             key = as_name(key), index = as_name(index), coords = coords,
             spatial = NULL, form = "nested")
}

#' @rdname cubble-class
#' @export
as_cubble.tbl_df <- function(data, key, index, coords, ...) {
  if (inherits(data, "tbl_ts")){
    key <- sym(tsibble::key_vars(data))
    index <- sym(tsibble::index(data))
  } else{
    key <- enquo(key)
    index <- enquo(index)
  }
  coords <- enquo(coords)
  coords <- names(data)[tidyselect::eval_select(coords, data)]
  # - check lat between -90 to 90
  # - check long between -180 to 180?
  # - give it an attribution on the range? 0 to 360 or -180 to 180

  # check if date is already nested in the list-column
  col_type <- map(data, class)
  listcol_var <- names(col_type)[col_type == "list"]

  if (length(listcol_var) == 0){
    all_vars <- find_invariant(data, !!key)

    out <- data %>%
      tidyr::nest(ts = c(!!!all_vars$variant)) %>%
      dplyr::rowwise()

  } else{
    listcol_var <- listcol_var[1]
    invariant_var <- names(col_type)[col_type != "list"]
    chopped <- data %>% tidyr::unchop(listcol_var)
    already <- as_name(index) %in% names(chopped$ts)

    out <- data
    variant <- chopped$ts %>% map_chr(pillar::type_sum)
  }

  new_cubble(out,
             key = as_name(key), index = as_name(index), coords = coords,
             spatial = NULL, form = "nested")
}

#' @rdname cubble-class
#' @export
as_cubble.rowwise_df <- function(data, key, index, coords, ...) {
  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  test_missing(quo = key, var = "key")
  test_missing(quo = index, var = "index")
  test_missing(quo = coords, var = "coords")

  # check presents in the data
  # checks for key
  # checks for index
  # checks for coords
  coords <- names(data)[tidyselect::eval_select(coords, data)]

  # if (any(duplicated(data[[as_name(key)]]))){
  #   abort("Make sure each row identifies a key!")
  # }

  # compute leaves
  #leaves <- as_tibble(data) %>% tidyr::unnest() %>% new_leaves(!!key)
  list_col <- get_listcol(data)

  if (length(list_col) == 0){
    abort("Can't identify the list-column, prepare the data as a rowwise_df with a list column")
  } else if (length (list_col) > 2){
    abort("Cubble currently can only deal with at most two list columns")
  } else{
    nested_names <- Reduce(union, map(data[[as_name(list_col)]], names))
    if (any(nested_names == as_name(key))){
      data <- data %>%
        mutate(!!list_col := list(!!ensym(list_col) %>% select(-!!key)))
    }
  }

  new_cubble(data,
             key = as_name(key), index = as_name(index), coords = coords,
             spatial = NULL, form = "nested")
}

#' @export
as_cubble.ncdf4 <- function(data, key, index, coords, vars,
                            lat_range = NULL, long_range = NULL, ...){

  # extract variables
  lat_raw <- extract_longlat(data)$lat
  long_raw <- extract_longlat(data)$long
  time_raw <- extract_time(data)
  var <- extract_var(data, vars)
  lat_idx <- 1:length(lat_raw)
  long_idx <- 1:length(long_raw)

  # subset long lat if applicable
  if (!is.null(lat_range)) {
    lat_idx <- which(lat_raw %in% lat_range)
    lat_raw <- lat_raw[which(lat_raw %in% lat_range)]
  }
  if (!is.null(long_range)) {
    long_idx <- which(long_raw %in% long_range)
    long_raw <- long_raw[which(long_raw %in% long_range)]
  }
  raw_data <- var$var %>% map(~.x[long_idx, lat_idx,])

  # define dimension and grid
  dim_order <- c(length(long_raw), length(lat_raw) , length(time_raw), length(var$name))
  latlong_grid <- tidyr::expand_grid(lat = lat_raw, long = long_raw) %>%
    dplyr::mutate(id = dplyr::row_number())
  mapping <- tidyr::expand_grid(var = var$name, time = time_raw) %>%
    tidyr::expand_grid(latlong_grid)

  # restructure data into flat
  data <- array(unlist(raw_data), dim = dim_order) %>%
    as.data.frame.table() %>%
    as_tibble() %>%
    dplyr::bind_cols(mapping) %>%
    dplyr::select(id, long, lat, time, var, Freq) %>%
    dplyr::arrange(id) %>%
    tidyr::pivot_wider(names_from = var, values_from = Freq)

  key <- "id"
  all_vars <- find_invariant(data, !!key)

  out <- data %>%
    tidyr::nest(ts = c(!!!all_vars$variant)) %>%
    dplyr::rowwise()

  new_cubble(out,
             key = key, index = "time", coords = c("long", "lat"),
             spatial = NULL, form = "nested")
}

