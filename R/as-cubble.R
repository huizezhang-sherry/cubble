#' Coerce foreign objects into a cubble object
#' @param ... other arguments.
#' @param data an object to be converted into an cubble object. Currently
#' support objects of classes `tibble`, `ncdf4`, `stars`, and `sftime`.
#' @param key a character (symbol), the spatial identifier,
#' see [cubble::make_cubble()]
#' @param index a character (symbol), the temporal identifier,
#' see [cubble::make_cubble()].
#' @param coords a vector of character (symbol) of length 2,
#' see [cubble::make_cubble()].
#' @param vars a vector of variables to read in (with quote),
#' used in `as_cubble.netcdf()`
#' to select the variable to read in.
#' @param lat_range,long_range in the syntax of `seq(FROM, TO, BY)`
#' to downsample
#' @param crs used in `as_cubble.tbl_df()` to set the crs.
#' the data to read in `as_cubble.netcdf()`.
#' @param dimensions used when creating a cubble from a stars object
#' @importFrom tidyr unchop
#' @importFrom tsibble index
#' @export
#' @return a cubble object
#' @examples
#' climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))
#'
#' # only need `coords` if create from a tsibble
#' dt <- climate_flat |>  tsibble::as_tsibble(key = id, index = date)
#' dt |>  as_cubble(coords = c(long, lat))
#'
#' # netcdf
#' path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
#' raw <- ncdf4::nc_open(path)
#' dt <- as_cubble(raw)
#' # subset degree
#' dt <- as_cubble(raw,vars = c("q", "z"),
#'                 long_range = seq(113, 153, 3),
#'                 lat_range = seq(-53, -12, 3))
#'
#'\dontrun{
#'# stars - take a few seconds to run
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <-  stars::read_stars(tif)
#' x |> as_cubble(index = band)
#'}
#'
#' # don't have to supply coords if create from a sftime
#' dt <- climate_flat |>
#'   sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84")) |>
#'   sftime::st_as_sftime()
#' dt |> as_cubble(key = id, index = date)
as_cubble <- function(data, key, index, coords, ...) {
  UseMethod("as_cubble")
}

#' @rdname as_cubble
#' @export
as_cubble.data.frame <- function(data, key, index, coords, ...){
  key <-  enquo(key)
  index <-  enquo(index)
  coords <-  enquo(coords)
  as_cubble(as_tibble(data), key = !!key, index = !!index, coords = !!coords)
}


#' @rdname as_cubble
#' @export
as_cubble.tbl_df <- function(data, key, index, coords, crs, dimensions, ...) {
  if (is_tsibble(data)){
    key <- sym(tsibble::key_vars(data))
    index <- sym(tsibble::index(data))
  } else{
    key <- enquo(key)
    index <- enquo(index)
  }
  coords <- enquo(coords)
  coords <- names(data)[tidyselect::eval_select(coords, data)]

  # check if date is already nested in the list-column
  col_type <- map(data, class)
  listcol_var <- names(col_type)[col_type == "list"]

  if (length(listcol_var) == 0){
    all_vars <- find_invariant(data, !!key)

    data <- data |>
      tidyr::nest(ts = c(!!!all_vars$variant))

  } else{
    listcol_var <- listcol_var[1]
    if (listcol_var != "ts")
      colnames(data)[colnames(data) == listcol_var] <- "ts"
    chopped <- data |> tidyr::unchop("ts")
    already <- as_name(index) %in% names(chopped[["ts"]])
    if (!already) cli::cli_abort(
      "Can't' find the index variable in the data. Please check."
    )
  }

  res <- new_spatial_cubble(
    data, key = as_name(key), index = as_name(index), coords = coords
  )

  #if (!missing(crs)) res <- res |> make_spatial_sf(crs = crs)
  if (!missing(crs)) attr(res, "crs") <- crs
  if (!missing(dimensions)) attr(res, "dimensions") <- dimensions
  return(res)


}

#' @rdname as_cubble
#' @export
as_cubble.sf <-  function(data, key, index,...) {
	cc <-  sf::st_coordinates(sf::st_centroid(data))
	colnames(cc) <-  if (sf::st_is_longlat(data))
			c("long", "lat")
		else
			c("x", "y")
	sf_column <-  attr(data, "sf_column")
	data <-  cbind(data, cc)
	data <-  as_tibble(data)
	key <-  enquo(key)
	index <-  enquo(index)
	cu <-  as_cubble(data, key = !!key, index = !!index, coords = colnames(cc))
	cb_cls <- c("spatial_cubble_df", "cubble_df")
	structure(cu, class = c(cb_cls, "sf", setdiff(class(cu), cb_cls)),
              sf_column = sf_column)
}

#' @rdname as_cubble
#' @export
as_cubble.ncdf4 <- function(data, key, index, coords, vars,
                            lat_range = NULL, long_range = NULL, ...){

  # extract variables
  lat_raw <- extract_longlat(data)$lat
  long_raw <- extract_longlat(data)$long
  time_raw <- extract_time(data) |> as.Date()
  var <- extract_var(data, vars)
  lat_idx <- 1:length(lat_raw)
  long_idx <- 1:length(long_raw)

  # subset long lat if applicable
  if (!is.null(lat_range)) {
    lat_idx <- which(lat_raw %in% lat_range)
    lat_raw <- as.vector(lat_raw[which(lat_raw %in% lat_range)])
  }
  if (!is.null(long_range)) {
    long_idx <- which(long_raw %in% long_range)
    long_raw <- as.vector(long_raw[which(long_raw %in% long_range)])
  }
  raw_data <- var$var |>  map(~.x[long_idx, lat_idx,])

  # define dimension and grid
  dim_order <- c(length(long_raw), length(lat_raw) ,
                 length(time_raw), length(var$name))
  latlong_grid <- tidyr::expand_grid(lat = lat_raw, long = long_raw) |>
    dplyr::mutate(id = dplyr::row_number())
  mapping <- tidyr::expand_grid(var = var$name, time = time_raw) |>
    tidyr::expand_grid(latlong_grid)

  # restructure data into flat
  data <- array(unlist(raw_data), dim = dim_order) |>
    as.data.frame.table() |>
    as_tibble() |>
    dplyr::bind_cols(mapping) |>
    dplyr::select(.data$id, .data$long, .data$lat,
                  .data$time, .data$var, .data$Freq) |>
    dplyr::arrange(.data$id) |>
    tidyr::pivot_wider(names_from = .data$var, values_from = .data$Freq)

  key <- "id"
  all_vars <- find_invariant(data, !!key)
  out <- data |> tidyr::nest(ts = c(!!!all_vars$variant))

  new_spatial_cubble(
    out, key = key, index = "time", coords = c("long", "lat")
    )
}

#' @rdname as_cubble
#' @export
as_cubble.stars <- function(data, key, index, coords, ...){

  if (is.na(stars::st_raster_type(data))) { # vector data cube
	stopifnot(is.null(data$id),
	          # Check if any of the dimensions is an sfc
	          any(sapply(
	            stars::st_dimensions(data),
	            function(i) inherits(i$values, "sfc"))))
    data$id <- seq_len(dim(data)[1]) # recycles
    data <-  sf::st_as_sf(data, long = TRUE)
    key <-  enquo(key)
    index <-  enquo(index)
	as_cubble(data, key=!!key, index=!!index)
  } else { # raster data cube
    # making the assumption that long/lat are the first two dimensions
    longlat <- names(stars::st_dimensions(data))[1:2]
    index <-  enquo(index)
    if (quo_is_missing(index)) index <-  quo(!!sym(names(dim(data)[3])))
    as_tibble(data) |>
      group_by(!!!map(longlat, sym)) |>
      mutate(id = dplyr::cur_group_id()) |>
      ungroup() |>
      as_cubble(key = id, index = !!index, coords = longlat, dimension = stars::st_dimensions(data))
  }
}

#' @rdname as_cubble
#' @export
as_cubble.sftime <- function(data, key, index, coords, ...){

  key <- enquo(key)
  index <- enquo(index)
  coords <- enquo(coords)

  # here assume the geometry column in an sftime object is always sfc_POINT
  data <- data |>
    mutate(long = sf::st_coordinates(geometry)[,1], lat = sf::st_coordinates(geometry)[,2])

  if (quo_is_missing(coords)){
    coords <-  quo(c("long", "lat"))
  }
  coords <- as.list(quo_get_expr(coords))[-1]
  coords <- unlist(map(coords, as_string))

  all_vars <- data |> find_invariant(!!key)
  spatial <- data |> select(all_vars$invariant, -!!index) |> distinct()
  temporal <- as_tibble(data) |>
    select(!!key, all_vars$variant, !!index) |>
    nest(ts = all_vars$variant)
  out <- spatial |> left_join(temporal, by = as_name(key))

  new_spatial_cubble(
    out, key = as_name(key), index = as_name(index), coords = coords
    )

}

globalVariables(c(".", "id", "geometry"))



