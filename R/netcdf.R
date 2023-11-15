#' Functions to extract NetCDF dimension and variables
#' @param data a NetCDF file read in from \code{ncdf4::nc_open()}
#' @param vars variables to read, see the variables in your data with
#' \code{names(data$var)}
#' @importFrom ncdf4 ncvar_get
#' @rdname netcdf
#' @return extracted netcdf4 components
#' @keywords internal
extract_var <- function(data, vars = NULL){
  if (!inherits(data, "ncdf4")) abort("Data supplied is not of class ncdf4")
  if (is_missing(vars)){vars <- names(data$var)}

  out <- map(vars, ~ncdf4::ncvar_get(data,.x))

  list(var = out, name = vars)
}

#' @rdname netcdf
extract_longlat <- function(data){
  if (!inherits(data, "ncdf4")) abort("Data supplied is not of class ncdf4")

  dims <- names(data$dim)

  long_name <- c("lon", "long", "longitude")
  if (any(dims %in% long_name)) long_idx <- which(dims %in% long_name)

  lat_name <- c("lat", "latitude")
  if (any(dims %in% lat_name)) lat_idx <- which(dims %in% lat_name)

  long <- ncdf4::ncvar_get(data, dims[long_idx]) |> as.vector()
  lat <- ncdf4::ncvar_get(data, dims[lat_idx]) |> as.vector()

  list(long = long, lat = lat)

}

#' @importFrom lubridate %m+% hours days minutes seconds years
#' @rdname netcdf
extract_time <- function(data){
  if (!inherits(data, "ncdf4")) abort("Data supplied is not of class ncdf4")
  dims <- names(data$dim)
  if (!"time" %in% dims) inform("No time dimension detected!")
  if ("time" %in% dims) time <- ncdf4::ncvar_get(data, "time")

  tunits <- ncdf4::ncatt_get(data, "time", "units")

  # process period
  tperiod <- sub(" .*", "\\1", tunits$value)
  if (tperiod %in% c("day", "hour", "minute", "second" ,"month", "year")) {
    tperiod <- paste0(tperiod, "s")
  }

  origin <- parse_time(tunits$value)
  out <- origin %m+% do.call(tperiod, list(x = floor(time)))
  out

}

parse_time <- function(tstring){

  # process date time and timezone
  tstring <- stringr::str_extract(tstring, "[?<=\\d].*$")
  seg_n <- stringr::str_split(tstring, " ", simplify = TRUE)

  dttm <- tstring
  tzone_std <- "UTC"

  if (length(seg_n) == 3) {
    dttm <- stringr::str_remove(tstring, seg_n[3])
    tzone_string <- as.numeric(stringr::str_replace(seg_n[3], ":", "."))
    tzone_std <- tzone_list |> 
      dplyr::filter(as.numeric(.data$utc_offset_h) == tzone_string) |> 
      dplyr::pull(.data$tz_name)

    if (length(tzone_std) == 0){
      cli::cli_abort("Timezone can't be parsed.")
    }
  }

  lubridate::as_datetime(dttm, tz = tzone_std)

}
