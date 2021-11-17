#' Functions to extract NetCDF dimension and variables
#' @param data a NetCDF file read in from \code{ncdf4::nc_open()}
#' @param vars variables to read, see the variables in your data with \code{names(data$var)}
#' @export
#' @importFrom ncdf4 ncvar_get
#' @rdname netcdf
extract_var <- function(data, vars){
  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")

  out <- map(vars, ~ncdf4::ncvar_get(data,.x))

  list(var = out, name = vars)
}

#'@export
#' @rdname netcdf
extract_longlat <- function(data){
  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")

  dims <- names(data$dim)

  if (all(!dims %in% c("longitude", "latitude", "time"))){
    abort("Dimension supported by cubble from NetCDF file: long, lat, and time.")
  }


  if ("longitude" %in% dims) long <- ncdf4::ncvar_get(data, "longitude")
  if ("latitude" %in% dims) lat <- ncdf4::ncvar_get(data, "latitude")

  list(long = long, lat = lat)

}

#' @importFrom stringr word
#' @importFrom lubridate %m+% hours
#' @export
#' @rdname netcdf
extract_time <- function(data){
  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")
  dims <- names(data$dim)
  if (!"time" %in% dims) inform("No time dimension detected!")
  if ("time" %in% dims) time <- ncdf4::ncvar_get(data, "time")

  tunits <- ncdf4::ncatt_get(data, "time", "units")

  # process period
  tperiod <- stringr::word(tunits$value)
  if (tperiod %in% c("day", "hour", "minute", "second" ,"month", "year")) {
    tperiod <- paste0(tperiod, "s")
  }

  origin <- parse_time(tunits$value)
  out <- origin %m+% do.call(tperiod, list(x = time))
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
    tzone_std <- tzone_list %>%
      dplyr::filter(as.numeric(.data$utc_offset_h) == tzone_string) %>%
      dplyr::pull(.data$tz_name)

    if (length(tzone_std) == 0){
      cli::cli_abort("Timezone can't be parsed.")
    }
  }

  lubridate::as_datetime(dttm, tz = tzone_std)

}