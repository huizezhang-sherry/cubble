#' Functions to extract NetCDF dimension and variables
#' @param data a NetCDF file read in from \code{ncdf4::nc_open()}
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get
#' @rdname netcdf
extract_var <- function(data){

  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")

  vars <- names(data$var)
  if (length(vars) > 1) infom("Only read in the first variable.")

  list(var = ncdf4::ncvar_get(data, vars), name = vars)
}

#'@export
#' @rdname netcdf
extract_longlat <- function(data){
  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")

  dims <- names(data$dim)

  if (all(!dims %in% c("long", "lat", "time"))){
    abort("Dimension supported by cubble from NetCDF file: long, lat, and time.")
  }


  if ("lon" %in% dims) long <- ncdf4::ncvar_get(data, "lon")
  if ("lat" %in% dims) lat <- ncdf4::ncvar_get(data, "lat")

  list(long = long, lat = lat)

}

#' @importFrom stringr word
#' @importFrom lubridate %m+%
#' @export
#' @rdname netcdf
extract_time <- function(data){
  if (class(data) != "ncdf4") abort("Data supplied is not of class ncdf4")
  dims <- names(data$dim)
  if (!"time" %in% dims) inform("No time dimension detected!")
  if ("time" %in% dims) time <- ncdf4::ncvar_get(data, "time")

  tunits <- ncdf4::ncatt_get(data,"time","units")
  tperiod <- stringr::word(tunits$value)

  if (tperiod %in% c("day", "hour", "minute", "second" ,"month", "year")) {
    tperiod <- paste0(tperiod, "s")
  }

  time_origin <- as.Date(gsub("[^0-9|-]", "\\1", tunits$value))
  out <- time_origin %m+%  do.call(tperiod, list(x = time))
  out
}
