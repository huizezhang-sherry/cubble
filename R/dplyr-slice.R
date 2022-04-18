# helper
slice_factory <- function(f, ...){
  function(data, ...){
    key <- key_vars(data)
    spatial <- spatial(data)
    index <- index(data)
    coords <- coords(data)
    data <- tibble::as_tibble(data)
    out <- NextMethod()

    new_cubble(out,
               key = key , index = index , coords = coords,
               spatial = spatial, form = determine_form(data))
  }
}

#' Slicing a cubble
#'
#' Slicing can be useful when the number of site is too large to be all visualised in a
#' single plot. The slicing family in cubble wraps around the [dplyr::slice()] family to
#' allow slicing from top and bottom, based on a variable, or in random.
#'
#' @param data a cubble object to slice
#' @param ... other arguments passed to the [dplyr::slice()]
#' @examples
#' # slice the first 50 stations from the top/ bottom
#' library(dplyr)
#' climate_aus |> slice_head(n = 50)
#' climate_aus |> slice_tail(n = 50)
#'
#' # slice based on the max/ min of a variable
#'
#' climate_aus |> slice_max(elev, n = 10)
#' climate_aus |> slice_min(lat, n = 10)
#'
#' # random sample
#' climate_aus |> slice_sample(n = 10)
#' @importFrom dplyr slice_head slice_tail slice_min slice_max slice_sample
#' @rdname slice
#' @return a cubble object
#' @export
slice_head.cubble_df <- slice_factory("slice_head", group = FALSE)

#' @rdname slice
#' @export
slice_tail.cubble_df <- slice_factory("slice_tail")

#' @rdname slice
#' @export
slice_min.cubble_df <- slice_factory("slice_min")

#' @rdname slice
#' @export
slice_max.cubble_df <- slice_factory("slice_max")

#' @rdname slice
#' @export
slice_sample.cubble_df <- slice_factory("slice_sample")

#' Location-based slicing
#' @param data the data to slice
#' @param coord the coordinate of used to slice nearby locations
#' @param buffer the buffer added to the coordinate for slicing
#' @param n the number of nearby points to slice, based on distance
#' @examples

#' # slice locations within 1 degree of (130E, 25S)
#' slice_nearby(climate_aus, coord = c(130, -25), buffer = 3)
#'
#' # slice the 5 closest location to (130E, 25S)
#' slice_nearby(climate_aus, coord = c(130, -25), n = 5)
#' @export
#' @return a cubble object
slice_nearby <- function(data, coord, buffer, n){
  UseMethod("slice_nearby")
}

#' @export
slice_nearby.cubble_df <- function(data, coord, buffer = NA, n = NA){

  test_cubble(data)
  if (form(data) == "long") data <- data |> face_spatial()

  if (length(coord) != 2){
    cli::cli_abort("{.val coord needs to be in the format of {.code c(LONG, LAT)}}")
  }

  if (!is.na(buffer)){
    lat_min <- coord[2] - buffer
    lat_max <- coord[2] + buffer
    long_min <-  coord[1] - buffer
    long_max <-  coord[1] + buffer

    out <- data |> filter(
      dplyr::between(.data$lat, lat_min, lat_max),
      dplyr::between(.data$long, long_min, long_max)
    )
  }

  if (!is.na(n)){
    out <- data |>
      mutate(long_ref = coord[1], lat_ref = coord[2]) |>
      calc_dist(coords1 = as.list(coords(data)) |> syms(),
                coords2 = list(long_ref = sym("long_ref"), lat_ref = sym("lat_ref"))) |>
      slice_min(.data$dist, n = n)
  }

  out


}
