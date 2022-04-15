#' Find (multi)polygons with small area
#'
#' @param data An sf object
#' @param geom The geometry column
#' @param area The area column if any
#' @param point_threshold The number of point threshold used to define small crumb
#' @param area_threshold The area size threshold used to define small crumb
#'
#' @return An sf object
#' @importFrom sf st_area st_coordinates st_is_empty
#' @export
#'
simplify_sf <- function(data, geom, area, point_threshold = 0.9, area_threshold = 0.9){

  geom <- enquo(geom)
  area <- enquo(area)

  dt <- get_areapoints(data, !!geom, !!area)
  out <- label_sf(dt, point_threshold, area_threshold)
  out

}

get_areapoints <- function(data, geom, area){

  geom <- enquo(geom)
  area <- enquo(area)

  if (!inherits(data, "sf")){
    cli::cli_abort("Require an {.code sf} object for {.fn simplify_sf}.")
  }

  if (quo_is_missing(geom)) geom <- sym(attr(data, "sf_column"))
  if (!"sfc" %in% class(eval(geom, data))) {
    cli::cli_abort("Require {.code geom} to be an {.code sfc} column.")
  }

  if (quo_is_missing(area)) {
    data <- data |> dplyr::mutate(area = as.numeric(sf::st_area({{geom}})))
    area <- sym("area")
  }

  data <- data |> filter(!sf::st_is_empty({{geom}}))

  points <- data |>
    sf::st_coordinates({{geom}}) |>
    tibble::as_tibble() |>
    dplyr::count(.data$L3) |>
    dplyr::rename(n_points = .data$n) |>
    dplyr::pull(.data$n_points)

  out <- data |> dplyr::bind_cols(n_points = .data$points)

  attr(out, "area") <- area
  out

}

label_sf <- function(data, point_threshold = 0.9, area_threshold = 0.9){

  if (!"n_points" %in% colnames(data)){
    cli::cli_abort("Data should be an output of {.fn simplify_sf} with an {.code n_points} column")
  }

  area <- attr(data, "area")
  point_qunt <- stats::quantile(data[["n_points"]], prob = point_threshold)
  area_qunt <- stats::quantile(data[[area]], prob = area_threshold)

  data |>
    mutate(crumb = ifelse(!!area < area_qunt & .data$n_points < point_qunt, TRUE, FALSE))

}
