#' @importFrom sf st_coordinates
convert_sfc_to_ll <- function(data, coords){
  dt <- data %>%
    dplyr::mutate(.val = list(tibble::as_tibble(sf::st_coordinates(geom)))) %>%
    dplyr::select(-geom)
  coords <- c("X", "Y")

  list(data = dt, coords = coords)
}
