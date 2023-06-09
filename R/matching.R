#' Matching sites from two data sources
#'
#' The function includes both spatial and temporal matching. The spatial matching
#' is based on the distance and the distance is calculated using the Vincenty
#' formula assuming earth is sphere with a radius of 6371 km. The temporal matching
#' first filters out the `n` largest increases, determined by `temporal_n_highest`,
#' in both datasets,  constructs an interval of length `temporal_window` from
#' one dataset and count the number that large increase from the other dataset
#' falls into the interval constructed.
#'
#' @param major The major dataset to match, every key in the major dataset will have a match, unless filtered by \code{dist_max}
#' @param minor The dataset to match from
#' @param spatial_single_match Whether each observation in the minor dataset is only allowed to to be matched once, default to `TRUE`
#' @param spatial_n_keep The number of matching to keep
#' @param spatial_dist_max The maximum distance allowed between matched pair
#' @param temporal_matching Whether to perform temporal matching
#' @param temporal_by The variable used for temporal matching
#' @param temporal_n_highest The number of highest peak used for temporal matching
#' @param temporal_independent The dataset used to construct the temporal window,
#' need to be the name of either major or minor.
#' @param temporal_window The temporal window allowed to fall in
#' @param temporal_min_match The minimum number of peak matching for temporal matching
#' @param match_table The spatial matching table
#'
#'
#' @return A cubble with matched pairs
#' @export
#' @rdname matching
match_sites <- function(major,
                        minor,
                        spatial_n_keep = 1,
                        spatial_n_pair = 5,
                        temporal_matching = TRUE,
                        temporal_by,
                        temporal_n_highest = 20,
                        temporal_independent,
                        temporal_window = 5,
                        temporal_min_match = 10) {

  out <- match_spatial(
    major,
    minor,
    spatial_n_keep = spatial_n_keep,
    spatial_n_pair = spatial_n_pair
  )

  if (temporal_matching) {

    major_id <- key_data(major) %>%  dplyr::pull(!!key_vars(major))
    minor_id <- key_data(minor) %>%  dplyr::pull(!!key_vars(minor))

    major_matched <- out %>%  filter(.data$id %in% major_id)
    minor_matched <- out %>%  filter(.data$id %in% minor_id)

    out <- match_temporal(
      major_matched,
      minor_matched,
      temporal_by = temporal_by,
      temporal_n_highest = temporal_n_highest,
      temporal_independent = temporal_independent,
      temporal_window = temporal_window,
      temporal_min_match = temporal_min_match
    )

  }

  out

}


#' @export
#' @rdname matching
match_spatial <- function(major, minor,
                          major_crs = "EPSG:4087", minor_crs = "EPSG:4087",
                          spatial_n_keep = 1,
                          spatial_n_pair = 10) {
  stopifnot(is_cubble(major), is_cubble(minor))

  key <- key_vars(major)
  key2 <- key_vars(minor)
  # check key matches

  key_val <- as_tibble(major)[[tidyselect::eval_select(key, major)]]
  key_val2 <- as_tibble(minor)[[tidyselect::eval_select(key, minor)]]

  if (!is_sf(major)) major <- major %>% make_spatial_sf(crs = sf::st_crs(major_crs))
  if (!is_sf(minor)) minor <- minor %>% make_spatial_sf(crs = sf::st_crs(minor_crs))

  if (sf::st_is_longlat(major) || sf::st_is_longlat(minor)) {
    cli::cli_inform("Use EPSG:4087 (projected CRS) by default for distance calculation...")
  }

  # use the which argument form sf::st_ditance
  res <- sf::st_distance(major, minor) %>%
    as_tibble() %>%
    mutate(from = key_val) %>%
    rename_with(~ c(key_val2, "from")) %>%
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "dist") %>%
    dplyr::slice_min(dist, n = spatial_n_keep, by = from) %>%
    arrange(dist) %>%
    mutate(group = row_number()) %>%
    tidyr::pivot_longer(from:to, names_to = "order", values_to = "id")

  major %>%
    inner_join(res %>% filter(order == "from")) %>%
    update_cubble() %>%
    bind_rows(minor %>% inner_join(res %>% filter(order == "to")) %>% update_cubble()) %>%
    dplyr::select(-order) %>%
    filter(group <= spatial_n_pair)
}

#' @export
#' @rdname matching
#' @importFrom lubridate %within%
match_temporal <- function(major,
                           minor,
                           temporal_by,
                           temporal_n_highest = 20,
                           temporal_independent,
                           temporal_window = 5,
                           temporal_min_match = 10) {
  stopifnot(is_cubble(major), is_cubble(minor))

  major_var <- names(temporal_by)
  minor_var <- unname(temporal_by)

  data <- dplyr::bind_rows(fix_data(major, major_var), fix_data(minor, minor_var))
  key <- key_vars(data)

  if (identical(temporal_independent, major_var)) {
    temporal_independent <-  "major"
  } else if (identical(temporal_independent, minor_var)) {
    temporal_independent <- "minor"
  } else{
    cli::cli_abort("The independent set needs to be either the major or minor variable.")
  }

  res <- data %>%
    group_split(group) %>%
    map_dbl(function(x){
      dt <- x %>% face_temporal() %>%
              unfold(type) %>%
              group_split(type) %>%
              map(~.x$matched_var)

      do.call(match_peak, args = list(list = dt,
        temporal_n_highest = temporal_n_highest,
        temporal_window = temporal_window))
    })

  res2 <- tibble(group = sort(unique(data$group)), n_matches = res)

  data %>% left_join(res2) %>% update_cubble()

}

match_peak <- function(list, temporal_n_highest, temporal_window){

  ts1_top <- sort(diff(list[[1]]), decreasing = TRUE, index.return = TRUE)$ix[1:temporal_n_highest]
  ts2_top <- sort(diff(list[[2]]), decreasing = TRUE, index.return = TRUE)$ix[1:temporal_n_highest]
  ts2_rg <- map(ts2_top, ~.x +1:temporal_window) %>% unlist() %>% unique()
  sum(ts1_top %in% ts2_rg)

}



fix_data <- function(data, chosen_var){
  stopifnot(is_cubble(data))
  data %>%
    face_temporal() %>%
    dplyr::select(key_vars(data), index(data), chosen_var) %>%
    dplyr::rename(matched_var = chosen_var) %>%
    face_spatial()
}
