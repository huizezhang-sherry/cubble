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
                        spatial_single_match = TRUE,
                        spatial_n_keep = 1,
                        spatial_dist_max = 10,
                        temporal_matching = TRUE,
                        temporal_by,
                        temporal_n_highest = 20,
                        temporal_independent,
                        temporal_window = 5,
                        temporal_min_match = 10) {

  out <- match_spatial(
    major,
    minor,
    spatial_single_match = spatial_single_match,
    spatial_n_keep = spatial_n_keep,
    spatial_dist_max = spatial_dist_max
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
match_spatial <- function(major, minor,
                          major_ll = NULL, minor_ll = NULL,
                          major_key = NULL, minor_key = NULL,
                          spatial_single_match = TRUE,
                          spatial_n_keep = 1,
                          spatial_dist_max = 10){

  major_small <- construct_match_ll(major, major_ll)
  minor_small <- construct_match_ll(minor, minor_ll, major = FALSE)

  data_std <- major_small %>%
    dplyr::mutate(minor = list(minor_small)) %>%
    tidyr::unnest(minor)
  class(data_std) <- c("std_ll", class(data_std))
  res <- data_std %>% calc_dist()

  res <- res %>%
    dplyr::group_by(!!sym("key1")) %>%
    dplyr::slice_min(dist, n = spatial_n_keep) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dist <= spatial_dist_max)
  class(res) <- c("std_dist", class(res))

  if (spatial_single_match){
    res <- res %>%
      dplyr::group_by(!!sym("key2")) %>%
      dplyr::slice_min(dist, n = 1) %>%
      dplyr::ungroup()
  }

  res <- res %>%
    dplyr::arrange(dist) %>%
    dplyr::mutate(group = dplyr::row_number())

  out <- match_postprocessing(major, minor, res, major_ll, minor_ll)
  out
}

#' @export
construct_match_ll <- function(data, data_ll, major = TRUE){
  data_ll <- enquo(data_ll)

  if (is_cubble(data)){
    data <- data %>% dplyr::select(key_vars(data), coords(data))
  } else{
    if (quo_is_null(data_ll)){
      cli::cli_abort("Please provide the longitude and latitude column of the data")
    }
    data <- data %>% dplyr::select(1, !!!data_ll)
  }

  if (major){
    colnames(data) <- c("key1", "long1", "lat1")
  } else{
    colnames(data) <- c("key2", "long2", "lat2")
  }

  data
}

#' @export
calc_dist <- function(data){
  UseMethod("calc_dist")
}

#' @export
calc_dist.std_ll <- function(data){
  res <- data %>%
    dplyr::mutate(
      dplyr::across(c(long1, long2, lat1, lat2), to_radian),
      d_long = abs(long1 - long2),
      d_lat = abs(lat1 - lat2),
      a = (cos(lat2) * sin(d_long)) ^ 2 +
        (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(d_long)) ^ 2,
      denom = sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(d_long),
      dist = 6371 * atan(sqrt(a) / denom)) %>%
    select(key1, key2, dist)

}

to_radian <- function(val) {
  val * pi / 180
}


calc_dist.tbl_df <- function(data, ll){

  # make it into long1, long2, lat1, lat2

}

#' @export
is.std_ll <- function(data){
  inherits(data, "std_ll")
}

match_postprocessing <- function(major, minor,
                                 match_table,
                                 major_ll, minor_ll){
  matched_major <- restore(major, match_table, major_ll)
  matched_minor <- restore(minor, match_table, minor_ll, major = FALSE)

  if (colnames(matched_major)[1] != colnames(matched_minor)[1]){
    colnames(matched_minor)[1] <- colnames(matched_major)[1]
  }

  common_var <- intersect(colnames(matched_major), colnames(matched_minor))
  if (length(common_var) != ncol(matched_major) |
      length(common_var) != ncol(matched_minor)) {
    cli::cli_inform("Only bind the common variables from both datasets.")
  }

  out <- matched_major %>%
    dplyr::select(common_var) %>%
    rbind(matched_minor %>% dplyr::select(1, common_var)) %>%
    dplyr::arrange(dist)
  out
}

restore <- function(data, match_table, data_ll, major = TRUE){
  # data_ll <- enquo(data_ll)
  if (major){
    matched_key <- "key1"
    matched_info <- match_table %>% select("key1", "dist", "group")
  } else{
    matched_key <- "key2"
    matched_info <- match_table %>% select("key2", "dist", "group")
  }

  if (is_cubble(data)){
    dt_key <- key_vars(data)
    matched_origin <- data %>%
      rename_coords(c("long", "lat")) %>%
      dplyr::inner_join(matched_info, by = stats::setNames(matched_key, dt_key))
  } else{
    first <- colnames(data)[1]
    colnames(data)[colnames(data) %in% data_ll] <- c("long", "lat")
    matched_origin <- data %>%
      dplyr::inner_join(matched_info, by = stats::setNames(matched_key, first))
  }

  matched_origin

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
  is_cubble(major)
  is_cubble(minor)

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


  dt <- data %>%
    face_temporal() %>%
    unfold(.data$group) %>%
    dplyr::mutate(lag = dplyr::lag(.data$matched_var),
                  diff = .data$lag-.data$matched_var) %>%
    dplyr::top_n(n = temporal_n_highest, wt = diff) %>%
    dplyr::arrange(!!sym(index(data)), .by_group = TRUE)

  ngroup <- unique(dt$group)

  out <- map(1:length(ngroup),
                 ~ {
                   group_id <- ngroup[.x]

                   match_temporal_single(dt,
                                         group_id = group_id,
                                         independent = temporal_independent,
                                         window = temporal_window)
                 }) %>%  dplyr::bind_rows()

  good <- out %>%
    dplyr::arrange(-.data$n_match) %>%
    dplyr::filter(.data$n_match >= temporal_min_match) %>%
    as_tibble() %>%
    select(!!sym(key), .data$group, .data$n_match)

  good_groups <- good %>%
    dplyr::pull(.data$group) %>%
    unique()

  good_n_match <- good %>%
    dplyr::select(.data$n_match, .data$group) %>%
    unique()

  data %>%
    inner_join(good, by = c("group", key)) %>%
    dplyr::arrange(-.data$n_match)


}

fix_data <- function(data, chosen_var){
  test_cubble(data)
  data %>%
    face_temporal() %>%
    dplyr::select(key_vars(data), index(data), chosen_var) %>%
    dplyr::rename(matched_var = chosen_var) %>%
    face_spatial()
}

match_temporal_single <- function(data, group_id,
                                  independent, window = 5) {
  data_long <- data %>%  dplyr::filter(.data$group == group_id)
  data_nested <-
    data %>%  face_spatial() %>%  dplyr::filter(.data$group == group_id)

  if (independent == "major") {
    major_id <- data_nested$id[1]
    minor_id <- data_nested$id[2]
  } else{
    major_id <- data_nested$id[2]
    minor_id <- data_nested$id[1]
  }


  to_match <- data_long %>%
    dplyr::filter(.data$id == minor_id) %>%
    dplyr::pull(date)

  a <- data_long %>%
    dplyr::filter(.data$id == major_id) %>%
    dplyr::mutate(int = lubridate::interval(date, date + window)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(match = ifelse(any(to_match %within% int), TRUE, FALSE)) %>%
    dplyr::ungroup()


  data_nested %>%  dplyr::mutate(n_match = sum(a$match == TRUE))

}
