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
#' @param match_temporal Wether to perform temporal matching
#' @param spatial_single_match Whether each observation in the minor dataset is only allowed to to be matched once, default to `TRUE`
#' @param spatial_n_keep The number of matching to keep
#' @param spatial_dist_max The maximum distance allowed between matched pair
#' @param temporal_var_to_match The variable used for temporal matching
#' @param temporal_n_highest The number of highest peak used for temporal matching
#' @param temporal_independent The dataset used to construct the temporal window,
#' need to be the name of either major or minor.
#' @param temporal_window The temporal window allowed to fall in
#' @param temporal_min_match The minimum number of peak matching for temporal matching
#' @param match_table The output from `match_spatial()`
#'
#'
#' @return A cubble with matched pairs
#' @export
#' @rdname matching
#'
match_sites <- function(major,
                        minor,
                        match_temporal = TRUE,
                        spatial_single_match = TRUE,
                        spatial_n_keep = 1,
                        spatial_dist_max = 10,
                        temporal_var_to_match,
                        temporal_n_highest = 20,
                        temporal_independent,
                        temporal_window = 5,
                        temporal_min_match = 10) {

  match_table <- match_spatial(
    major,
    minor,
    spatial_single_match = spatial_single_match,
    spatial_n_keep = spatial_n_keep,
    spatial_dist_max = spatial_dist_max
  )

  out <- match_postprocessing(major, minor, match_table)

  if (match_temporal) {
    independent <- temporal_independent


    half <- nrow(out) / 2
    major_matched <- out[1:half, ]
    minor_matched <- out[(half + 1):(2 * half), ]

    if (identical(independent, major)) {
      independent <- major_matched
    } else if (identical(independent, minor)) {
      independent <- minor_matched
    } else{
      cli::cli_abort("The independent set needs to be either the major or minor set.")
    }


    out <- match_temporal(
      major_matched,
      minor_matched,
      temporal_var_to_match = !!enquo(temporal_var_to_match),
      temporal_n_highest = temporal_n_highest,
      temporal_independent = independent,
      temporal_window = temporal_window,
      temporal_min_match = temporal_min_match
    )

  }

  out

}


#' @export
#' @rdname matching
match_spatial <- function(major,
                          minor,
                          spatial_single_match = TRUE,
                          spatial_n_keep = 1,
                          spatial_dist_max = 10) {
  test_cubble(major)
  test_cubble(minor)

  coords_mj <- syms(coords(major))
  coords_mn <- syms(coords(minor))

  if (identical(coords_mj, coords_mn)) {
    major <- major %>%
      rename(long_ref = coords_mj[[1]],
             lat_ref = coords_mj[[2]])
    coords_mj <- syms(c("long_ref", "lat_ref"))
  }

  key_mj <- key_vars(major)
  key_mn <- key_vars(minor)

  if (identical(key_mj, key_mn)) {
    major <- major %>% rename_key(key_mj = key_mj)
    key_mj <- "key_mj"

    minor <- minor %>% rename_key(key_mn = key_mn)
    key_mn <- "key_mn"
  }

  out <- major %>%
    as_tibble() %>%
    dplyr::mutate(minor = list(
      tibble::as_tibble(minor) %>%
        dplyr::select(!!key_mn, !!!coords_mn)
    )) %>%
    tidyr::unnest(minor) %>%
    calc_dist(coords_mj, coords_mn) %>%
    dplyr::group_by(!!sym(key_mj)) %>%
    dplyr::slice_min(dist, n = spatial_n_keep) %>%
    dplyr::select(!!sym(key_mj), !!sym(key_mn), .data$dist) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dist <= spatial_dist_max)

  if (spatial_single_match) {
    temp <- out %>%
      dplyr::mutate(dup = duplicated(!!sym(key_mn)))

    prob_sites <- temp %>%
      dplyr::filter(dup) %>%
      dplyr::pull(!!sym(key_mn))

    if (length(prob_sites) != 0){
      good <- temp %>% dplyr::filter(!dup) %>% dplyr::pull(!!sym(key_mn))
      good_sites <- good[!good %in% prob_sites]

      cli::cli_inform(
        "Detect site {.val {prob_sites}} having multiple matches.
        Only keep the closest match."
      )

      dup_fixed <- out %>%
        dplyr::filter(!!sym(key_mn) %in% prob_sites) %>%
        dplyr::group_by(!!sym(key_mn)) %>%
        dplyr::filter(dist == min(dist))

      out <- temp %>%
        dplyr::filter(!!sym(key_mn) %in% good_sites) %>%
        dplyr::bind_rows(dup_fixed) %>%
        dplyr::select(-dup)
    }

  }

  out <- out %>%
    arrange(dist) %>%
    mutate(.group = dplyr::row_number())

  out
}

calc_dist <- function(data, coords1, coords2) {
  dt <- data %>%
    summarise(dplyr::across(.cols = c(!!!coords1, !!!coords2),
                            to_radian)) %>%
    mutate(
      d_long = abs(.data$long - .data$long_ref),
      d_lat = abs(.data$lat - .data$lat_ref),
      a = (cos(.data$lat_ref) * sin(.data$d_long)) ^ 2 +
        (
          cos(.data$lat) * sin(.data$lat_ref) -
            sin(.data$lat) * cos(.data$lat_ref) * cos(.data$d_long)
        ) ^ 2,
      denom = sin(.data$lat) * sin(.data$lat_ref) +
        cos(.data$lat) * cos(.data$lat_ref) * cos(.data$d_long),
      d = 6371 * atan(sqrt(.data$a) / .data$denom)
    )

  data %>%
    mutate(dist = dt$d)

}

to_radian <- function(val) {
  val * pi / 180
}


#' @export
#' @rdname matching
match_postprocessing <- function(major, minor, match_table) {
  is_cubble(major)
  is_cubble(minor)

  major_key <- key_vars(major)
  minor_key <- key_vars(minor)

  matched_major <- match_table %>% select(1, dist, .data$.group)
  matched_minor <- match_table %>% select(2, dist,  .data$.group)

  major_key2 <- colnames(matched_major)[1]
  minor_key2 <- colnames(matched_minor)[1]

  if (!identical(major_key, minor_key)) {
    cli::cli_inform(
      "The key variable is named differently in the two datasets.
                    Coerce the key to {.field id} to bind them together."
    )
    major <- major %>% rename_key("id" = major_key)
    minor <- minor %>% rename_key("id" = minor_key)
  }

  joined_major <- major %>%
    dplyr::inner_join(matched_major,
                      by = stats::setNames(major_key2, key_vars(major)))

  joined_minor <- minor %>%
    dplyr::inner_join(matched_minor,
                      by = stats::setNames(minor_key2, key_vars(minor))) %>%
    arrange(.data$.group)


  common_var <-
    intersect(colnames(joined_major), colnames(joined_minor))

  if (length(common_var) != ncol(joined_major) |
      length(common_var) != ncol(joined_minor)) {
    cli::cli_inform("Only bind the common variables from both datasets.")
  }

  out <- joined_major %>%
    dplyr::select(common_var) %>%
    dplyr::bind_rows(joined_minor %>%
                       dplyr::select(common_var)) %>%
    dplyr::arrange(dist)

  out
}

#' @export
#' @rdname matching
#' @importFrom lubridate %within%
match_temporal <- function(major,
                           minor,
                           temporal_var_to_match,
                           temporal_n_highest = 20,
                           temporal_independent,
                           temporal_window = 5,
                           temporal_min_match = 10) {
  is_cubble(major)
  is_cubble(minor)
  var <- enquo(temporal_var_to_match)
  independent <- temporal_independent

  data <- dplyr::bind_rows(major, minor)
  key <- key_vars(data)

  if (identical(independent, major)) {
    independent <-  "major"
  } else if (identical(independent, minor)) {
    independent <- "minor"
  } else{
    cli::cli_abort("The independent set needs to be either the major or minor set.")
  }


  dt <- data %>%
    stretch() %>%
    migrate(.data$.group) %>%
    dplyr::mutate(lag = dplyr::lag(!!var),
                  diff = .data$lag-!!var) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::top_n(n = temporal_n_highest, wt = diff) %>%
    dplyr::arrange(!!sym(index(data)), .by_group = TRUE)

  ngroup <- unique(dt$.group)

  out <- map_dfr(1:length(ngroup),
                 ~ {
                   group_id = ngroup[.x]

                   match_temporal_single(dt,
                                         group_id = group_id,
                                         independent = independent,
                                         window = temporal_window)
                 })

  good <- out %>%
    dplyr::arrange(-.data$n_match) %>%
    dplyr::filter(.data$n_match >= temporal_min_match) %>%
    as_tibble() %>%
    select(!!sym(key), .group, n_match)

  good_groups <- good %>%
    dplyr::pull(.group) %>%
    unique()

  good_n_match <- good %>%
    dplyr::select(n_match, .group) %>%
    unique()

  data %>% inner_join(good, by = c(".group", key))


}

match_temporal_single <- function(data, group_id,
                                  independent, window = 5) {
  data_long <- data %>% dplyr::filter(.data$.group == group_id)
  data_nested <-
    data %>% tamp() %>% dplyr::filter(.data$.group == group_id)

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


  data_nested %>% dplyr::mutate(n_match = sum(a$match == TRUE))

}
