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
#' @param temporal_matching Wether to perform temporal matching
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
#'
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

    major_id <- key_data(major) %>% dplyr::pull(!!key_vars(major))
    minor_id <- key_data(minor) %>% dplyr::pull(!!key_vars(minor))

    major_matched <- out %>% filter(id %in% major_id)
    minor_matched <- out %>% filter(id %in% minor_id)

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
    mutate(group = dplyr::row_number())

  if (any(stringr::str_detect(coords_mj[[1]], "ref"))) {
    mn_long <- coords_mn[[1]]
    mn_lat <-  coords_mn[[2]]
    major <- major %>%
      rename( !!{mn_long} := coords_mj[[1]],
              !!{mn_lat} := coords_mj[[2]])
  }

  match_postprocessing(major, minor, out)
}

calc_dist <- function(data, coords1, coords2) {
  coords1 <- eval_tidy(enquo(coords1), data)
  coords2 <- eval_tidy(enquo(coords2), data)

  long1 <- coords1[[1]]
  lat1 <- coords1[[2]]
  long2 <- coords2[[1]]
  lat2 <- coords2[[2]]

  dt <- data %>%
    tibble::as_tibble() %>%
    dplyr::summarise(dplyr::across(.cols = c(!!!coords1, !!!coords2),
                            to_radian)) %>%
    dplyr::mutate(
      d_long = abs(!!long1 - !!long2),
      d_lat = abs(!!lat1 - !!lat2),
      a = (cos(!!lat2) * sin(.data$d_long)) ^ 2 +
        (
          cos(!!lat1) * sin(!!lat2) -
            sin(!!lat1) * cos(!!lat2) * cos(.data$d_long)
        ) ^ 2,
      denom = sin(!!lat1) * sin(!!lat2) +
        cos(!!lat1) * cos(!!lat2) * cos(.data$d_long),
      d = 6371 * atan(sqrt(.data$a) / .data$denom)
    )

  data %>%
    dplyr::bind_cols(dist = dt$d)

}

to_radian <- function(val) {
  val * pi / 180
}


#' @rdname matching
match_postprocessing <- function(major, minor, match_table) {
  is_cubble(major)
  is_cubble(minor)

  major_key <- key_vars(major)
  minor_key <- key_vars(minor)

  matched_major <- match_table %>% select(1, dist, .data$group)
  matched_minor <- match_table %>% select(2, dist,  .data$group)

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
    arrange(.data$group)


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
    stretch() %>%
    migrate(group) %>%
    dplyr::mutate(lag = dplyr::lag(matched_var),
                  diff = .data$lag-matched_var) %>%
    dplyr::top_n(n = temporal_n_highest, wt = diff) %>%
    dplyr::arrange(!!sym(index(data)), .by_group = TRUE)

  ngroup <- unique(dt$group)

  out <- map_dfr(1:length(ngroup),
                 ~ {
                   group_id = ngroup[.x]

                   match_temporal_single(dt,
                                         group_id = group_id,
                                         independent = temporal_independent,
                                         window = temporal_window)
                 })

  good <- out %>%
    dplyr::arrange(-.data$n_match) %>%
    dplyr::filter(.data$n_match >= temporal_min_match) %>%
    as_tibble() %>%
    select(!!sym(key), group, n_match)

  good_groups <- good %>%
    dplyr::pull(group) %>%
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
    stretch() %>%
    dplyr::select(key_vars(data), index(data), chosen_var) %>%
    dplyr::rename(matched_var = chosen_var) %>%
    tamp()
}

match_temporal_single <- function(data, group_id,
                                  independent, window = 5) {
  data_long <- data %>% dplyr::filter(.data$group == group_id)
  data_nested <-
    data %>% tamp() %>% dplyr::filter(.data$group == group_id)

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
