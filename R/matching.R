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
#' @param df1,df2 the two cubble objects to match
#' @param crs a crs object from \code{st_crs}
#' @param spatial_n_each integer, the number of matched "station" in \code{df2} for each \code{df1} record
#' @param spatial_n_group integer, the number of matched group (pair) return
#' @param temporal_matching logical, whether to match temporally
#' @param temporal_by in the syntax of c("xxx" = "xxx), the variables to match in \code{df1} and \code{df2}
#' @param return_cubble logical (default to false), whether to return the cubble object or a matching summary table
#' @param data the result from spatial matching
#' @param data_id variable name that separates \code{df1} and \code{df2}
#' @param match_id variable name that groups the same match
#' @param temporal_match_fn character, the function name on how two time series should be matched
#' @param temporal_n_highest numeric, the number of highest peak used for temporal matching in \code{match_peak}
#' @param temporal_window The temporal window allowed in \code{match_peak}
#' @param temporal_min_match The minimum number of peak matching for temporal matching in \code{match_peak}
#' @param ... parameters passing to temporal match
#' @inheritParams sf::st_distance
#'
#' @export
#' @rdname matching
#' @examples
#' a1 <- match_spatial(climate_aus, river)
#' # turn with different distance calculation:
#' a2 <- match_spatial(climate_aus, river, which = "Hausdorff")
#' # tune the number of matches in each group
#' a3 <- match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2)
match_sites <- function(df1, df2, crs = sf::st_crs("OGC:CRS84"),
                        which = NULL, par = 0,
                        spatial_n_each = 1,
                        spatial_n_group = 4,
                        temporal_matching = TRUE,
                        temporal_by,
                        temporal_match_fn = match_peak,
                        temporal_n_highest = 20,
                        temporal_window = 5,
                        temporal_min_match = 10, ...) {

  out <- match_spatial(
    df1, df2, crs = crs,
    which = NULL, par = 0,
    spatial_n_each = spatial_n_each,
    spatial_n_group = spatial_n_group
  )


  if (temporal_matching){
    out <- out %>%
      map(~.x %>% match_temporal(
        temporal_match_fn = match_peak,
        temporal_by = temporal_by,
        temporal_window = temporal_window,
        temporal_n_highest = temporal_n_highest,
        temporal_min_match = temporal_min_match,
        ...))
  }

  out

}


#' @export
#' @rdname matching
match_spatial <- function(df1, df2,
                          crs = sf::st_crs("OGC:CRS84"),
                          which = NULL,
                          par = 0,
                          spatial_n_each = 1,
                          spatial_n_group = 4,
                          return_cubble = FALSE) {
  stopifnot(is_cubble(df1), is_cubble(df2))

  key <- key_vars(df1)
  key2 <- key_vars(df2)
  if (key2 != key){df2 <- df2 %>% dplyr::rename(!!key := key2)}

  key_val <- as_tibble(df1) %>% dplyr::pull(key)
  key_val2 <- as_tibble(df2) %>% dplyr::pull(key)

  if (!is_sf(df1) || !is_sf(df2)) {
    cli::cli_inform("Use OGC:CRS84 by default for distance calculation...")
  }

  if (!is_sf(df1)) df1 <- df1 %>% make_spatial_sf(crs = crs)
  if (!is_sf(df2)) df2 <- df2 %>% make_spatial_sf(crs = crs)
  if (is.null(which)) which <- ifelse(isTRUE(sf::st_is_longlat(df1)), "Great Circle", "Euclidean")

  dist_df <- sf::st_distance(df1, df2, which = which, par = par) %>%
    as_tibble() %>%
    mutate(from = key_val) %>%
    dplyr::rename_with(~ c(key_val2, "from")) %>%
    tidyr::pivot_longer(cols = -.data$from, names_to = "to", values_to = "dist")

  gp_return <- dist_df %>%
    dplyr::slice_min(.data$dist, n = 1, by = .data$from) %>%
    dplyr::slice_min(.data$dist, n = spatial_n_group) %>%
    mutate(group = dplyr::row_number())

  dist_df2 <- dist_df %>%
    inner_join(gp_return %>% select(-.data$dist, -.data$to), by = "from") %>%
    dplyr::slice_min(.data$dist, n = spatial_n_each, by = .data$from) %>%
    arrange(group)

  if (return_cubble){

    res1 <- df1 %>%
      inner_join(dist_df2 %>% select(.data$from, .data$group) %>% distinct() %>% rename(!!key := .data$from), by = key) %>%
      update_cubble()

    res2 <- df2 %>%
      inner_join(dist_df2 %>% select(-.data$from) %>% rename(!!key := .data$to), by = key) %>%
      update_cubble() %>%
      arrange(.data$dist)

    dist_df2 <- bind_rows(res1, res2) %>% dplyr::group_split(.data$group) %>% map(update_cubble)
  }

  return(dist_df2)
}

#' @export
#' @rdname matching
match_temporal <- function(data,
                           data_id, match_id,
                           temporal_by,
                           return_cubble = FALSE,
                           temporal_match_fn = match_peak,
                           temporal_n_highest = 20,
                           temporal_window = 5,
                           temporal_min_match = 10,
                           ...) {

  match_id <- enquo(match_id)
  data_id <- enquo(data_id)
  var_names <- list(names(temporal_by), unname(temporal_by))

  data_long <- data %>%
    dplyr::arrange(!!match_id) %>%
    dplyr::group_split(!!match_id) %>%
    map(~.x %>%
          dplyr::group_split(!!data_id) %>%
          purrr::map2(var_names, ~.x %>%
                 face_temporal() %>%
                 dplyr::select(key_vars(data), index_var(data), .y) %>%
                 dplyr::rename(matched = .y)))

  vecs <- data_long %>% map(~map(.x, ~.x$matched))

  res <- map_dbl(vecs, function(x)
    do.call(temporal_match_fn,
            args = list(list = x,
                        temporal_n_highest = temporal_n_highest,
                        temporal_window = temporal_window, ...)))

  res <- tibble(
    !!match_id :=data %>% dplyr::pull(!!match_id) %>% unique() %>% sort(),
    n_matches = res) %>%
    dplyr::arrange(-n_matches)

  if (return_cubble){
    res <- data_long %>%
      map(~map(.x, ~face_spatial(.x))) %>%
      bind_rows() %>%
      left_join(res)
  }

  return(res)

}

match_peak <- function(list, temporal_n_highest, temporal_window){

  ts1_top <- sort(diff(list[[1]]), decreasing = TRUE, index.return = TRUE)$ix[1:temporal_n_highest]
  ts2_top <- sort(diff(list[[2]]), decreasing = TRUE, index.return = TRUE)$ix[1:temporal_n_highest]
  ts1_rg <- map(ts1_top, ~.x +0:temporal_window) %>% unlist() %>% unique()
  sum(ts2_top %in% ts1_rg)

}


