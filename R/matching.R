#' Match stations in two cubbles by spatial distance/ temporal similarity
#'
#' The spatial matching is calculated using [sf::st_distance()] with different
#' distance (in meter or degree) available depending on the coordinate reference
#' system and parameter (`which` and `par`). The temporal matching is based on
#' a temporal matching function (`temporal_match_fn`) that can be customised.
#'
#' @param df1,df2 the two cubble objects to match
#' @param crs a crs object from [sf::st_crs()]
#' @param spatial_n_each integer, the number of matched "station" in \code{df2}
#' for each \code{df1} record
#' @param spatial_n_group integer, the number of matched group (pair) return
#' @param temporal_matching logical, whether to match temporally
#' @param temporal_by in the \code{by} syntax in \code{dplyr::*_join()},
#'  the variables to match temporally in \code{df1} and \code{df2}.
#' @param return_cubble logical (default to false), whether to return the
#' cubble object or a matching summary table
#' @param data the resulting cubble object from spatial matching (with
#'  \code{return_cubble = TRUE} in spatial matching)
#' @param data_id a character (or symbol), the variable differentiates
#' \code{df1} and \code{df2}
#' @param match_id a character (or symbol), the variable differentiate
#' each group of match
#' @param temporal_match_fn character, the function name on how two time
#' series should be matched
#' @param temporal_n_highest numeric, the number of highest peak used for
#' temporal matching in \code{match_peak}
#' @param temporal_window The temporal window allowed in \code{match_peak}
#' @param ... parameters passing to temporal match
#' @inheritParams sf::st_distance
#'
#' @export
#' @rdname matching
#' @examples
#' library(dplyr)
#' climate_aus <- mutate(climate_aus, type = "climate")
#' match_spatial(climate_aus, river)
#' # turn with different distance calculation:
#' match_spatial(climate_aus, river, which = "Hausdorff")
#' # tune the number of matches in each group
#' match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2)
#'
#' a1 <- match_spatial(climate_aus, river, return_cubble = TRUE) %>% bind_rows()
#' match_temporal(a1, data_id = type, match_id = group,
#' temporal_by = c("prcp" = "Water_course_level"))
#' match_temporal(a1, data_id = type, match_id = group,
#' temporal_by = c("prcp" = "Water_course_level"), return = TRUE)
match_sites <- function(df1, df2, crs = sf::st_crs("OGC:CRS84"),
                        which = NULL, par = 0,
                        spatial_n_each = 1,
                        spatial_n_group = 4,
                        data_id, match_id,
                        temporal_matching = TRUE,
                        temporal_by,
                        temporal_match_fn = match_peak,
                        temporal_n_highest = 20,
                        temporal_window = 5, ...) {

  out <- match_spatial(
    df1, df2, crs = crs,
    which = NULL, par = 0,
    spatial_n_each = spatial_n_each,
    spatial_n_group = spatial_n_group,
    return_cubble = TRUE
  )


  if (temporal_matching){
    out <- out %>%
      map(~.x %>% match_temporal(
        data_id = !!enquo(data_id), match_id = !!enquo(match_id),
        temporal_match_fn = match_peak,
        temporal_by = temporal_by,
        temporal_window = temporal_window,
        temporal_n_highest = temporal_n_highest,
        return_cubble = TRUE,
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
  if (is.null(which)) which <- ifelse(isTRUE(sf::st_is_longlat(df1)),
                                      "Great Circle", "Euclidean")

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
    arrange(.data$group)

  if (return_cubble){

    res1 <- df1 %>%
      inner_join(dist_df2 %>%
                   select(.data$from, .data$group) %>%
                   rename(!!key := .data$from),
                 by = key) %>%
      update_cubble()

    res2 <- df2 %>%
      inner_join(dist_df2 %>%
                   select(-.data$from) %>%
                   rename(!!key := .data$to),
                 by = key) %>%
      update_cubble() %>%
      arrange(.data$dist)

    dist_df2 <- bind_rows(res1, res2) %>%
      dplyr::group_split(.data$group) %>%
      map(update_cubble)
  }

  return(dist_df2)
}

#' @export
#' @rdname matching
match_temporal <- function(data,
                           data_id, match_id = NULL,
                           temporal_by,
                           return_cubble = FALSE,
                           temporal_match_fn = match_peak,
                           temporal_n_highest = 30,
                           temporal_window = 5,
                           ...) {
  match_id <- enquo(match_id)
  data_id <- enquo(data_id)
  var_names <- list(names(temporal_by), unname(temporal_by))
  key <- key_vars(data)
  index <- index_var(data)

  multiple_match <- any(data %>%
                          group_by(!!match_id) %>%
                          dplyr::group_size() != 2)
  if (multiple_match){
    data <- data %>%
      dplyr::group_split(!!match_id) %>%
      map(~.x %>% update_cubble() %>% group_by(type) %>%
            mutate(group2 = dplyr::row_number()) %>%
            dplyr::group_split(.data$group2)) %>%
      unlist(recursive = FALSE) %>%
      map(update_cubble)
  } else{
    data <- data %>%
      dplyr::group_split(!!match_id)
  }

  data_long <-  data %>%
    map(~.x %>%
          dplyr::group_split(!!data_id) %>%
          purrr::map2(var_names, ~.x %>%
                 face_temporal() %>%
                 dplyr::select(key, index, .y) %>%
                 dplyr::rename(matched = .y)))

  vecs <- data_long %>% map(~map(.x, ~.x$matched))

  res <- map_dbl(vecs, function(x)
    do.call(temporal_match_fn,
            args = list(list = x,
                        temporal_n_highest = temporal_n_highest,
                        temporal_window = temporal_window, ...)))

  out <- bind_rows(data) %>% as_tibble()
  if (multiple_match){
    out <- out %>% distinct(!!match_id, .data$group2)
  } else{
    out <- out %>% distinct(!!match_id)
  }
  res <- out %>% dplyr::bind_cols(match_res = res)

  if (return_cubble){
    res <- data_long %>%
      map(~map(.x, ~face_spatial(.x)) %>% bind_rows) %>%
      map2(res$match_res, ~.x %>% mutate(match_res = .y))
  }

  return(res)

}

match_peak <- function(list, temporal_n_highest, temporal_window, ...){

  sort_index <- function(x){
    sort(x, decreasing = TRUE, index.return = TRUE)
  }

  ts1_top <- sort_index(diff(list[[1]]))$ix[1:temporal_n_highest]
  ts2_top <- sort_index(diff(list[[2]]))$ix[1:temporal_n_highest]
  ts1_rg <- map(ts1_top, ~.x +0:temporal_window) %>% unlist() %>% unique()
  sum(ts2_top %in% ts1_rg)

}



