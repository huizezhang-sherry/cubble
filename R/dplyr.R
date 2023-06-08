#' Access to dplyr verbs
#'
#' Verbs supported for both nested and long cubble include:
#' [dplyr::mutate()], [dplyr::filter()],  [dplyr::arrange()], [dplyr::select()],
#' [dplyr::group_by()], [dplyr::ungroup()], [dplyr::summarise()],. [dplyr::rename()],
#' [dplyr::bind_cols()], [dplyr::rowwise()],
#' \code{dplyr::slice_*()}, \code{dplyr::*_join())}, [dplyr::relocate()], [dplyr::pull()]
#'
#' You may find not all the verbs have a \code{verb.spatial_cubble_df} or
#' \code{verb.temporal_cubble_df} implemented. These verbs under the hood call
#' the dplyr extending trios: \code{dplyr_row_slice}, \code{dplyr_col_modify}, and
#' \code{dplyr_reconstruct}. See https://dplyr.tidyverse.org/reference/dplyr_extending.html
#' @param data,.data a cubble object of class \code{spatial_cubble_df} or \code{temporal_cubble_df}
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::dplyr_row_slice
#' @inheritParams dplyr::dplyr_col_modify
#' @inheritParams dplyr::dplyr_reconstruct
#' @inheritParams dplyr::summarise
#' @inheritParams dplyr::bind_cols
#' @inheritParams dplyr::bind_rows
#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @importFrom utils head
#' @rdname dplyr
#' @export
#' @examples
#' library(dplyr)
#' cb_nested <- climate_mel
#' cb_long <- face_temporal(climate_mel)
#'
#' # filter - currently filter.spatial_cubble_df, dply_row_slice
#' cb_nested %>% filter(elev > 40)
#' cb_long %>% filter(prcp > 0)
#'
#' # mutate - curerntly mutate.spatial_cubble_df, dply_col_modify
#' cb_nested %>% mutate(elev2 = elev + 10)
#' cb_long %>% mutate(prcp2 = prcp + 10)
#'
#' # arrange - currently arrange.spatial_cubble_df, arrange.temporal_cubble_df
#' cb_nested %>% arrange(wmo_id)
#' cb_long %>% arrange(prcp)
#'
#' # summarise - summarise.spatial_cubble_df,  summarise.temporal_cubble_df
#' cb_long %>%
#'   group_by(first_5 = ifelse(lubridate::day(date) <=5, 1, 2 )) %>%
#'   summarise(tmax = mean(tmax))
#' cb_long %>%
#'   mutate(first_5 = ifelse(lubridate::day(date) <=5, 1, 2)) %>%
#'   summarise(t = mean(tmax), .by = first_5)
#'
#' # select -  select.spatial_cubble_df,  select.temporal_cubble_df
#' cb_nested %>% select(name)
#' cb_nested %>% select(-id, -name)
#' cb_long %>% select(prcp)
#' cb_long %>% select(-prcp, -date)
#'
#' # rename - rename.spatial_cubble_df, rename.temporal_cubble_df
#' cb_nested %>% rename(elev2 = elev)
#' cb_long %>% rename(prcp2 = prcp)
#' # rename on key attributes
#' cb_nested %>% rename(id2 = id)
#' cb_long %>% rename(date2 = date)
#'
#' # join - mutate_join - dplyr_reconstruct()
#' # join - filter_join - dplyr_row_slice()
#' df1 <- cb_nested %>% as_tibble() %>% select(id, name) %>% head(2)
#' nested <- cb_nested %>% select(-name)
#' nested %>% left_join(df1, by = "id")
#' nested %>% right_join(df1, by = "id")
#' nested %>% inner_join(df1, by = "id")
#' nested %>% full_join(df1, by = "id")
#' nested %>% anti_join(df1, by = "id")
#'
#' # bind_rows - dplyr_reconstruct, bind_rows.temporal_cubble_df
#' df1 <- cb_nested %>% head(1)
#' df2 <- cb_nested %>% tail(2)
#' bind_rows(df1, df2)
#' df1 <- cb_long %>% head(10)
#' df2 <- cb_long %>% tail(20)
#' bind_rows(df1, df2)
#'
#' # relocate - dplyr_col_select, dplyr_col_select
#' cb_nested %>% relocate(ts, .before = name)
#' cb_nested %>% face_temporal() %>% relocate(tmin)
#'
#' # slice - all the slice_* uses dplyr::slice(), which uses dplyr_row_slice()
#' cb_nested %>% slice_head(n = 2)
#' cb_nested %>% slice_tail(n = 2)
#' cb_nested %>% slice_max(elev)
#' cb_nested %>% slice_min(elev)
#' cb_nested %>% slice_sample(n = 2)
#'
#' # rowwise - rowwise.spatial_cubble_df, rowwise.temporal_cuble_df
#' cb_nested %>% rowwise()
#' cb_long %>% rowwise()
#'
#' # group_by & ungroup - group_by/ungroup.spatial_cubble_df, xxx.temporal_cubble_df
#' (res <- cb_nested %>% mutate(group1 = c(1, 1, 2)) %>% group_by(group1))
#' res %>% ungroup()
#' (res2 <- res %>% face_temporal() %>% unfold(group1) %>% group_by(group1))
#' res2 %>% ungroup()
#' res2 %>% mutate(first_5 = ifelse(lubridate::day(date) <= 5, 1, 6)) %>%
#'   group_by(first_5) %>%
#'   ungroup(group1)
arrange.temporal_cubble_df <- function(.data, ...){
  out <- NextMethod()
  dplyr_reconstruct(out, .data)
}

#' @rdname dplyr
#' @export
select.spatial_cubble_df <- function(.data, ...){

  loc <- tidyselect::eval_select(expr(c(...)), data = .data,
                                 error_call = error_call)
  cb_attrs <- c(key_vars(.data), coords(.data), "ts")
  data <- .data
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  attrs_no_in <- !cb_attrs %in% names(loc)
  if (any(attrs_no_in)){
    attr_missing <- cb_attrs[which(attrs_no_in)]
    loc <- c(tidyselect::eval_select(attr_missing, data = .data), loc)
    cli::cli_alert_info("Missing attribute {.code {attr_missing}}, add it back.")
  }

  out <- select(.data, loc)
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
select.temporal_cubble_df <- function(.data, ...){
  loc <- tidyselect::eval_select(expr(c(...)), data = .data,
                                 error_call = error_call)
  data <- .data
  class(.data) <- setdiff(class(.data), cb_temporal_cls)
  # the key argument is slightly different from tsibble's key
  if (is_tsibble(.data)) .data <- as_tsibble(.data, key = key_vars(data),
                                             index = index_var(data))
  cb_attrs <- c(key_vars(data), index_var(data))
  attrs_no_in <- !cb_attrs %in% names(loc)
  if (any(attrs_no_in)){
    attr_missing <- cb_attrs[which(attrs_no_in)]
    loc <- c(tidyselect::eval_select(attr_missing, data = .data), loc)
    cli::cli_alert_info("Missing attribute {.code {attr_missing}}, add it back.")
  }
  out <- select(.data, loc)
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
group_by.spatial_cubble_df <- function(.data, ..., .add, .drop){
  vars <- enquos(..., .named = TRUE)
  grp_vars <- names(vars)

  out <- NextMethod()
  groups <- out %@% groups

  new_spatial_cubble(
    out, key = key_vars(.data), index = index_var(.data), coords = coords(.data),
    groups = groups
  )
}

#' @rdname dplyr
#' @export
group_by.temporal_cubble_df <- function(.data, ..., .add, .drop){
  vars <- enquos(..., .named = TRUE)
  grp_vars <- names(vars)

  out <- NextMethod()
  groups <- out %@% groups

  new_temporal_cubble(
    out, key = key_vars(.data), index = index_var(.data), coords = coords(.data),
    spatial = spatial(.data), groups = groups
  )

}

#' @rdname dplyr
#' @export
ungroup.spatial_cubble_df <- function(x, ...){

  key <- key_vars(x)
  index <- index_var(x)
  coords <- coords(x)
  out <- NextMethod()

  new_spatial_cubble(
    out, key = key, index = index, coords = coords
  )
}

#' @rdname dplyr
#' @export
ungroup.temporal_cubble_df <- function(x, ...){
  key <- key_vars(x)
  index <- index_var(x)
  coords <- coords(x)
  out <- NextMethod()

  new_temporal_cubble(
    out, key = key, index = index, coords = coords, spatial  = spatial(x)
  )
}


#' @rdname dplyr
#' @export
summarise.spatial_cubble_df <- function(.data, ..., .by = NULL, .groups = NULL){
  vars <- enquos(..., .named = TRUE)
  out <- NextMethod()

  new_spatial_cubble(
    out, key = key_vars(.data), index = index_var(.data), coords = coords(.data)
  )
}

#' @rdname dplyr
#' @export
summarise.temporal_cubble_df <- function(.data, ..., .by = key_vars(.data), .groups = NULL){
  vars <- enquos(..., .named = TRUE)
  index <- index_var(.data)
  key <- key_vars(.data)
  coords <- coords(.data)
  spatial <-  spatial(.data)
  .by <- eval(quo_get_expr(enquo(.by)), envir = .data)
  class(.data) <- setdiff(class(.data), cb_temporal_cls)
  if (inherits(.data, "grouped_df")){
    gv <- c(group_vars(.data), .by)
    .data <- .data %>% group_by(!!!syms(gv))
    out <- summarise(.data, ..., .groups = .groups)
  } else{
    out <- summarise(.data, ..., .by = .by, .groups = .groups)
  }

  if (!index %in% colnames(out) && "groups" %in% names(attributes(out))){
    potential_index <- .data %@% groups %>% colnames() %>% utils::head(-1)
    index <- setdiff(potential_index, key)
  }

  if (!index %in% colnames(out) || !key %in% colnames(out)){
    return(as_tibble(out))
  }

  new_temporal_cubble(
    out, key = key, index = index, coords = coords, spatial = spatial)
}

#' @export
#' @rdname dplyr
rename.spatial_cubble_df <- function(.data, ...){
  key <- key_vars(.data)
  index <- index_var(.data)
  coords <- coords(.data)
  loc <-  tidyselect::eval_rename(quote(c(...)), .data)
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  res <- NextMethod()

  if (1 %in% loc) key <- names(loc)[1]
  if (2 %in% loc) coords[1] <- names(which(loc == 2))
  if (3 %in% loc) coords[2] <- names(which(loc == 3))

  new_spatial_cubble(
    res, key = key, index = index, coords = coords)
}


#' @export
#' @rdname dplyr
rename.temporal_cubble_df <- function(.data, ...){

  key <- key_vars(.data)
  index <- index_var(.data)
  coords <- coords(.data)
  spatial <- spatial(.data)
  loc <-  tidyselect::eval_rename(quote(c(...)), .data)
  class(.data) <- setdiff(class(.data), cb_temporal_cls)
  res <- NextMethod()

  if (1 %in% loc) names(spatial)[1] <- key <- names(loc)[1]
  if (2 %in% loc) index <- names(which(loc == 2))

  new_temporal_cubble(
    res, key = key, index = index, coords = coords, spatial = spatial)
}

#' @export
#' @rdname dplyr
bind_rows.temporal_cubble_df <- function(..., .id = NULL){

  dots <- list2(...)
  all_temporal_cubble <- all(map_lgl(dots, is_cubble_temporal))
  same_key <- map_chr(dots, key_vars) %>% reduce(identical)
  same_index <- map_chr(dots, index_var) %>% reduce(identical)
  same_coords <- map_chr(dots, coords) %>% reduce(identical)
  if (!all_temporal_cubble)
    cli::cli_abort("All the objects needs to be temporal cubbles to bind.")

  if (!same_key) cli::cli_abort("All the objects needs to have the same key")
  if (!same_index) cli::cli_abort("All the objects needs to have the same index")
  if (!same_coords) cli::cli_abort("All the objects needs to have the same coords")

  class(.data) <- setdiff(class(.data), cb_temporal_cls)
  res <- NextMethod()
  spatial <- map(dots, spatial) %>% reduce(bind_rows)
  new_temporal_cubble(
    res, key = same_key, index = same_index, coords = same_coords, spatial = spatial)

}

#' @export
#' @rdname dplyr
bind_cols.spatial_cubble_df <- function(..., .name_repair){
  cli::cli_abort("Not yet support {.fn bind_cols}")
}

#' @export
#' @rdname dplyr
bind_cols.temporal_cubble_df <- function(..., .name_repair){
  cli::cli_abort("Not yet support {.fn bind_cols}")
}


#' @export
#' @rdname dplyr
rowwise.spatial_cubble_df <- function(data, ...){
  out <- NextMethod()
  new_spatial_cubble(
    out, key = key_vars(data), index = index_var(data), coords = coords(data))
}

#' @export
#' @rdname dplyr
rowwise.temporal_cubble_df <- function(data, ...){
  out <- NextMethod()
  new_temporal_cubble(
    out, key = key_vars(data), index = index_var(data),
    coords = coords(data), spatial = spatial(data))
}


#' @rdname dplyr
#' @export
dplyr_col_modify.cubble_df <- function(data, cols) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)

}

#' @rdname dplyr
#' @export
dplyr_row_slice.spatial_cubble_df <- function(data, i, ...){
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
dplyr_row_slice.temporal_cubble_df <- function(data, i, ...){
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
dplyr_reconstruct.spatial_cubble_df <- function(data, template) {

  if (!inherits(data, "tbl_df")) data <- as_tibble(data)
  if (is_sf(template)) data <- sf::st_as_sf(data, crs = sf::st_crs(template))

  new_spatial_cubble(
    data, key = key_vars(template),
    index = index_var(template), coords = coords(template)
  )
}

#' @rdname dplyr
#' @export
dplyr_reconstruct.temporal_cubble_df <- function(data, template) {
  #browser()

  key_var <- key_vars(template)
  key_vals <- unique(data[[key_var]])
  index_var <-  index_var(template)
  spatial <- spatial(template) %>% filter(!!sym(key_var) %in% key_vals)

  if (is_tsibble(template)){
    suppressWarnings(
      data <- tsibble::build_tsibble(data, key = key_var, index = index_var, ordered = FALSE)
    )

  }

  if (!inherits(data, "tbl_df")) data <- as_tibble(data)

  new_temporal_cubble(
    data, key = key_var, index = index_var, coords = coords(template),
    spatial = spatial
  )

}

globalVariables(c("groups"))


# those should ideally not needed
#' @export
#' @rdname dplyr
mutate.spatial_cubble_df <- function(.data, ...){

  data <- .data
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  res <- NextMethod()
  dplyr_reconstruct(res, data)
}


#' @export
#' @rdname dplyr
filter.spatial_cubble_df <- function(.data,...){
  data <- .data
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  res <- NextMethod()
  dplyr_reconstruct(res, data)
}

#' @export
dplyr::filter

#' @export
#' @rdname dplyr
arrange.spatial_cubble_df <- function(.data, ...){

  data <- .data
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  res <- NextMethod()
  dplyr_reconstruct(res, data)
}


