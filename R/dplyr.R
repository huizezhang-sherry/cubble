#' Access to dplyr verbs
#' @param data,.data a cubble object(used as defined by the dplyr generic)
#' @param cols,i,template,...,.add,.drop,.id,.name_repair see [dplyr::dplyr_col_modify()], [dplyr::dplyr_row_slice()],
#' and [dplyr::dplyr_reconstruct()]
#' @param .by,.groups used by dplyr verbs
#'
#' @references https://dplyr.tidyverse.org/reference/dplyr_extending.html
#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct
#' @importFrom utils head
#' @rdname dplyr
#' @export
arrange.temporal_cubble_df <- function(.data, ...){
  out <- NextMethod()
  dplyr_reconstruct(out, .data)
}

#' @rdname dplyr
#' @export
select.spatial_cubble_df <- function(.data, ...){

  data <- .data
  class(.data) <- setdiff(class(.data), cb_spatial_cls)
  out <- NextMethod()
  cb_attrs <- c(key_vars(data), coords(data))
  attrs_no_in <- !cb_attrs %in% colnames(out)
  if (any(attrs_no_in)){
    attr_missing <- cb_attrs[which(attrs_no_in)]
    out[[attr_missing]] <- .data[[attr_missing]]
    cli::cli_alert_info("Missing attribute {.code {attr_missing}}, add it back.")
  }


  dplyr_reconstruct(out, data)
}

#' @rdname dplyr
#' @export
select.temporal_cubble_df <- function(.data, ...){
  # not working
  data <- .data
  class(.data) <- setdiff(class(.data), cb_temporal_cls)
  out <- NextMethod()
  cb_attrs <- c(key_vars(data), index_var(data))
  attrs_no_in <- !cb_attrs %in% colnames(out)
  if (any(attrs_no_in)){
    attr_missing <- cb_attrs[which(attrs_no_in)]
    out[[attr_missing]] <- .data[[attr_missing]]
    cli::cli_alert_info("Missing attribute {.code {attr_missing}}, add it back.")
  }
  dplyr_reconstruct(out, .data)
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
summarise.spatial_cubble_df <- function(.data, ..., .by = NULL, .groups = NULL){
  vars <- enquos(..., .named = TRUE)
  out <- NextMethod()

  new_spatial_cubble(
    out, key = key_vars(.data), index = index_var(.data), coords = coords(.data)
  )
}

#' @rdname dplyr
#' @export
summarise.temporal_cubble_df <- function(.data, ..., .by = NULL, .groups = NULL){
  vars <- enquos(..., .named = TRUE)
  index <- index_var(.data)
  key <- key(.data)
  exist_grp_vars <- dplyr::group_vars(.data)
  .data <- .data %>% group_by(!!!key, !!!syms(exist_grp_vars))
  out <- NextMethod()

  if (!index %in% colnames(out)){
    potential_index <- .data %@% groups %>% colnames() %>% utils::head(-1)
    potential_index <- setdiff(potential_index, key_vars(.data))
    new_temporal_cubble(
      out, key = key_vars(.data), index = potential_index, coords = coords(.data),
      spatial = spatial(.data)
    )

    # back to tsibble if possible?
    #new_tibble(out)
  } else{
    new_temporal_cubble(
      out, key = key_vars(.data), index = index_var(.data), coords = coords(.data),
      spatial = spatial(.data)
    )
  }

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

  key_var <- key_vars(template)
  key_vals <- key_data(template)$id
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


