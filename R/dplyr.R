#' Access to dplyr verbs
#' @param data,.data a cubble object(used as defined by the dplyr generic)
#' @param cols,i,template,... see [dplyr::dplyr_col_modify()], [dplyr::dplyr_row_slice()],
#' and [dplyr;:dplyr_reconstruct()]
#'
#' @references \link{https://dplyr.tidyverse.org/reference/dplyr_extending.html}
#' @importFrom dplyr dplyr_col_modify dplyr_row_slice dplyr_reconstruct select
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
  if (inherits(template, "sf")) data <- sf::st_as_sf(data, crs = sf::st_crs(template))

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

  if (inherits(template, "tbl_ts")){
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

#' @export
summarise.cubble_df <- function(.data, ..., .by, .groups){
  data <- .data
  key <- key_vars(data)
  spatial <- spatial(data)
  origin <- data
  class(data) <- class(data)[class(data) != "cubble_df"]
  out <- NextMethod()

  dplyr_reconstruct(out, origin)

}


#' @export
#' @importFrom dplyr group_by_prepare
group_by.cubble_df <- function(.data, ..., .add, .drop){
  data <- .data
  key <- key_vars(data)
  groups <- dplyr::group_by_prepare(data, ..., .add = TRUE)
  group_var <- groups$group_names
  index <- setdiff(group_var,key)
  out <- groups$data

  new_cubble(out,
             key = c(key, index), index = index, coords = coords(data),
             spatial = spatial(data))
}

#' @export
ungroup.cubble_df <- function(x, ...){
  data <- x
  ungroup_var <- names(enquos(..., .named = TRUE))

  if (!all(ungroup_var %in% names(data))){
    problem <- ungroup_var[!ungroup_var %in% names(data)]
    cli::cli_abort("the ungroup variable: {problem} is not found in the data")
  }

  if (key_vars(data)[1] %in% ungroup_var){
    cli::cli_abort("Can't ungroup the spatio identifier!")
  }


  updated_group_var <- setdiff(key_vars(data), ungroup_var)

  new_cubble(data,
             key = updated_group_var, index = index(data), coords = coords(data),
             spatial = spatial(data))
}

