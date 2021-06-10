#' @exportS3Method tidyr::nest
nest.single_cb <- function(dt, ...){

  test_single_sheet(dt)

  if ("tbl_ts" %in% class(dt)) {
    nested <- nest(as_tsibble(dt), ...)
  } else if ("tbl_df" %in% class(dt)){
    nested <- nest(as_tibble(dt), ...)
  }

  new_sheet_single(nested, by = by_var(dt), cb = cb(dt))

}

#' @exportS3Method dplyr::mutate
mutate.single_cb <- function(dt, ...){

  if ("tbl_ts" %in% class(dt)) {
    out <- mutate(as_tsibble(dt), ...)
  } else if ("tbl_df" %in% class(dt)){
    out <- mutate(as_tibble(dt), ...)
  }

  new_sheet_single(out, by = by_var(dt), cb = cb(dt))

}
