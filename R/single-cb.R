#' Create a single cubble object
#' @param single_sheet one of the sheet in a cubble object
#' @param by the joining variable
#' @param cb the other sheet in the cubble object
#' @export
new_sheet_single <- function(single_sheet, by, cb){
  if(is_tsibble(single_sheet)){
    new_tsibble(single_sheet, by = by, cb = cb, class = "single_cb")
  } else if (tibble::is_tibble(single_sheet)){
    tibble::new_tibble(single_sheet, by = by, cb = cb, class = "single_cb")
  }

}


#' Test whether an object is a single cubble
#'
#' @param x a single cubble
#' @export
is_single_cubble <- function(x){
  inherits(x, "single_cb")
}

by_var <- function(x){
  x %@% by
}

cb <- function(x){
  x %@% cb
}

