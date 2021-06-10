#' Create a cubble object
#' @param main the main sheet (a tsibble)
#' @param item the item sheet (a tibble)
#' @param by the variable that join the two sheets
#' @examples
#' df <- cubble(climate, station, by = c("station" = "id"))
#' @rdname cubble-package
#' @export
cubble <- function(main, item, by = NULL){
  # check whether main is a tsibble
  tsibble::is_tsibble(main)

  # check whether item is a tibble
  tibble::is_tibble(item)

  # check if key presents
  main_names <- colnames(main)
  item_names <- colnames(item)
  all_names <- vctrs::vec_c(main_names, item_names)
  if (is.null(by)){
    by <- all_names[duplicated(all_names)]
    if (vctrs::vec_size(by) == 0){
      rlang::abort(glue::glue("`by` needs to be supplied if no common variable between {main} and {item}"))
    } else if (vctrs::vec_size(by) > 1){
      rlang::abort(glue::glue("cubble curently only supports one key variable, check if there are duplicated column names"))
    }
  } else{
    # assume c("x" = "y") structure
    if (!vec_in(names(by), main_names) ){
      rlang::abort(glue::glue("{names(by)} must present in {main}"))
    }

    if (!vec_in(unname(by), item_names)){
      rlang::abort(glue::glue("{unname(by)} must present in {item}"))
    }

    by <- c(names(by), unname(by))
  }

  # check whether each station is a separate row in item
  if (vec_duplicate_any(item[["by"]])){
    rlang::abort(glue::glue("duplicated value of {by} in {item}"))
  }

  new_cubble(main = main, item = item, by = by)
}

#' @rdname cubble-package
new_cubble <- function(main, item, by){

  new_list_of(list(main = main, item = item), by = by, class = "tbl_cb")

}


#' @rdname cubble-package
as_cubble <- function(){

}

vec_ptype_full.tbl_cb <- function(x, ...) "cubble"
vec_ptype_abbr.tbl_cb <- function(x, ...) "cb"

#' Test whether an object is a cubble
#'
#' @param x a cubble object
#' @export
is_cubble <- function(x) {
  inherits(x, 'tbl_cb')
}

by_var <- function(x){
  x %@% by
}



# print

# cli head 3 main: climate: A tsibble: 487,639 x 13 [1D]
# 5 rows
# # cli head 3item: station: A tibble: 1,451 x 6
# 5 rows
