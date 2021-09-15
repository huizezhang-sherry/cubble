#' Constructor for the cubble class
#'
#' @param ... a list object to create new cubble
#' @param data the object to be created or tested as cubble
#' @param group the spatio identifier
#' @param leaves metadata to include in the attributes
#' @param form whether the long or wide form
#' @rdname cubble-class
#' @examples
#' # create a nested tibble
#' library(tibble)
#' dt <- tibble(
#'   station = c("A", "B"),
#'   long = c(110, 120),
#'   lat = c(-10, -20),
#'   ts = list(
#'     tibble(
#'       date = c(
#'         as.Date("2021-07-01"),
#'         as.Date("2021-08-01")
#'       ),
#'       prcp = c(0, 0),
#'       tmax = c(20, 125),
#'       tmin = c(15, 18)
#'     ),
#'     tibble(
#'       date = c(
#'         as.Date("2021-07-01"),
#'         as.Date("2021-08-01")
#'       ),
#'       prcp = c(0, 100),
#'       tmax = c(10, 15),
#'       tmin = c(5, 8)
#'     )
#'   )
#' )
#'
#' # create a cubble from `dt`
#' cubble(dt,
#'        group = station,
#'        leaves = tibble(
#'          station = c("A", "B"),
#'          long = c(110, 120),
#'          lat = c(-10, -20)
#'        ),
#'        form = "long"
#' )
#' @export
cubble <- function(..., key, index, coords, leaves, form) {
  data <- tibble::tibble(!!!list2(...))
  key <- enquo(key)
  leaves <- new_leaves(leaves, !!key)
  new_cubble(data, key = as_name(key), index = as_name(index),
             coords = coords, leaves = leaves, form = form)

}

#' @rdname cubble-class
#' @export
new_cubble <- function(data, key, index, coords, leaves, form) {

  key_data <- group_data(dplyr::grouped_df(data, vars = key))

  attr <- list(x = data,
               key = key_data,
               index = index,
               coords = coords,
               leaves = leaves,
               form = form,
               class = "cubble_df")
  tsibble_attr <- NULL

  # if ("tbl_ts" %in% class(data)){
  #
  #   # `key` attribute is not included since it is already there
  #   tsibble_attr_name <- c("index", "index2", "interval")
  #   tsibble_attr <- list(data %@% "index",
  #                        data %@% "index2",
  #                        data %@% "interval")
  # }
  # attr <- c(attr, tsibble_attr)

  if (form == "nested"){
    names(attr)[1] <- "data"
    out <- rlang::exec("new_rowwise_df", !!!attr)
  } else if (form == "long"){
    out <- rlang::exec("new_grouped_df", !!!attr)
  }

  out
}

#' @rdname cubble-class
#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {
  key <- key_vars(data)
  key_n <- map_dbl(key, ~length(unique(key_data(data)[[.x]])))
  key_msg <- glue::glue_collapse(glue::glue("{key} [{key_n}]"), sep = ", ")


  if (form(data) == "nested"){
    variant <- leaves(data) %>% variant()
    var_names <- names(variant)
    var_type <- variant

  } else if (form(data) == "long"){
    invariant <- leaves(data) %>% invariant()
    var_names <- names(invariant)
    var_type <- invariant
  }
  leaves_msg <- glue::glue_collapse(glue::glue("{var_names} [{var_type}]"), sep = ", ")


  if(form(data) == "nested"){
    item <- key_vars(data)[1]
    msg <- glue::glue("{item}-wise: nested form")
  } else if(form(data) == "long"){
    msg <- glue::glue("time-wise: long form")
  }

  if ("tbl_ts" %in% class(data)){
    msg <- glue::glue("{msg} [tsibble]")
  }

  c(
    "Cubble" = msg,
    "Key" = key_msg,
    "Leaves" = leaves_msg

  )
}

#' @rdname cubble-class
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}

