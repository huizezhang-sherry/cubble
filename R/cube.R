# create an S3 class for data cube from tibble/ tsibble to cube
# follow https://vctrs.r-lib.org/articles/s3-vector.html
# learn how a tsibble is built: https://github.com/tidyverts/tsibble/blob/master/R/as-tsibble.R
# need to define time, id, and param

#'@importFrom tibble new_tibble
is_cube <- function(x){
  inherits(x, "tbl_cube")
}


# new implement some detection of the three axis and use that as the printed header
tbl_sum.tbl_cube <- function(x, ...){
  c("Cube" = "structure is cube")
}


#' @export
tbl_format_setup.tbl_cube <- function(x, width, ...){
  setup <- NextMethod()
  setup$key <- if (is_tibble(x %@% key)) key_vars(x) else x %@% key
  setup$var <- x %@% var
  setup$index <- as_character(x %@% index)
  setup$tbl_sum <- c(setup$tbl_sum,
                     "var" = var,
                     "key" = key,
                     "index" = index)
  setup
}

#' @importFrom crayon magenta
#' @export
tbl_format_header.tbl_cube <- function(x, setup,...){
  # can add more details of each attribution here
  paste0(crayon::magenta("A cube with"), "\n",
         crayon::magenta("var: "), setup$var, "\n",
         crayon::magenta("index: "), setup$index, "\n",
         crayon::magenta("key: "), setup$key)
}


#' @export
vec_ptype_abbr.cube <- function(x, ...){
  "cube"
}


#' casting and coercion
#' @export
#' @importFrom ellipsis check_dots_used
#' @examples
#' \dontrun{
#' water <- as_cube(water_small, index = date, key = station, var = parameter)
#' attributes(water)
#' ped_cube <- as_cube(pedestrian, var = trip)
#' attributes(ped_cube)
#' }

as_cube <- function(x, key = NULL, index, var = NULL, ...){
  ellipsis::check_dots_used()
  UseMethod("as_cube")
}

#' @keywords internal
#' @export
as_cube.tbl_df <- function(x, key = NULL, index = NULL, var = NULL, ...){

  key <- enquo(key)
  var <- enquo(var)
  index <- enquo(index)

  build_cube(x, key = !!key, index = !!index, var = !!var)
}

#' @keywords internal
#' @export
as_cube.tbl_ts <- function(x, key = NULL, index = NULL, var = NULL, ...){

  key <- key_vars(x)
  index <- index(x)
  var <- enquo(var)

  build_cube(x, key = !!key, index = !!index, var = !!var)
}

#' @keywords internal
#' @export
#' @importFrom rlang abort
as_cube.default <- function(x, ...){
  rlang::abort("don't know how to convert `x` into a cube")
}


#' build a cube from tibble(tbl_df) or tsibble (tbl_ts)
#' @export
build_cube <- function(x, key = NULL, index = NULL, var = NULL){
  #browser()
  UseMethod("build_cube")
}

#' @keywords internal
#' @export
build_cube.tbl_df <- function(x, key = NULL, index = NULL, var = NULL, ...){
  index <- rlang::enquo(index)
  var <- rlang::enquo(var)
  key <- rlang::enquo(key)
  x <- tibble::new_tibble(x,
                          "index" = quo_name(index),
                          "var" = quo_name(var),
                          "key" = quo_name(key),
                          nrow = vec_size(x), class = "tbl_cube")
  x
}

#' @keywords internal
#' @export
build_cube.tbl_ts <- function(x, key = NULL, index = NULL, var = NULL, ...){
  var <- rlang::enquo(var)
  # this part is not good enough - currently var is a quosure and ideally it should be a string
  x <- tsibble::new_tsibble(x, "var" = quo_get_expr(var), class = "tbl_cube")
  x
}
