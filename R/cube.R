# create an S3 class for data cube from tibble/ tsibble to cube
# follow https://vctrs.r-lib.org/articles/s3-vector.html
# learn how a tsibble is built: https://github.com/tidyverts/tsibble/blob/master/R/as-tsibble.R
# need to define time, id, and param

#'@import vctrs
#'@importFrom tibble new_tibble
is_cube <- function(x){
  inherits(x, "data.frame")
}

tbl_sum.tbl_cube <- function(x, ...){
  c("A cube" = "hellow world!")
}

# tbl_format_header.tbl_cube <- function(x, setup,...){
#   # customise the header for cube: https://cran.r-project.org/web/packages/pillar/vignettes/extending.html
# }


#' @export
vec_ptype_abbr.cube <- function(x, ...){
  "cube"
}


#' casting and coercion
#' @export

as_cube <- function(x, key = NULL, index, var = NULL, ...){
  check_dots_used()
  UseMethod("as_cube")
}

#' @keywords internal
#' @export
as_cube.tbl_df <- function(x, key = NULL, index = NULL, var = NULL, ...){
  build_cube(x, key = key, index = index, var = var)
}

#' @keywords internal
#' @export
as_cube.tsibble <- function(x, key = NULL, index = NULL, var = NULL, ...){
  build_cube(x, key = key, index = index, var = var)
}




#' build a cube from tibble(tbl_df) or tsibble (tbl_ts)
#' @export
build_cube <- function(x, key = NULL, index = NULL, var = NULL){
  UseMethod("build_cube")
}

#' @keywords internal
#' @export
build_cube.tbl_df <- function(x, key = NULL, index = NULL, var = NULL, ...){
  x <- tibble::new_tibble(x, "index" = index, nrow = vec_size(x), class = "tbl_cube")
  class(x) <- c(class, class(x))
  x
}

#' @keywords internal
#' @export
build_cube.tbl_ts <- function(x, key = NULL, index = NULL, var = NULL, ...){

  var <- rlang::enquo(var)
  x <- tibble::new_tibble(x, "var" = var, nrow = vec_size(x), class = "tbl_cube")
  class(x) <- c(class, class(x))
  x
}
