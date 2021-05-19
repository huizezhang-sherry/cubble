# create an S3 class for data cube from tibble/ tsibble to cube
# need to define time, id, and param

# thinking:
# better name for the packag and  cube

# small tasks:
# document variables in this script



#'@importFrom tibble new_tibble
is_cube <- function(x){
  inherits(x, "tbl_cube")
}


#' @export
tbl_format_setup.tbl_cube <- function(x, width, ...){
  setup <- NextMethod()
  setup$param <- param_var(x)
  setup$id <- id_var(x)
  setup$index <- index_var(x)
  setup
}

#' @importFrom crayon magenta
#' @export
tbl_format_header.tbl_cube <- function(x, setup,...){
  # can add more details of each attribution here
  paste0(crayon::magenta("A cube: "), setup$tbl_sum[1],
         crayon::magenta(" with \n"),
         crayon::magenta("param: "), setup$param, "\n",
         crayon::magenta("index: "), setup$index, "\n",
         crayon::magenta("id: "), setup$id
         )
}


#' @export
vec_ptype_abbr.tbl_cube <- function(x, ...){
  "cube"
}


#' casting and coercion
#' @export
#' @importFrom ellipsis check_dots_used
#' @examples
#' \dontrun{
#' water <- water_raw %>%
#' as_tsibble(key = c(parameter, station), index = time, regular = FALSE) %>%
#' as_cube(param = parameter)
#' pedestrian %>%
#' tidyr::pivot_longer(cols = Count, names_to = "count", values_to = "val") %>%
#' as_cube(param = count)
#' }
as_cube <- function(x, key = NULL, index, param = NULL, ...){
  # next step: currently need to first as_tsibble() and then as_cube() - should only need as_cube()
  UseMethod("as_cube")
}

as_cube.tbl_ts <- function(x, key = NULL, index, param = NULL, ...){

  param <- enquo(param)

  build_cube(x, param = !!param)
}

as_cube.default <- function(...){
    abort("Cube is a sub-class of tsibble - please first build a tsibble!")
}


#' build a cube from a tsibble (tbl_ts)
#' @export
build_cube <- function(x, key = NULL, index = NULL, param = NULL){
  param <- rlang::enquo(param)
  x <- tsibble::new_tsibble(x, "param" = quo_get_expr(param), class = "tbl_cube")
  x
}

param <- function(x){
  UseMethod("param")
}

param.tbl_cube <- function(x){
  x %@% param
}

param.default <- function(x){
  abort("The data needs to be of class tbl_cube to have a param attribute")
}

param_var <- function(x){
  as.character(param(x))
}

id <- function(x){
  UseMethod("id")
}

id.tbl_cube <- function(x){
  sym(setdiff(key_vars(x), param_var(x)))
}

id.default <- function(x){
  abort("The data needs to be of class tbl_cube to have an id attribute")
}

id_var <- function(x){
  as.character(id(x))
}
