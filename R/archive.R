#' # create an S3 class for data cube from tibble/ tsibble to cube
#' # need to define time, id, and param
#'
#' # thinking:
#' # better name for the packag and  cube
#'
#' # small tasks:
#' # document variables in this script
#'
#'
#'
#' #'@importFrom tibble new_tibble
#' is_cube <- function(x){
#'   inherits(x, "tbl_cube")
#' }
#'
#'
#' #' @export
#' tbl_format_setup.tbl_cube <- function(x, width, ...){
#'   setup <- NextMethod()
#'   setup$param <- param_var(x)
#'   setup$id <- id_var(x)
#'   setup$index <- index_var(x)
#'   setup
#' }
#'
#' #' @importFrom crayon magenta
#' #' @export
#' tbl_format_header.tbl_cube <- function(x, setup,...){
#'   # can add more details of each attribution here
#'   paste0(crayon::magenta("A cube: "), setup$tbl_sum[1],
#'          crayon::magenta(" with \n"),
#'          crayon::magenta("param: "), setup$param, "\n",
#'          crayon::magenta("index: "), setup$index, "\n",
#'          crayon::magenta("id: "), setup$id
#'          )
#' }
#'
#'
#' #' @export
#' vec_ptype_abbr.tbl_cube <- function(x, ...){
#'   "cube"
#' }
#'
#'
#' #' casting and coercion
#' #' @export
#' #' @importFrom ellipsis check_dots_used
#' #' @examples
#' #' \dontrun{
#' #' water <- water_raw %>%
#' #' as_tsibble(key = c(parameter, station), index = time, regular = FALSE) %>%
#' #' as_cube(param = parameter)
#' #' pedestrian %>%
#' #' tidyr::pivot_longer(cols = Count, names_to = "count", values_to = "val") %>%
#' #' as_cube(param = count)
#' #' }
#' as_cube <- function(x, key = NULL, index, param = NULL, ...){
#'   # next step: currently need to first as_tsibble() and then as_cube() - should only need as_cube()
#'   UseMethod("as_cube")
#' }
#'
#' as_cube.tbl_ts <- function(x, key = NULL, index, param = NULL, ...){
#'
#'   param <- enquo(param)
#'
#'   build_cube(x, param = !!param)
#' }
#'
#' as_cube.default <- function(...){
#'     abort("Cube is a sub-class of tsibble - please first build a tsibble!")
#' }
#'
#'
#' #' build a cube from a tsibble (tbl_ts)
#' #' @export
#' build_cube <- function(x, key = NULL, index = NULL, param = NULL){
#'   param <- rlang::enquo(param)
#'   x <- tsibble::new_tsibble(x, "param" = quo_get_expr(param), class = "tbl_cube")
#'   x
#' }
#'
#' param <- function(x){
#'   UseMethod("param")
#' }
#'
#' param.tbl_cube <- function(x){
#'   x %@% param
#' }
#'
#' param.default <- function(x){
#'   abort("The data needs to be of class tbl_cube to have a param attribute")
#' }
#'
#' param_var <- function(x){
#'   as.character(param(x))
#' }
#'
#' id <- function(x){
#'   UseMethod("id")
#' }
#'
#' id.tbl_cube <- function(x){
#'   sym(setdiff(key_vars(x), param_var(x)))
#' }
#'
#' id.default <- function(x){
#'   abort("The data needs to be of class tbl_cube to have an id attribute")
#' }
#'
#' id_var <- function(x){
#'   as.character(id(x))
#' }

#' #' Slice the data cube into lower dimension
#' #' @param dt the raw dataframe to slice, in long form - does it has to be long form?
#' #' @param key the dimension to slice
#' #'
#' #' @return a nested tibble/ tsibble - think through here
#' #' @examples
#' #' \dontrun{
#' #' water_raw %>% find_non_varying(station, parameter, time)
#' #' }
#' #' @export
#' find_non_varying <- function(dt, ...){
#'   key <- ensyms(...)
#'   # currently don't process the non-varying variables for the index axis
#'   include_index <- any(key == index(dt))
#'   key <- key[key != index(dt)]
#'
#'   out <- unique(unlist(map(key, find_one, dt = dt)))
#'   if(include_index) out <- c(out, as_label(index(dt)))
#'   names(out) <- NULL
#'   out
#'
#' }
#'
#'
#' find_one <- function(dt, key){
#'   subset <- dt %>% as_tibble() %>% tidyr::nest(data = -(!!key)) %>% pull(data) %>% .[[1]]
#'   var_length <- map_dbl(colnames(subset), ~nrow(unique(subset[.x])))
#'   c(as_name(key), colnames(subset)[var_length == 1])
#'
#' }

#' #' Profile the data cube
#' #' @param dt the data cube to profile
#' #'
#' #' @return  here should be a customised proint of the profile
#' #' @export
#' profile_cube <- function(dt){
#'
#'   #first step is to detect the three axis
#'   # produce a vector of axis (axis <- c(station, time, param))
#'
#'   # map(axis, profile_single)
#'
#'   # then customise a print for the profile
#'   # - id <station>
#'   #   - name
#'   #   - lat
#'   #   - long
#'   # - param <parameter>
#'   # - time <time: interval>
#'   #
#' }
#'
#'
#' profile_single <- function(dt, axis){
#'
#'   # axis <- enquo(axis)
#'   # distinct_class <- dt %>% dplyr::distinct(!!axis) %>% nrow()
#'   #
#'   # count_table <- dt %>%
#'   #   dplyr::group_by(!!axis) %>%
#'   #   dplyr::summarise_all(dplyr::n_distinct) %>%
#'   #   dplyr::ungroup() %>%
#'   #   dplyr::summarise_if(is.numeric, sum)
#'   #
#'   # var_non_varying <- names(count_table)[count_table==distinct_class]
#'   # var_varying <- names(count_table)[count_table!=distinct_class]
#'   #
#'   # # extend the output for the case where all the variables are var_varying
#'   # tibble::tibble(!!axis := var_non_varying)
#' }
#'

#' #' Aggregate the time axis
#' #'
#' #' @param dt the data cube
#' #' @param val the value column to aggregate
#' #' @param unit the time unit to aggregate into, currently support "y", "ym", "ymd"
#' #' @param op a function to summarise by
#' #' @return a cube with index being aggregated to the instructed value
#' #'
#' #' @examples
#' #' \dontrun{
#' #'  water <- as_cube(water_raw, key = station, index = time, var = parameter)
#' #'  a <- aggregate_time(water, value, "ymd", mean)
#' #' }
#' #' @export
#' aggregate_time <- function(dt, val, unit, op){
#'
#'   if (!is_cube(dt)){
#'     abort("the data supplied needs to be of type tbl_cube!")
#'   }
#'
#'   val <- ensym(val)
#'
#'   check_unit(unit)
#'
#'   index_col <- index(dt)
#'   param_col <- param(dt)
#'   id_col <- id(dt)
#'   key_col <- c(as_label(param_col), as_label(id_col))
#'
#'   col <- c(index_col, param_col, id_col)
#'   var_to_group <- find_non_varying(dt, !!!col)
#'
#'   # next step: allow different op for different parameter
#'   # next step: allow lambda function for aggregation: can be useful for transformation: (.x)^(1/3)
#'   dt %>%
#'     as_tibble() %>%
#'     dplyr::mutate(!!index_col := recompute_date(unit, !!sym(index_col))) %>%
#'     dplyr::group_by(!!!syms(var_to_group)) %>%
#'     dplyr::summarise({{val}} :=  exec(op, {{val}})) %>%
#'     ungroup() %>%
#'     as_tsibble(index = !!index_col, key = all_of(key_col)) %>%
#'     build_cube(param = !!param_col)
#' }
#'
#'
#' #' @keywords internal
#' #' @importFrom glue glue glue_collapse
#' check_unit <- function(unit){
#'
#'   supported_units <- c("y", "ym", "ymd")
#'
#'   if(!unit %in% supported_units){
#'     units <- glue::glue_collapse(supported_units, last = " ,and ")
#'     abort(glue::glue("only {units} are currently supported"))
#'   }
#'
#' }
#'
#' #' @keywords internal
#' #' @importFrom lubridate year month day
#' find_unit_fun <- function(unit){
#'
#'   char <- as.vector(strsplit(unit, "")[[1]])
#'
#'   find_fun <- function(char){
#'     if (char == "y"){
#'       fun <- lubridate::year
#'     } else if (char == "m"){
#'       fun <- lubridate::month
#'     } else if (char == "d"){
#'       fun <- lubridate::day
#'     } else{
#'       abort(glue::glue("can't find the function to convert {char}"))
#'     }
#'
#'     fun
#'   }
#'
#'   map(char, find_fun)
#'
#' }
#'
#' #' @keywords internal
#' #' @examples
#' #' a <- as_datetime("2020-04-21 01:47:59")
#' #' recompute_date("ymd", a)
#' #' recompute_date("ym", a)
#' #' recompute_date("y", a)
#' recompute_date <- function(unit, var){
#'
#'   do.call(lubridate::make_date,
#'           map(find_unit_fun(unit), ~exec(.x, var)))
#'
#' }

#' #' Aggregate the id axis based on an id-invariant variable
#' #'
#' #' @param dt the data cube
#' #' @param weight a weighting matrix for multivariate parameters
#' #' @return a cube with parameters being aggregated
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(stringr)
#' #' water_small_cube <- water_small %>%
#' #'   mutate(river_name = str_extract(name, "^[^\\@]+ "),
#' #'          river_name = as.factor(str_trim(river_name)),
#' #'          river_name = tolower(word(river_name, 1, 2, sep = " "))) %>%
#' #'   as_tsibble(key = c(parameter, station), index = date) %>%
#' #'   as_cube(param = parameter)
#' #' v <- c(0.1, 0.2, 0.25, 0.3, 0.1, 0.05)
#' #' aggregate_param(water_small_cube, v)
#' #' }
#' #' @export
#' aggregate_param <- function(dt, weight){
#'
#'   # linear combinations would make more sense if values are standardised first
#'   # think: how to supply more than one linear combination
#'   # in the meta it would be nice to show: param: comb_1: Electrical_conductivity * 0.1 + ...
#'
#'   param_col <- param(dt)
#'   id_col <- id(dt)
#'   index_col <- index(dt)
#'   id_non_varying <- find_non_varying(dt, !!id_col) %>% map(as.symbol)
#'
#'   unique_param <- unique(dt %>% pull(!!param_col))
#'
#'   multiplies <- map2(syms(unique_param), weight, ~expr(!!.x * !!.y))
#'   single <- purrr::reduce(multiplies, ~expr(!!.x + !!.y))
#'
#'   dt %>%
#'     as_tibble() %>%
#'     tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
#'     group_by(!!!id_non_varying, !!index_col) %>%
#'     transmute(comb_1 = eval(single)) %>%
#'     ungroup() %>%
#'     as_tsibble(index = !!index_col, key = c(!!id_col)) %>%
#'     build_cube(param = comb_1)
#'
#' }
#'

#' #' Aggregate the id axis based on an id-invariant variable
#' #'
#' #' @param dt the data cube
#' #' @param val the value column to aggregate
#' #' @param along the id-invariant variable to aggregate upon
#' #' @param ... additional operations on the `along` variable
#' #' @param FUN a function to aggregate by
#' #' @return a cube with id being aggregated
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(stringr)
#' #' water_small_cube <- water_small %>%
#' #'   mutate(river_name = str_extract(name, "^[^\\@]+ "),
#' #'          river_name = as.factor(str_trim(river_name)),
#' #'          river_name = tolower(word(river_name, 1, 2, sep = " "))) %>%
#' #'   as_tsibble(key = c(parameter, station), index = date) %>%
#' #'   as_cube(param = parameter)
#' #' aggregate_id(water_small_cube, value, long, ~ggplot2::cut_interval(.x, 3))
#' #' aggregate_id(water_small_cube, val = value, along = river_name)
#' #' }
#' #' @export
#' aggregate_id <- function(dt, val, along, ..., FUN = mean){
#'
#'   # issue: cut_interval would approximate a decimal number to integer before cutting: 143.xxx -> 144 -> [144, 145]
#'
#'   along <- ensym(along)
#'   val <- ensym(val)
#'   if (!is_missing(...)) expr <- purrr::as_mapper(...)
#'
#'   param_col <- param(dt)
#'   id_col <- id(dt)
#'   index_col <- index(dt)
#'
#'
#'   if (!vec_in(as_label(along),find_non_varying(dt, !!id_col))){
#'     abort(glue::glue("{along} must be invariant to {id_col}!"))
#'   }
#'
#'   if (is.numeric(eval_tidy(along, dt))){
#'     dt <- dt %>%
#'       as_tibble() %>%
#'       mutate({{along}} := expr(eval_tidy(along, dt))) %>%
#'       as_tsibble(index = !!index_col, key = c(!!id_col, !!param_col)) %>%
#'       build_cube(param = !!param_col)
#'   }
#'
#'   dt %>%
#'     as_tibble() %>%
#'     group_by(!!along,!!param_col, !!index_col) %>%
#'     dplyr::summarise({{val}} := exec(FUN, {{val}}, na.rm = TRUE)) %>%
#'     ungroup() %>%
#'     as_tsibble(index = !!index_col, key = c(!!along, !!param_col)) %>%
#'     build_cube(param = !!param_col)
#' }
#'
