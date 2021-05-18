#' Aggregate the time axis
#'
#' @param dt the data cube
#' @param val the value column to aggregate
#' @param unit the time unit to aggregate into, currently support "y", "ym", "ymd"
#' @param op a function to summarise by
#' @return a cube with index being aggregated to the instructed value
#'
#' @examples
#' \dontrun{
#'  water <- as_cube(water_raw, key = station, index = date, var = parameter)
#'  a <- aggregate_time(water, value, "ymd", mean)
#' }
#' @export
aggregate_time <- function(dt, val, unit, op){

  if (!is_cube(dt)){
    abort("the data supplied needs to be of type tbl_cube!")
  }

  val <- ensym(val)

  check_unit(unit)

  # next step: implement index(), key(), and var() to extract attribut
  index_col <- attr(dt, "index")
  var_col <- attr(dt, "var")
  key_col <- attr(dt, "key")

  col <- c(index_col, var_col, key_col)
  # next step: speed up find_non_varying()
  var_to_group <- union(col, unlist(map(col, ~find_non_varying(dt, !!sym(.x)))))

  # next step: allow different op for different parameter
  # next step: allow lambda function for aggregation: can be useful for transformation: (.x)^(1/3)
  out <- dt %>%
    dplyr::mutate(!!index_col := recompute_date(unit, !!sym(index_col))) %>%
    dplyr::group_by(!!!syms(var_to_group)) %>%
    dplyr::summarise(!!val := exec(op, !!val)) %>%
    ungroup()

  out

}

#' @keywords internal
#' @importFrom glue glue glue_collapse
check_unit <- function(unit){

  supported_units <- c("y", "ym", "ymd")

  if(!unit %in% supported_units){
    units <- glue::glue_collapse(supported_units, last = " ,and ")
    abort(glue::glue("only {units} are currently supported"))
  }

}

#' @keywords internal
#' @importFrom lubridate year month day
find_unit_fun <- function(unit){

  char <- as.vector(strsplit(unit, "")[[1]])

  find_fun <- function(char){
    if (char == "y"){
      fun <- lubridate::year
    } else if (char == "m"){
      fun <- lubridate::month
    } else if (char == "d"){
      fun <- lubridate::day
    } else{
      abort(glue::glue("can't find the function to convert {char}"))
    }

    fun
  }

  map(char, find_fun)

}

#' @keywords internal
#' @examples
#' a <- as_datetime("2020-04-21 01:47:59")
#' recompute_date("ymd", a)
#' recompute_date("ym", a)
#' recompute_date("y", a)
recompute_date <- function(unit, var){

  do.call(lubridate::make_date,
          map(find_unit_fun(unit), ~exec(.x, var)))

}
