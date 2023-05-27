#' Compute missing summary
#'
#' @details
#' * `add_missing_prct()` computes the percentage of missing for the selected variables
#'
#' @param data a cubble object
#' @param vars variables to compute percentage missing (support tidyselect)
#' @examples
#' climate_aus %>%  add_missing_prct(vars = prcp)
#  climate_aus %>%  add_missing_prct(vars = prcp:tmin)
#  climate_aus %>%  add_missing_prct(vars = c(prcp, tmax, tmin))
#' @rdname missing
#' @importFrom tidyselect eval_select
#' @export
#' @return a cubble object with additional columns VAR_missing
add_missing_prct <- function(data, vars){

  test_cubble(data)
  key <- key_vars(data)

  exprs <- enexpr(vars)
  vars <- tidyselect::eval_select(exprs, face_temporal(data)) %>% names()

  # add_missing_prct should be called only in nested form
  var_names <- data %>%  face_temporal() %>%  names()

  if (!all(vars %in% var_names)){
    bad_vars <- vars[which(!vars %in% var_names )]
    abort(glue::glue("Variable not presented in the long form: {bad_vars}"))
  }

  calls <- map(vars,  ~expr(mean(is.na(ts[[!!.x]]))))
  names(calls) <- glue::glue("{vars}_missing")
  out <- data %>% mutate(!!!calls)

  new_spatial_cubble(out,
             key = key, index = index(data), coords = coords(data))
}

