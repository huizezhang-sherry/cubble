#' Compute missing summary
#'
#' @details
#' * `add_missing_prct()` computes the percentage of missing for the selected variables
#'
#' @param data a cubble object
#' @param ... variables to compute percentage missing (support tidyselect)
#' @examples
#' climate_aus %>% add_missing_prct(prcp:tmin)
#' @rdname missing
#' @importFrom tidyselect eval_select
#' @export
add_missing_prct <- function(data, ...){

  test_cubble(data)
  key <- key_vars(data)

  exprs <- expr(...)
  vars <- tidyselect::eval_select(exprs, data %>% face_temporal())

  # add_missing_prct should be called only in nested form
  var_names <- data %>% face_temporal() %>% names()

  if (!all(names(vars) %in% var_names)){
    bad_vars <- vars[which(!vars %in% var_names )]
    abort(glue::glue("Variable not presented in the long form: {bad_vars}"))
  }


  calls <- map(names(vars),  ~quo(sum(is.na(ts[[.x]]))/length(ts[[.x]])))
  names(calls) <- glue::glue("{names(vars)}_missing")
  out <- data %>% mutate(missing = list(map(calls, ~eval(.x)))) %>% tidyr::unnest_wider(missing)

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             spatial = NULL, form = "nested")
}

