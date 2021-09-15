#' Compute missing summary
#'
#' @details
#' * `add_missing_prct()` computes the percentage of missing for the selected variables
#' * `add_missing_dscrb()` gives a descriptive label (no missing, partly missing, and almost all missing)
#' for those variables that have missing percentage computed
#'
#' @param data a cubble object
#' @param ... variables to compute percentage missing (support tidyselect)
#' @param cutoff the threshold value, above which the variable
#'   will be described as "almost all missing".
#' @examples
#' climate_small %>% add_missing_prct(prcp:tmin)
#' climate_small %>% add_missing_prct(prcp:tmin) %>% add_missing_dscrb()
#' @rdname missing
#' @importFrom tidyselect eval_select
#' @export
add_missing_prct <- function(data, ...){

  test_cubble(data)
  key <- key_vars(data)

  exprs <- expr(...)
  vars <- tidyselect::eval_select(exprs, data %>% stretch())

  # different scenarios for long/nested form
  # now assume a nested form

  var_names <- data %>% stretch() %>% names()

  if (!all(names(vars) %in% var_names)){
    bad_vars <- vars[which(!vars %in% var_names )]
    abort(glue::glue("Variable not presented in the long form: {bad_vars}"))
  }


  calls <- map(names(vars),  ~quo(sum(is.na(ts[[.x]]))/length(ts[[.x]])))
  names(calls) <- glue::glue("{names(vars)}_missing")
  out <- data %>% mutate(missing = list(map(calls, ~eval(.x)))) %>% tidyr::unnest_wider(missing)

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             leaves = new_leaves(data, !!key), form = determine_form(data))
}

#' @rdname missing
#' @importFrom purrr map_dfr
#' @export
add_missing_dscrb <- function(data, cutoff = 0.99){
  test_cubble(data)

  key <- key_vars(data)
  all_names <- names(data)
  vars <- syms(all_names[grep("missing", all_names)])

  if (is_empty(vars)){
    abort("you need to first select the variables to compute missing percentage
          before using `add_missing_dscrb`.")
  }

  calls <- map(vars, ~quo(ifelse(!!.x >= cutoff, "almost all missing",
                                 ifelse(!!.x == 0, "no missing", "partly missing"))))

  names(calls) <- map_chr(vars, ~sub("missing", "dscrb", .x))

  dscrb <- purrr::map_dfr(calls, ~eval_tidy(.x, data = data))
  out <- tibble::as_tibble(data) %>% dplyr::bind_cols(dscrb)

  new_cubble(out,
             key = key, index = index(data), coords = coords(data),
             leaves = new_leaves(data,!!key), form = determine_form(data))

}
