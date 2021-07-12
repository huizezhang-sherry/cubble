#' Compute statistics for variable missingness
#'
#' @param data a cubble object
#' @param vars the variable to compute missingness
#' @param cutoff the lower and upper threshold value, below or above which the variable
#'   will be described as "almost no missing" and "almost all missing". ("some missing" for
#'   if in between). Only used in \code{add_missing_dscrb()}
#' @examples
#'
#' \dontrun{
#' out <- oz_global2 %>% add_missing_prct(vars = c("prcp", "tmin", "tmax"))
#' out2 <- out %>% add_missing_dscrb()
#' }
#' @rdname missing
#' @export
add_missing_prct <- function(data, vars){

  test_cubble(data)

  # different scenarios for long/ list-column form
  # now assume list-column form

  var_names <- data %>% zoom() %>% names()

  if (!all(vars %in% var_names)){
    bad_vars <- vars[which(!vars %in% var_names )]
    abort(glue::glue("Variable not presented in the long form: {bad_vars}"))
  }


  calls <- map(vars,  ~quo(sum(is.na(ts[[.x]]))/length(ts[[.x]])))
  names(calls) <- glue::glue("{vars}_missing")
  out <- data %>% mutate(missing = list(map(calls, ~eval(.x)))) %>% tidyr::unnest_wider(missing)

  cubble_df(out, group = group_vars(data), meta_data = meta(data), form = determine_form(data))
}

#' @rdname missing
#' @importFrom purrr map_dfr
#' @export
add_missing_dscrb <- function(data, cutoff = c(0, 0.95)){
  test_cubble(data)

  all_names <- names(data)
  vars <- syms(all_names[grep("missing", all_names)])

  cutoff_lower <- cutoff[1]
  cutoff_upper <- cutoff[2]

  calls <- map(vars, ~quo(ifelse(!!.x >= cutoff_upper, "almost all missing",
                                 ifelse(!!.x <= cutoff_lower, "almost no missing", "some missing"))))

  names(calls) <- map_chr(vars, ~sub("missing", "dscrb", .x))

  dscrb <- purrr::map_dfr(calls, ~eval_tidy(.x, data = data))
  out <- tibble::as_tibble(data) %>% dplyr::bind_cols(dscrb)

  cubble_df(out, group = group_vars(data), meta_data = meta(data), form = determine_form(data))

}
