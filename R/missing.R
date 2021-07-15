#' Compute statistics for variable missingness
#'
#' @param data a cubble object
#' @param ... the variable to compute \code{add_missing_prct()}
#' @param cutoff the threshold value, above which the variable
#'   will be described as "almost all missing". Only used in \code{add_missing_dscrb()}
#' @examples
#'
#' \dontrun{
#' out <- oz_global2 %>% add_missing_prct(vars = c("prcp", "tmin", "tmax"))
#' out2 <- out %>% add_missing_dscrb()
#' }
#' @rdname missing
#' @export
add_missing_prct <- function(data, ...){

  test_cubble(data)

  exprs <- expr(...)
  vars <- tidyselect::eval_select(exprs, data %>% zoom())

  # different scenarios for long/ list-column form
  # now assume list-column form

  var_names <- data %>% zoom() %>% names()

  if (!all(names(vars) %in% var_names)){
    bad_vars <- vars[which(!vars %in% var_names )]
    abort(glue::glue("Variable not presented in the long form: {bad_vars}"))
  }


  calls <- map(names(vars),  ~quo(sum(is.na(ts[[.x]]))/length(ts[[.x]])))
  names(calls) <- glue::glue("{names(vars)}_missing")
  out <- data %>% mutate(missing = list(map(calls, ~eval(.x)))) %>% tidyr::unnest_wider(missing)

  cubble_df(out, group = group_vars(data), meta_data = meta(data), form = determine_form(data))
}

#' @rdname missing
#' @importFrom purrr map_dfr
#' @export
add_missing_dscrb <- function(data, cutoff = 0.99){
  test_cubble(data)

  all_names <- names(data)
  vars <- syms(all_names[grep("missing", all_names)])


  calls <- map(vars, ~quo(ifelse(!!.x >= cutoff, "almost all missing",
                                 ifelse(!!.x == 0, "no missing", "partly missing"))))

  names(calls) <- map_chr(vars, ~sub("missing", "dscrb", .x))

  dscrb <- purrr::map_dfr(calls, ~eval_tidy(.x, data = data))
  out <- tibble::as_tibble(data) %>% dplyr::bind_cols(dscrb)

  cubble_df(out, group = group_vars(data), meta_data = meta(data), form = determine_form(data))

}
