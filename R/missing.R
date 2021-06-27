#' @examples
#'
#' \dontrun{
#' out <- oz_global2 %>% add_missing_prct(vars = c("prcp", "tmin", "tmax"))
#' out2 <- out %>% add_missing_dscrb()
#' }
add_missing_prct <- function(data, vars){


  test_cubble(data)

  # different scenarios for long/ list-column form
  # now assume list-column form

  var_names <- data %>% zoom(data) %>% colnames()

  if (!all(vars %in% var_names)){
    bad_vars <- var[which(!vars %in% var_names )]
    abort(glue::glue("Variable {bad_vars} are not presented in the long form"))
  }


  calls <- map(vars,  ~quo(sum(is.na(data[[.x]]))/length(data[[.x]])))
  names(calls) <- glue::glue("{vars}_missing")
  data %>% mutate(missing = list(map(calls, ~eval(.x)))) %>% tidyr::unnest_auto(missing)

}


add_missing_dscrb <- function(data, cutoff = c(0, 0.95)){
  #test_cubble(data)

  all_names <- names(data)
  vars <- syms(all_names[stringr::str_detect(all_names, "missing")])

  cutoff_lower <- cutoff[1]
  cutoff_upper <- cutoff[2]

  calls <- map(vars, ~quo(ifelse(!!.x >= cutoff_upper, "almost all missing",
                                 ifelse(!!.x <= cutoff_lower, "almost no missing", "some missing"))))

  vars_name <- stringr::str_extract(vars, "[^_]+")
  names(calls) <- glue::glue("{vars_name}_dscrb")

  dscrb <- purrr::map_dfr(calls, ~eval_tidy(.x, data = data))
  data %>% dplyr::bind_cols(dscrb)


}
