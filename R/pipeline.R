#' Pipeline for index calculation
#'
#' @param data a cubble object
#' @param scale the time scale used for aggregation, in month
#' @param var the variable to aggregate on
#'
#' @return a long cubble
#' @export
#' @rdname pipeline
ts_aggregate <- function(data, scale, var){
  test_cubble(data)

  if (form(data) == "nested") data <- data %>% face_temporal()

  var <- enquo(var)
  id <- sym(key_vars(data))
  date <- sym(index(data))
  new_name <- paste0(as_label(var), "_agg")



  data %>%
    dplyr::mutate(!!new_name := c(
      rep(NA, scale-1), rowSums(stats::embed(!!var, scale), na.rm = TRUE)
      )) %>%
    stats::na.omit()
}


#' @param dist a distribution function, see details
#' @param gran the granulate for calculating the index, default to "month"
#' @param var the variable used to calculate the index
#'
#' @return a tibble object (end of a pipeline)
#' @export
#' @rdname pipeline
normalising <- function(data, dist, gran = "month", var){
  test_cubble(data)

  var <- enquo(var)
  id <- sym(key_vars(data))
  date <- sym(index(data))


  if (quo_is_null(var)){
    found <- grepl("_agg", colnames(data))
    if (any(found)){
      var <- quo(colnames(data)[found])
    } else{
      cli::cli_abort("Please specify the variable to normalising with {.code var =}")
    }
  }

  if (grepl("_agg", as_label(var))){
    var <- sym(sub("_agg","", quo_get_expr(var)))
  }

  created_fit <- paste0(as_label(var), "_fit")
  created_idx <- paste0(as_label(var), "_idx")

    data %>%
      dplyr::group_by(g = do.call(gran, list(!!date))) %>%
      dplyr::mutate(
        !!created_fit := do.call(dist, list(!!var, !!var)),
        !!created_idx := stats::qnorm(!!sym(created_fit))
      ) %>%
      dplyr::select(-.data$g)
}

