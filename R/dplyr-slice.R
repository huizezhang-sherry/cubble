# helper
slice_factory <- function(f, ...){
  function(data, ...){
    key <- key_vars(data)
    spatial <- spatial(data)
    index <- index(data)
    coords <- coords(data)
    data <- tibble::as_tibble(data)
    out <- NextMethod()

    new_cubble(out,
               key = key , index = index , coords = coords,
               spatial = spatial, form = determine_form(data))
  }
}

#' Slicing a cubble
#'
#' Slicing can be useful when the number of site is too large to be all visualised in a
#' single plot. The slicing family in cubble wraps around the [dplyr::slice()] family to
#' allow slicing from top and bottom, based on a variable, or in random.
#'
#' @param data a cubble object to slice
#' @param ... other arguments passed to the [dplyr::slice()]
#' @examples
#' # slice the first 50 stations from the top/ bottom
#' library(dplyr)
#' climate_aus %>% slice_head(n = 50)
#' climate_aus %>% slice_tail(n = 50)
#'
#' # slice based on the max/ min of a variable
#'
#' climate_aus %>% slice_max(elev, n = 10)
#' climate_aus %>% slice_min(lat, n = 10)
#'
#' # random sample
#' climate_aus %>% slice_sample(n = 10)
#' @importFrom dplyr slice_head slice_tail slice_min slice_max slice_sample
#' @rdname slice
#' @export
slice_head.cubble_df <- slice_factory("slice_head", group = FALSE)

#' @rdname slice
#' @export
slice_tail.cubble_df <- slice_factory("slice_tail")

#' @rdname slice
#' @export
slice_min.cubble_df <- slice_factory("slice_min")

#' @rdname slice
#' @export
slice_max.cubble_df <- slice_factory("slice_max")

#' @rdname slice
#' @export
slice_sample.cubble_df <- slice_factory("slice_sample")

#' @param data the data
#' @param ... conditions to filter for the selected data
#' @param buffer the buffer added to the bounding box for slicing
#' @param n the number of nearby points to sample
#' @rdname slice
#' @export
slice_nearby <- function(data, ..., buffer = 1, n = 5){

  if (form(data) != "nested") {
    cli::cli_abort("slicing should be performed in the nested form on the grouping variable")
  }

  dots <- enquos(...)

  idx <- map(dots, ~ eval_tidy(.x, data))
  target <- vec_slice(data, Reduce("&", idx))
  target <- dplyr_reconstruct(target, data)

  if (nrow(target) == 0) {
    cli::cli_abort(
      "No candidate fulfill the condition(s),
          try {.code data %>% filter(...)} on the condition to diagnose"
    )
  }

  lat_min <-  min(target$lat) - buffer
  lat_max <- max(target$lat) + buffer
  long_min <-  min(target$long) - buffer
  long_max <-  max(target$long) + buffer

  target_var <- key_data(target) %>% dplyr::pull(!!key_vars(data))

  cand <-
    data %>% filter(
      dplyr::between(.data$lat, lat_min, lat_max),
      dplyr::between(.data$long, long_min, long_max),
      !(!!sym(key_vars(data)) %in% target_var)
    )

  cli::cli_inform("The bounding box gives {.code nrow(cand)} candidates")

  if (n > nrow(cand)) {
    cli::cli_inform("Number of element to sample is larger than the candidate.
           All candidates are selected")
  }

  out <- rbind(
    target %>% mutate(type = "selected"),
    cand %>% slice_sample(n = n) %>% mutate(type = "sampled")
  )
  out <- dplyr_reconstruct(out, data)
  attr(out, "bbox") <- c(long_min, long_max, lat_min, lat_max)

  out


}
