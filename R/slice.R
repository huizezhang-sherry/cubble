# helper
slice_factory <- function(f, ...){
  function(data, ...){
    if (form(data) != "nested"){
      # think about slicing on the long form
      abort("slicing should be performed in the nested form on the grouping variable")
    }

    group_vars <- group_vars(data)

    data <- tibble::as_tibble(data)
    out <- NextMethod(f)

    cubble_df(out, group = group_vars, leaves = out, form = determine_form(out))
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
#' library(dplyr)
#' # slice the first 50 stations from the top/ bottom
#' climate_small %>% slice_head(n = 50)
#' climate_small %>% slice_tail(n = 50)
#'
#' # slice based on the max/ min of a variable
#' climate_small %>% slice_max(elevation, n = 10)
#' climate_small %>% slice_min(lat, n = 10)
#'
#' # random sample
#' climate_small %>% slice_sample(n = 10)
#' @importFrom dplyr slice_head slice_tail slice_min slice_max slice_sample
#' @rdname slice
#' @export
slice_head.cubble_df <- slice_factory("slice_head")

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

