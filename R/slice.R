# helper
slice_factory <- function(f, ...){
  function(data, ...){
    if (form(data) != "list-col"){
      abort("slicing should be performed in the list-col form on the grouping variable")
    }

    group_vars <- group_vars(data)

    data <- tibble::as_tibble(data)
    out <- NextMethod(f)

    cubble_df(out, group = group_vars, meta_data = out, form = determine_form(out))
  }
}

#' Slicing a cubble
#' @param data a cubble object to slice
#' @param ... other arguments passed to the [dplyr::slice()]
#' @examples
#' \dontrun{
#' library(cubble)
#' # create a cubble object
#' all <- climate %>%
#'          dplyr::left_join(station, by = c("station" = "id")) %>%
#'          global(station)
#'
#' # slice the first 50 stations from the top/ bottom
#' all %>% slice_head(n = 50)
#' all %>% slice_tail(n = 50)
#'
#' # slice based on the max/ min of a variable
#' all %>% slice_max(elev, n = 10)
#' all %>% slice_min(lat, n = 10)
#'
#' # random sample
#' all %>% slice_sample(n = 10)
#' }
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

