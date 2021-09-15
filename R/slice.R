# helper
slice_factory <- function(f, ...){
  function(data, ...){
    if (form(data) != "nested"){
      # think about slicing on the long form
      abort("slicing should be performed in the nested form on the grouping variable")
    }

    data <- tibble::as_tibble(data)
    out <- NextMethod(f)

    new_cubble(out, key = key_vars(data), leaves = new_leaves(out, !!key_vars), form = determine_form(out))
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

#' @param data the data
#' @param ... conditions to filter for the selected data
#' @param buffer the buffer added to the bounding box for slicing
#' @param n the number of nearby points to sample
#' @rdname nearby
#' @export
slice_nearby <- function(data, ..., buffer = 1, n = 5){

  if (form(data) != "nested") {
    abort("slicing should be performed in the nested form on the grouping variable")
  }

  dots <- enquos(...)

  idx <- map(dots, ~ eval_tidy(.x, data))
  target <- vec_slice(data, Reduce("&", idx))
  target <- dplyr_reconstruct(target, data)

  if (nrow(target) == 0) {
    abort(
      "No candidate fulfill the condition(s),
          try `data %>% filter(...) on the condition to diagnose`"
    )
  }

  lat_min <-  min(target$lat) - buffer
  lat_max <- max(target$lat) + buffer
  long_min <-  min(target$long) - buffer
  long_max <-  max(target$long) + buffer

  target_var <- groups(target) %>% dplyr::pull(!!key_vars(data))

  cand <-
    data %>% filter(
      dplyr::between(.data$lat, lat_min, lat_max),
      dplyr::between(.data$long, long_min, long_max),
      !(!!sym(key_vars(data)) %in% target_var)
    )

  inform(glue::glue("The bounding box gives {nrow(cand)} candidates"))

  if (n > nrow(cand)) {
    inform("Number of element to sample is larger than the candidate.
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

#' Autoplot for viewing the nearby points sampled
#' @param data a data object output from \code{slice_nearby()}
#' @param map a map object as the background, default to the Australia country map
#' @param origin the original dataset supplied to \code{slice_nearby()}
#' @examples
#' slice_nearby(climate_large, name == "Melbourne Airport") %>%
#'    view_nearby(origin = climate_large,
#'                map = ozmaps::abs_ste %>% dplyr::filter(NAME == "Victoria"))
#' @export
#' @rdname nearby
view_nearby <- function(data, origin, map = NULL) {
  if (!"type" %in% colnames(data)) {
    abort("Data needs to have the `type` column outputted from `slice_nearby`!")
  }

  if (is.null(map)) {
    map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
  }

  bbx <- attr(data, "bbox")
  all <- origin %>%
    filter(dplyr::between(.data$lat, bbx[3], bbx[4]),
           dplyr::between(.data$long, bbx[[1]], bbx[[2]]))

  plot_map(map_data = map) +
    ggplot2::geom_point(data = all,
                        ggplot2::aes(x = .data$long, y = .data$lat),
                        color = "grey") +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = .data$long, y = .data$lat,
                                     color = .data$type)) +
    ggplot2::geom_rect(data = data,
                       ggplot2::aes(xmin = bbx[1], xmax = bbx[2],
                                    ymin = bbx[3], ymax = bbx[4]),
                       linetype = "dashed", fill = "transparent", color = "black") +
    ggplot2::scale_color_brewer(palette = "Dark2")

}
