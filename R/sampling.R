#' Sample group variables for cubble
#'
#' @param data a cubble object to sample
#' @param size the number to sample, see \link[base]{sample}
#' @param ... other argument passed to \link[base]{sample}
#' @examples
#' \dontrun{
#' set.seed(123)
#' out <- oz_zoom2 %>% sample_group_var(size = 3)
#' out %>% global(station)
#' meta(out)
#' }
#' @export
sample_group_var <- function(data, size = 5, ...){

  test_cubble(data)

  meta <- meta(data)
  group_var <- group_vars(data)
  var_sampled <- sample(meta[[group_var]], size = size, ...)

  data %>% dplyr::filter(!!sym(group_var) %in% var_sampled)


}
