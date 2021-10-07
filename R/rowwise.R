#' Remove the rowwise grouping of a cubble
#'
#' @param data a cubble object
#' @examples
#' library(dplyr)
#' # row number is not properly added since each row is a separate group
#' aus_climate %>% mutate(.id = row_number())
#'
#' # proper id after removing the grouping structure
#' aus_climate %>% strip_rowwise() %>% mutate(.id = row_number())
#'
#' @export
strip_rowwise <- function(data){

  test_cubble(data)
  attr(data, "groups") <- attr(data, "groups")[NULL, ]
  attr(data, "class") <- attr(data, "class")[attr(data, "class") != "rowwise_df"]

  data
}
