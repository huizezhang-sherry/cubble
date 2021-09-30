#' Spatial join of two cubbles
#'
#' An option to perform spatial join is to use \code{sf::st_join()}, however, this
#' requires 1) a data frame to be converted to an sf object based on long and lat,
#' 2) use \code{sf::st_join()} for joining, and 3) convert the object back to a data
#' frame and drop the geometry for a sole operation. \code{ll_join()} uses package s2
#' to create a lnglat vector and perform the join directly on a cubble without
#' converting between object.
#'
ll_join <- function(data1, data2){

  # test data1 data2
  # extracting ll column for both data

  data2 <- data2 %>%
    mutate(.i = dplyr::row_number())

  data1 <- data1 %>%
    mutate(ll = s2::s2_lnglat(long, lat),
           .i = default_which(s2::s2_intersects(ll, data2$geometry)))

  data1 %>%
    left_join(data2) %>%
    select(-c(.i, geometry, ll))


}


default_which <- function(x, default = na_int){
  out <- which(x)
  if (length(out) == 0){
    out <- default
  }
  out
}

