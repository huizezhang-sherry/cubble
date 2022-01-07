#' Title
#'
#' find the convex hull that wraps around the cluster, make it a polygon, find the centroid of the polygon and finally, extract the x and y coordinate of each centroid:
#' @param data a cubble data object
#'
#' @return
#' @importFrom geosphere centroid
#' @export
#'
get_centroid <- function(data){
  test_cubble(data)

  if (form(data) != "nested"){
    abort("Require nested form of a cubble to calculate centroid")
  }

  if (!".val" %in% colnames(data)){
    abort("Something is wrong - there should be a .val column")
  }

  key <- key_vars(data)
  coords <- coords(data)

  data %>%
    mutate(chull = list(chull(.val[[coords[1]]],
                              .val[[coords[2]]])),
           hull = list(.val[chull,]),
           cent = list(geosphere::centroid(.val[chull,coords])),
           cent_long = as.numeric(cent[,1]),
           cent_lat = as.numeric(cent[,2])) %>%
    select(-chull, -cent)
}


