find_invariant <- function(data, key) {
  key <- as_name(enquo(key))
  data <- tibble::as_tibble(data)

  # temporarily only one key
  # remove the list-column, useful in a nested form
  data[map(data, class) == "list"] <- NULL

  key_col <- data[,key]
  nested_col <- data[,which(names(data) != key)]
  list_col <- vec_split(nested_col, key_col)$val


  out <- map(list_col, function(data){
    var_length <- map_dbl(colnames(data), ~ nrow(unique(data[.x])))
    c(key, colnames(data)[var_length == 1])
  })

  invariant <- Reduce(intersect, out)
  names(invariant) <- NULL

  col_names <- names2(data)
  variant <- col_names[!col_names %in% invariant]

  list(variant = variant, invariant = invariant)
}
