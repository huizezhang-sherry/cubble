find_non_varying_var <- function(data, key) {
  key <- enexpr(key)
  data <- tibble::as_tibble(data)
  # remove the list-column, useful in a nested form
  data[map(data, class) == "list"] <- NULL

  list_col <- data %>%
    tidyr::nest(data = -(!!key)) %>%
    dplyr::pull(data)

  out <- map(list_col, function(data){
    var_length <- map_dbl(colnames(data), ~ nrow(unique(data[.x])))
    all_na <- map_lgl(map(data, is.na), all)
    c(as_name(key), colnames(data)[var_length == 1 & !all_na])
  })

  out <- Reduce(intersect, out)
  names(out) <- NULL

  out
}

find_nest_var <- function(data, key) {
  key <- enquo(key)
  data <- tibble::as_tibble(data)

  # temporarily only one key
  non_varying_var <- find_non_varying_var(data, !!key)
  col_names <- names2(data)
  nest_var <- col_names[!col_names %in% non_varying_var]

  list(nest_var = nest_var, non_varying_var = non_varying_var)
}
