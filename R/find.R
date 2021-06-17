find_non_varying_var <- function(data, key) {
  key <- enexpr(key)

  subset <- data %>%
    tidyr::nest(data = -(!!key)) %>%
    dplyr::pull(data) %>%
    .[[1]]
  var_length <- map_dbl(colnames(subset), ~ nrow(unique(subset[.x])))

  out <- c(as_name(key), colnames(subset)[var_length == 1])
  names(out) <- NULL

  out
}

find_nest_var <- function(data, key) {
  key <- enquo(key)
  # temporarily only one key
  non_varying_var <- find_non_varying_var(data, !!key)
  col_names <- names2(data)
  nest_var <- col_names[!col_names %in% non_varying_var]

  list(nest_var = nest_var, non_varying_var = non_varying_var)
}
