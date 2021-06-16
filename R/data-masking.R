#' Data structure
#' @examples
#' global <- global(oz_climate, station)
#' zoom <- global %>% zoom(data)
#' @export
#' @rdname data-structure
global <- function(data, key){

  key <- enquo(key)
  # temporarily only one key
  non_varying_var <- find_non_varying_var(data, !!key)
  col_names <- names2(data)
  nest_var <- col_names[!col_names %in% non_varying_var]

  list_col <- data %>%
    tidyr::nest(!!!nest_var) %>%
    dplyr::rowwise()
  meta_data <- list_col[non_varying_var]


  cubble_df(list_col, group_vars = as_name(key), meta_data = meta_data, format = "wide")

}

cubble_df <- function(data, group_vars, meta_data, format){

  new_cubble_df(data, group_vars, meta_data, format = format)
}


new_cubble_df <- function(data, group_vars, meta_data, format){

  if (format == "wide"){
    nrow <- nrow(data)
    group_data <- as_tibble(data)[group_vars]
    group_data <- new_tibble(dplyr:::dplyr_vec_data(group_data), nrow = nrow)
    group_data$.rows <- new_list_of(as.list(seq_len(nrow)), ptype = integer())
  } else if (format == "long"){
    nrow <- nrow(meta_data)
    group_data <- dplyr:::compute_groups(data, as_name(group_vars))
  }


  new_tibble(data, groups = group_data, meta = meta_data, format = format, class = "cubble_df")
}

find_non_varying_var <- function(data, key){
  key <- enexpr(key)

  subset <- data %>% tidyr::nest(data = -(!!key)) %>% dplyr::pull(data) %>% .[[1]]
  var_length <- map_dbl(colnames(subset), ~nrow(unique(subset[.x])))

  out <- c(as_name(key), colnames(subset)[var_length == 1])
  names(out) <- NULL

  out

}

#' @export
#' @rdname data-structure
zoom <- function(dt, col){

  # check from a global_df
  col <- enquo(col)

  if (!is_list(eval_tidy(col, dt))){
    abort("The column to zoom need to be a list-column")
  }

  group_var <- sym(names(dt %@% groups)[1])
  meta_data <- dt %@% meta

  out <- dt %>%
    dplyr::select(group_var, !!col) %>%
    tidyr::unnest(!!col)

  nrow <- nrow(dt)

  cubble_df(out, group_vars = group_var, meta_data = meta_data, format = "long")
}


dplyr_col_modify.cubble_df <- function(data, cols){


}


dplyr_reconstruct.cubble_df <- function(data, template){

}
