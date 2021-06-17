#' Data structure
#' @examples
#' global <- global(oz_climate, station)
#' zoom <- global %>% zoom(data)
#' @export
#' @rdname data-structure
global <- function(data, key){
  UseMethod("global")

}

find_nest_var <- function(data, key){
  key <- enquo(key)
  # temporarily only one key
  non_varying_var <- find_non_varying_var(data, !!key)
  col_names <- names2(data)
  nest_var <- col_names[!col_names %in% non_varying_var]

  list(nest_var= nest_var, non_varying_var = non_varying_var)
}

#' @export
global.cubble_df <- function(data, key){
  key <- enquo(key)
  nest_var <- find_nest_var(data, !!key)
  meta_data <- data %@% meta
  out <- as_tibble(data) %>%
    dplyr::left_join(as_tibble(meta_data)) %>%
    tidyr::nest(!!!nest_var$nest_var) %>%
    dplyr::rowwise()

  cubble_df(out, group_vars = as_name(key), meta_data = meta_data, format = "wide")
}

#' @export
global.tbl_df <- function(data, key){
  key <- enquo(key)
  nest_var <- find_nest_var(data, !!key)

  out <- data %>%
    tidyr::nest(!!!nest_var$nest_var) %>%
    dplyr::rowwise()
  meta_data <- out[nest_var$non_varying_var]
  cubble_df(out, group_vars = as_name(key), meta_data = meta_data, format = "wide")
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


  new_tibble(data, groups = group_data, meta = meta_data, format = format, class = c("cubble_df", "rowwise_df"))
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

#' @export
dplyr_col_modify.cubble_df <- function(data, cols){

  out <- dplyr_col_modify(as_tibble(data), cols)
  group_vars <- names2(data %@% groups)[1]
  meta_data <- data %@% meta
  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "wide")
}

#' @export
dplyr_row_slice.cubble_df <- function(data, i, ...){

  out <- vec_slice(data, i)
  group_vars <- names2(data %@% groups)[1]
  meta_data <- data %@% meta
  cubble_df(out, group_vars = group_vars, meta_data = meta_data, format = "long")
}

#' @export
dplyr_reconstruct.cubble_df <- function(data, template){

  group_vars <-  names2(data %@% groups)[1]
  meta_data <- data %@% meta

  cubble_df(data, group_vars = group_vars, meta_data = meta_data, format = "long")
}

