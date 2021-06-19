#' Data structure
#' @param data the data to be converted into a cubble object
#' @param key the spatio identifier
#'
#' @examples
#' oz_global <- global(oz_climate, station)
#' oz_zoom <- oz_global %>% zoom(data)
#' back <- oz_zoom %>% global(station)
#' @export
#' @rdname data-structure
global <- function(data, key) {
  UseMethod("global")
}

#' @export
global.cubble_df <- function(data, key) {
  key <- enquo(key)
  nest_var <- find_nest_var(data, !!key)
  meta_data <- meta(data)
  out <- as_tibble(data) %>%
    dplyr::left_join(as_tibble(meta_data)) %>%
    tidyr::nest(!!!nest_var$nest_var) %>%
    dplyr::rowwise()

  cubble_df(out, group_vars = as_name(key), meta_data = meta_data, format = "wide")
}

#' @export
global.tbl_df <- function(data, key) {
  key <- enquo(key)
  nest_var <- find_nest_var(data, !!key)

  out <- data %>%
    tidyr::nest(!!!nest_var$nest_var) %>%
    dplyr::rowwise()
  meta_data <- out[nest_var$non_varying_var]
  cubble_df(out, group_vars = as_name(key), meta_data = meta_data, format = "wide")
}

#' @export
cubble_df <- function(data, group_vars, meta_data,  format) {
  new_cubble_df(data, group_vars, meta_data, format = format)

}

new_cubble_df <- function(data, group_vars, meta_data, format, others = NULL) {

  if (format == "wide") {
    nrow <- nrow(data)
    group_data <- as_tibble(data)[group_vars]
    group_data <- new_tibble(dplyr:::dplyr_vec_data(group_data), nrow = nrow)
    group_data$.rows <- new_list_of(as.list(seq_len(nrow)), ptype = integer())
  } else if (format == "long") {
    nrow <- nrow(meta_data)
    group_data <- dplyr:::compute_groups(data, as_name(group_vars))
  }

  if ("tbl_ts" %in% class(data)){
    attr <- attributes(data)
    attr_to_add <- attr[!names(attr) %in% c("names", "row.names", "class")]

    class <- c("cubble_df", "rowwise_df", class(data))

    attr_basics <- list(x = data,
                        groups = group_data,
                        meta = meta_data,
                        format = format,
                        class = class)

    attr_all <- c(attr_basics, attr_to_add)

    rlang::exec("new_tibble", !!!attr_all)

  } else{
    new_tibble(data, groups = group_data, meta = meta_data, format = format, class = c("cubble_df", "rowwise_df"))
  }

}


#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {
  c(
    NextMethod(),
    "Cubble" = format(data)
  )
}


#' @param data the data to zoom
#' @param col the list-column in the data to zoom into
#' @export
#' @rdname data-structure
zoom <- function(data, col) {
  UseMethod("zoom")
}

#' @export
zoom.cubble_df <- function(data, col){

  test_cubble(data)
  col <- enquo(col)

  if (!is_list(eval_tidy(col, data))) {
    abort("The column to zoom need to be a list-column")
  }

  group_var <- sym(group_vars(data))
  meta_data <- meta(data)

  list_col <- data %>% dplyr::pull(!!col)

  if ("tbl_ts" %in% class(list_col[[1]])){
    data$data <- map(data$data, as_tibble)
    out <- data[vec_c(as_name(group_var), as_name(col))]
    out <- out %>%
      tidyr::unnest(!!col) %>%
      tsibble::as_tsibble(key = !!group_var)
  } else{
    out <- data %>%
      dplyr::select(!!group_var, !!col) %>%
      tidyr::unnest(!!col)
  }

  cubble_df(out, group_vars = group_var, meta_data = meta_data, format = "long")
}


#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}


#' get attributes for a cubble object
#'
#' @param data an cubble object
#'
#' @export
#' @rdname attributes
format <- function(data){
  test_cubble(data)
  data %@% format
}

#' @export
#' @rdname attributes
meta <- function(data){
  test_cubble(data)
  data %@% meta
}

#' @export
#' @rdname attributes
group_vars <- function(data){
  test_cubble(data)
  groups <- data %@% groups
  names <- names2(groups)
  names[names != ".rows"]
}


