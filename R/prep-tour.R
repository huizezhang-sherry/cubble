#' A function to prepare edges data for tour display
#'
#' @param data a cubble object
#' @param edges_col the variable maps to edges colour
#' @param color_col the variable maps to point colour
#' @param cols the numerical column selected for a tour
#' @rdname prep-tour
#' @export
#' @return a list of edge linkage, edge color, and point color
prep_edges <- function(data, edges_col, color_col){
  UseMethod("prep_edges")
}

#' @rdname prep-tour
#' @export
prep_edges.cubble_df <- function(data, edges_col, color_col = NULL){

  test_cubble(data)
  if (is_nested(data)) data <- data %>%  face_temporal()
  id <- key_vars(data)
  edges_col <- enquo(edges_col)
  col_col <- enquo(color_col)

  if (quo_name(edges_col) %in% colnames(spatial(data))){
    data <- data %>%  unfold(!!edges_col)
  }
  if (quo_name(col_col) %in% colnames(spatial(data))){
    data <- data %>%  unfold(!!col_col)
  }

  raw <-  data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      from = dplyr::row_number(),
      to = dplyr::lead(.data$from),
      id_lead = dplyr::lead(.data$id),
      to = ifelse(id == .data$id_lead, .data$to, NA)) %>%
    dplyr::filter(!is.na(.data$to))

  edges <- raw %>%
    dplyr::select(.data$from, .data$to) %>%
    as.matrix()

  edges_col <- find_col(raw, !!edges_col)

  if (quo_is_null(col_col)){
    color_col <-  NULL
  } else{
    color_col <- find_col(raw, !!col_col)
  }

  list(edges = edges,
       edges_col = edges_col,
       color_col = color_col)

}

find_col <- function(data, col){
  col <- enquo(col)
  col_name <- quo_name(col)
  if (col_name %in% colnames(data)){
    res <- data %>%  dplyr::pull(!!col)
  } else{
    cli::cli_abort("Column {.code col} is not in the data, please check.")
  }

  res
}


#' @rdname prep-tour
#' @export
prep_data <- function(data, cols){
  UseMethod("prep_data")
}

#' @rdname prep-tour
#' @export
prep_data.cubble_df <- function(data, cols = NULL){
  cols <- enquo(cols)
  test_cubble(data)
  key <- key_vars(data)
  if (quo_is_null(cols)){
    cols <- map_chr(data, class) == "numeric"
  }

  data %>%  tibble::as_tibble() %>%  select(!!cols) %>%  as.matrix()
}
