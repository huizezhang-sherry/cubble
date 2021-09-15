#' Generate geofacet grid based on long and lat
#'
#' This function automatically generates a grid for \code{\link[geofacet]{facet_geo}}
#' based on longitude and latitude.
#' @param data a cubble object
#' @param var the facet variable
#' @param nrow number of row in the facet
#' @param ncol number of column in the facet
#'
#' @examples
#' library(lubridate)
#' library(cubble)
#' library(ggplot2)
#' library(geofacet)
#' library(dplyr)
#' short <- climate_small %>% dplyr::slice_tail(n = 15)
#'
#' # generate the grid
#' grid1 <- short %>% gen_grid(var = name)
#'
#' make_plot <- function(grid_supplied){
#'   short %>%
#'     stretch() %>%
#'     migrate(name) %>%
#'     dplyr::filter(year(date) == 2020, month(date) == 1) %>%
#'     ggplot(aes(x = date, y= tmax, group = name)) +
#'     geom_line() +
#'     facet_geo(vars(name), grid = grid_supplied)
#'  }
#'
#' # make the plot with grid1
#' make_plot(grid1)
#'
#' # manually modify the position of hobart airport in the grid
#' grid2 <- grid1 %>%
#'   mutate(row = ifelse(name == "hobart airport", 8, row),
#'          col = ifelse(name == "hobart airport", 3, col))
#'
#' # make the same plot based on grid2
#' make_plot(grid2)
#'
#' @importFrom stringr str_sub
#' @importFrom dplyr row_number between
#' @export
gen_grid <- function(data, var = NULL, nrow = NULL, ncol = NULL) {

  var <- enquo(var)
  if (quo_is_null(var)) var <- sym(key_vars(data))

  latlong <- data %>%
    as_tibble() %>%
    select(!!var, .data$lat, .data$long) %>%
    dplyr::distinct()

  n_enlarge <- nrow(data) * 3
  if (is.null(nrow) & is.null(ncol)){
    nrow <- ceiling(sqrt(n_enlarge))
    ncol <-  (n_enlarge %/% nrow)
  }  else if (is.null(nrow) & !is.null(ncol)){
    nrow <- (n_enlarge %/% ncol )
  } else if (!is.null(nrow) & is.null(ncol)){
    ncol <- (n_enlarge %/% nrow )
  }

  grid_raw <- latlong %>%
    mutate(
      row_rank = rank(.data$long, ties.method = "first"),
      col_rank = rank(.data$lat,  ties.method = "first"),
      # the row basket depends on how many column there should be
      row_bsk = as.numeric(cut(.data$long, breaks = ncol, include.lowest = TRUE,
                               ordered_result = TRUE)),
      col_bsk = as.numeric(cut(.data$lat, breaks = nrow, include.lowest = TRUE,
                               ordered_result = TRUE))
      )

  check_dup <- grid_raw %>%
    mutate(rowcol = paste0(.data$row_bsk, .data$col_bsk),
           check_dup = duplicated(.data$rowcol))

  i <- 1
  while (i <= nrow(check_dup)) {
    if (check_dup$check_dup[i]) {
      prob <- check_dup %>% filter(row_number() == i)
      other <- check_dup %>% filter(.data$rowcol %in% prob$rowcol, !check_dup)

      row <- check_dup$row_bsk[i]
      col <- check_dup$col_bsk[i]
      rowcol_cand <- expand.grid(r = c(row - 1, row, row + 1),
                                 c = c(col - 1, col, col + 1)) %>%
        filter(between(.data$r, 1, nrow), between(.data$c, 1, ncol)) %>%
        mutate(cand = paste0(.data$r, .data$c),
               already = .data$cand %in% check_dup$rowcol) %>%
        filter(!.data$already)

      if (prob$row_rank > other$row_rank) {
        rowcol_cand <- rowcol_cand %>% filter(.data$r >= row)
      } else{
        rowcol_cand <- rowcol_cand %>% filter(.data$r <= row)
      }

      if (prob$col_rank > other$col_rank) {
        rowcol_cand <- rowcol_cand %>% filter(.data$c >= col)
      } else{
        rowcol_cand <- rowcol_cand %>% filter(.data$c <= col)
      }

      check_dup <- check_dup %>%
        mutate(
          rowcol = ifelse(row_number() == i, utils::tail(rowcol_cand$cand, 1), .data$rowcol),
          check_dup = ifelse(row_number() == i, FALSE, check_dup)
        )

    }

    i <- i + 1
  }

  out <- check_dup %>%
    mutate(col = as.numeric(stringr::str_sub(.data$rowcol, 1, 1)),
           row = (nrow + 1) - as.numeric(stringr::str_sub(.data$rowcol, 2, 2))) %>%
    mutate(code = !!var, name = !!var) %>%
    select(.data$code, .data$name, .data$row, .data$col)


  if (any(is.na(out$row)) | any(is.na(out$col))){
    warning("The grid dimension is not sufficient for the data, increase `nrow` or `ncol`.")
  }
  out
}
