#' Remove the rowwise grouping of a cubble
#'
#' @param data a cubble object
#' @examples
#' library(dplyr)
#' # row number is not properly added since each row is a separate group
#' climate_aus |> mutate(.id = row_number())
#'
#' # proper id after removing the grouping structure
#' climate_aus |> strip_rowwise() |> mutate(.id = row_number())
#' @return a cubble object
#' @export
strip_rowwise <- function(data){

  test_cubble(data)
  attr(data, "groups") <- attr(data, "groups")[NULL, ]
  attr(data, "class") <- attr(data, "class")[attr(data, "class") != "rowwise_df"]

  data
}


find_invariant <- function(data, key) {
  key <- as_name(enquo(key))
  data <- tibble::as_tibble(data)

  if (length(key) != 1){
    cli::cli_abort("Only one key for {.fn find_invariant}.")
  }

  # remove the list-column, useful in a nested form
  data[map(data, class) == "list"] <- NULL

  key_col <- data[,key]
  nested_col <- data[,which(names(data) != key)]
  list_col <- vec_split(nested_col, key_col)$val

  if (length(list_col) > 10000){
    list_col <- list_col[1]
    cli::cli_alert_info(
      "More than 10,000 keys: only use the first key to test spatial & temporal variables."
      )
  }

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


get_listcol <- function(data){

  out <- names(data)[which(names(data) == "ts")]
  if (inherits(data[[out]], "list")) return(out)

  # if the list column is not named ts, detect the list-column
  out <- names(data)[map(data, class) == "list"]
  if (length(out) == 1){
    return(out)
  }else{
    cli::cli_abort("Can't determine the list column, please specify it")
  }
}


key_level <- function(data, key){
   map_dbl(key, ~unique(data[[.x]]) |> length())
}

tsibble_from_cubble <- function(data){
  test_cubble(data)
  long <- is_long(data) && "tbl_ts" %in% class(data)
  nested <- is_nested(data) && "tbl_ts" %in% unlist(map(data$ts[1], class))

  long | nested
}


all_cubble_cols <- function(data){
  test_cubble(data)

  if (is_long(data)) {
    unique(c(names(spatial(data)), names(data)))
    } else if (is_nested(data)){
    unique(c(names(data), names(data[["ts"]][[1]])))
  }
}


