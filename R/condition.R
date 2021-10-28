test_cubble <- function(data) {
  if (!is_cubble(data)) abort("data supplied needs to be a cubble object!")
}

test_leaves <- function(data) {
  if (!is_leaves(data)) abort("data supplied needs to be leaves of a cubble!")
}

test_long <- function(data){
  test_cubble(data)
  if (form(data) != "long") abort("data is not in the long form")
}

test_nested <- function(data){
  test_cubble(data)
  if (form(data) != "nested") abort("data is not in the nested form")
}

test_missing <- function(quo, var){
  if (quo_is_missing(quo)){
    abort(glue::glue("Variable {var} is missing for creating a cubble"))
  }
}

get_listcol <- function(data){
  col_type <- map(data, class)
  names(data)[col_type == "list"]
}

test_list <- function(data, col){
  col <- enquo(col)
  idx <- tidyselect::eval_select(col, data)
  class_vec <- map_chr(as_tibble(data), class)
  res <- class_vec[idx]
  if (res != "list") {
    abort("The column to stretch need to be a list-column")
  }

}
